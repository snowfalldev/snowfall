const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const Script = @import("Script.zig");

const ast = @import("ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;

const unicode = @import("unicode.zig");

const tty = std.io.tty;
const Color = tty.Color;

// INTERNAL LOGGING HELPERS

// scope our logs so they work well as a library
pub const scoped = std.log.scoped(.snowfall);

var config: tty.Config = undefined;
var get_config_once = std.once(get_config);
fn get_config() void {
    config = tty.detectConfig(std.io.getStdErr());
}

pub fn customLogFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (!std.log.logEnabled(level, scope)) return;

    get_config_once.call();
    const color: Color = switch (level) {
        .err => .red,
        .warn => .yellow,
        .info => .green,
        .debug => .cyan,
    };

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    // we use .snowfall as our log scope throughout the library code, treat that the same as .default
    const prefix = if (scope == .snowfall or scope == .default) "" else @tagName(scope) ++ ": ";

    nosuspend {
        config.setColor(writer, .reset) catch return;
        config.setColor(writer, color) catch return;
        writer.writeAll(comptime level.asText() ++ ": ") catch return;
        config.setColor(writer, .reset) catch return;
        writer.print(prefix ++ format, args) catch return;
        bw.flush() catch return;
    }
}

// LOGGER STRUCTURE

// std.log.Level with fatal added.
pub const Level = enum { fatal, err, warn, info, debug };

pub fn ToolsT(comptime Message: type, comptime Error: type) type {
    return struct {
        print: fn (*const Message, anytype) anyerror!void,
        level: ?fn (*const Message) Level,
        fatality: ?fn (*const Message) Error!void,
        printData: ?fn (*const Message) bool,
    };
}

fn MsgContainer(
    comptime Message: type,
    comptime FatalErrors: type,
    comptime tools: ToolsT(Message, FatalErrors),
) type {
    return struct {
        script: *Script,
        msg: Message,
        span: Span,
        hi: ?Pos,

        const Self = @This();

        pub fn format(
            self: *const Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try tools.print(&self.msg, writer); // print message

            // print script name & position
            try writer.print(" ({s}:{}:{})\n", .{
                self.script.name,
                self.span[0].row + 1,
                self.span[0].col + 1,
            });

            if (!self.printData()) return;

            // print affected line
            const src = self.script.src;
            const row = self.script.rows.items[self.span[0].row];
            var end: usize = row.raw + self.span[1].col + 1;
            while (end < src.len and !unicode.isNewLine(src[end])) end += 1;
            try writer.writeAll(src[row.raw..end]);
            try writer.writeByte('\n');

            // highlight affected section
            try writer.writeByteNTimes(' ', self.span[0].col);
            if (self.span[1].col >= self.span[0].col) {
                var rem = self.span[1].col - self.span[0].col;

                if (self.hi) |hi| {
                    const offset = hi.col - self.span[0].col;
                    try writer.writeByteNTimes('~', offset);
                    rem -= offset;
                }

                try writer.writeByte('^');
                try writer.writeByteNTimes('~', rem);
            }
            try writer.writeByte('\n');
        }

        pub inline fn level(self: Self) Level {
            return if (tools.level) |f| f(&self.msg) else Level.err;
        }

        pub inline fn fatality(self: Self) FatalErrors!void {
            if (tools.fatality) |f| return f(&self.msg);
        }

        pub inline fn printData(self: Self) bool {
            return if (tools.printData) |f| f(&self.msg) else true;
        }
    };
}

pub fn Logger(
    comptime Message: type,
    comptime FatalErrors: type,
    comptime tools: ToolsT(Message, FatalErrors),
) type {
    return struct {
        script: *Script,
        errors: std.ArrayListUnmanaged(Container) = .{},
        others: std.ArrayListUnmanaged(Container) = .{},

        const Self = @This();

        pub const Container = MsgContainer(Message, FatalErrors, tools);
        pub const Error = Allocator.Error || FatalErrors;

        pub inline fn deinit(self: Self) void {
            self.errors.deinit(self.script.allocator());
            self.others.deinit(self.script.allocator());
        }

        pub fn log(self: *Self, msg: Message, span: Span, hi: ?Pos) Error!void {
            if (self.script.failed) return;

            const c = Container{ .script = self.script, .msg = msg, .span = span, .hi = hi };
            const level = c.level();

            if (level == .fatal) self.script.failed = true; // Fatal Fails Fast

            try switch (level) {
                .fatal, .err => self.logInner(.err, c),
                .warn => self.logInner(.warn, c),
                .info => self.logInner(.info, c),
                .debug => self.logInner(.debug, c),
            };
        }

        inline fn logInner(self: *Self, comptime level: std.log.Level, msg: Container) Error!void {
            try switch (level) { // store message
                .err => self.errors.append(self.script.allocator(), msg),
                else => self.others.append(self.script.allocator(), msg),
            };

            // store lower level logs but don't print
            if (!std.log.logEnabled(level, .snowfall)) return;

            switch (level) { // print message
                .err => scoped.err("{}", .{msg}),
                .warn => scoped.warn("{}", .{msg}),
                .info => scoped.info("{}", .{msg}),
                .debug => scoped.debug("{}", .{msg}),
            }

            try msg.fatality();
        }

        pub inline fn failedEarly(self: Self) bool {
            if (self.errors.items.len == 0) return false;
            return self.errors.items[self.errors.items.len - 1].level() == .fatal;
        }
    };
}

// SIMPLE MESSAGES

const Type = std.builtin.Type;

pub const SimpleMessageInfo = struct { Level, @Type(.enum_literal), []const u8, type };

fn SimpleMessageT(comptime messages: []const SimpleMessageInfo) type {
    // make enum type

    comptime var enum_fields: [messages.len]Type.EnumField = undefined;
    for (messages, 0..) |msg, i|
        enum_fields[i] = .{
            .name = @tagName(msg.@"1"),
            .value = i,
        };

    const enum_info = Type.Enum{
        .tag_type = u16,
        .fields = &enum_fields,
        .decls = &.{},
        .is_exhaustive = true,
    };
    const EnumT = @Type(.{ .@"enum" = enum_info });

    // make union type

    comptime var union_fields: [messages.len]Type.UnionField = undefined;
    for (messages, 0..) |msg, i|
        union_fields[i] = .{
            .name = @tagName(msg.@"1"),
            .type = msg.@"3",
            .alignment = 0,
        };

    const union_info = Type.Union{
        .layout = .auto,
        .tag_type = EnumT,
        .fields = &union_fields,
        .decls = &.{},
    };
    return @Type(.{ .@"union" = union_info });
}

fn pascalify(comptime string: []const u8) []const u8 {
    comptime var output: []const u8 = "";
    comptime var split = std.mem.splitScalar(u8, string, '_');

    inline while (split.next()) |chunk| {
        var capitalized: [chunk.len]u8 = chunk[0..].*;
        capitalized[0] = switch (chunk[0]) {
            'a'...'z' => |c| c - ('a' - 'A'),
            else => |c| c,
        };
        output = output ++ capitalized;
    }

    return output;
}

fn SimpleErrorT(comptime messages: []const SimpleMessageInfo) type {
    comptime var error_count: usize = 0;
    for (messages) |msg| {
        if (msg.@"0" == .fatal)
            error_count += 1;
    }

    comptime var index: usize = 0;
    comptime var errors: [error_count]Type.Error = undefined;
    for (messages) |msg| {
        if (msg.@"0" != .fatal) continue;
        const pascal = pascalify(@tagName(msg.@"1"));
        errors[index] = .{ .name = pascal ++ "" };
        index += 1;
    }

    return @Type(.{ .error_set = errors[0..] });
}

fn SimpleToolsT(comptime messages: []const SimpleMessageInfo) type {
    return ToolsT(SimpleMessageT(messages), SimpleErrorT(messages));
}

pub fn simpleMessage(
    comptime data_msgs: []const SimpleMessageInfo,
    comptime no_data_msgs: []const SimpleMessageInfo,
) struct { type, type, SimpleToolsT(data_msgs ++ no_data_msgs) } {
    const Message = SimpleMessageT(data_msgs ++ no_data_msgs);
    const Error = SimpleErrorT(data_msgs ++ no_data_msgs);

    const Functions = struct {
        pub fn print(self: *const Message, writer: anytype) !void {
            inline for (data_msgs ++ no_data_msgs) |msg| {
                if (self.* == msg.@"1") {
                    const data = @field(self.*, @tagName(msg.@"1"));
                    return switch (msg.@"3") {
                        void => writer.writeAll(msg.@"2"),
                        else => writer.print(msg.@"2", data),
                    };
                }
            }
        }

        pub fn level(self: *const Message) Level {
            inline for (data_msgs ++ no_data_msgs) |msg|
                if (self.* == msg.@"1") return msg.@"0";
            unreachable;
        }

        pub fn fatality(self: *const Message) Error!void {
            inline for (data_msgs ++ no_data_msgs) |msg|
                if (self.* == msg.@"1" and msg.@"0" == .fatal)
                    return @field(Error, pascalify(@tagName(msg.@"1")));
        }

        pub fn printData(self: *const Message) bool {
            inline for (no_data_msgs) |msg|
                if (self.* == msg.@"1") return false;
            return true;
        }
    };

    return .{ Message, Error, .{
        .print = Functions.print,
        .level = Functions.level,
        .fatality = Functions.fatality,
        .printData = Functions.printData,
    } };
}
