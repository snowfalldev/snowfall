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

// UTILITIES

// std.log.Level with fatal added.
pub const Level = enum { fatal, err, warn, info, debug };

var config: tty.Config = undefined;
var get_config_once = std.once(get_config);
fn get_config() void {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    config = tty.detectConfig(std.io.getStdErr());
}

// LOGGER STRUCTURE

fn MsgContainer(
    comptime scope: @Type(.enum_literal),
    comptime Message: type,
    comptime print_fn: fn (*const Message, anytype) anyerror!void,
    comptime level_fn: ?fn (*const Message) Level,
) type {
    return struct {
        script: *Script,
        msg: Message,
        span: Span,
        hi: ?Pos,

        const Self = @This();

        pub fn format(
            self: *const Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            return self.printRaw(writer, false);
        }

        pub inline fn printRaw(
            self: *const Self,
            writer: anytype,
            stderr: bool,
        ) !void {
            if (stderr) {
                // set to bold before we print message
                try config.setColor(writer, .reset);
                try config.setColor(writer, .bold);
            }

            try print_fn(&self.msg, writer); // print message

            if (builtin.mode == .Debug) // print scope in debug mode
                try writer.writeAll("[" ++ @tagName(scope) ++ "] ");

            // print script name & position
            try writer.print(" [{s} - {}:{}]\n", .{
                self.script.name,
                self.span[0].row + 1,
                self.span[0].col + 1,
            });

            // reset color before we print affected code
            if (stderr) try config.setColor(writer, .reset);

            // find affected line bounds
            const src = self.script.src;
            var start = self.span[0].raw - self.span[0].col;
            while (start > 0 and !unicode.isNewLine(src[start - 1])) start -= 1;
            var end: usize = start;
            while (end < src.len and !unicode.isNewLine(src[end])) end += 1;

            // print affected line
            try writer.writeAll(src[start..end]);
            try writer.writeByte('\n');

            // set color to bright green before we print highlight
            if (stderr) try config.setColor(writer, .green);

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

            // reset color before we leave
            if (stderr) try config.setColor(writer, .reset);
        }

        pub inline fn level(self: Self) Level {
            return if (level_fn) |l| l(&self.msg) else Level.err;
        }
    };
}

pub fn Logger(
    comptime scope: @Type(.enum_literal),
    comptime Message: type,
    comptime print_fn: fn (*const Message, anytype) anyerror!void,
    comptime level_fn: ?fn (*const Message) Level,
) type {
    return struct {
        script: *Script,
        allocator: Allocator,
        errors: std.ArrayList(Container),
        others: std.ArrayList(Container),

        const Self = @This();

        const Container = MsgContainer(scope, Message, print_fn, level_fn);

        pub fn init(allocator: Allocator, script: *Script) Self {
            get_config_once.call();

            return .{
                .script = script,
                .allocator = allocator,
                .errors = std.ArrayList(Container).init(allocator),
                .others = std.ArrayList(Container).init(allocator),
            };
        }

        pub inline fn deinit(self: Self) void {
            self.msgs.deinit();
        }

        inline fn mkContainer(self: Self, msg: Message, span: Span, hi: ?Pos) Container {
            return .{ .script = self.script, .msg = msg, .span = span, .hi = hi };
        }

        pub fn log(self: *Self, msg: Message, span: Span, hi: ?Pos) !void {
            const c = self.mkContainer(msg, span, hi);
            const level = c.level();

            if (level == .fatal) self.script.failed = true;

            switch (level) {
                .fatal, .err => try self.errors.append(c),
                else => try self.others.append(c),
            }

            const color: Color, const name: []const u8 = switch (level) {
                .fatal => .{ .red, "fatal" },
                .err => .{ .red, "error" },
                .warn => .{ .yellow, "warning" },
                .info => .{ .green, "info" },
                .debug => .{ .cyan, "debug" },
            };

            std.debug.lockStdErr();
            defer std.debug.unlockStdErr();
            const stderr = std.io.getStdErr().writer();

            // print level
            try config.setColor(stderr, .reset);
            try config.setColor(stderr, .bold);
            try config.setColor(stderr, color);
            if (level == .fatal) try config.setColor(stderr, .dim);
            try stderr.print("{s}: ", .{name});

            // print message
            try c.printRaw(stderr, true);
        }
    };
}

// SIMPLE MESSAGES

pub const SimpleMessageInfo = struct { Level, @Type(.enum_literal), []const u8, type };

fn SimpleMessageT(comptime messages: []const SimpleMessageInfo) type {
    const Type = std.builtin.Type;

    // make enum type

    comptime var enum_fields: [messages.len]Type.EnumField = undefined;
    inline for (messages, 0..) |msg, i| {
        enum_fields[i] = .{
            .name = @tagName(msg.@"1"),
            .value = i,
        };
    }

    const enum_info = Type{ .@"enum" = .{
        .tag_type = u16,
        .fields = &enum_fields,
        .decls = &.{},
        .is_exhaustive = true,
    } };
    const EnumT = @Type(enum_info);

    // make union type

    comptime var union_fields: [messages.len]Type.UnionField = undefined;
    inline for (messages, 0..) |msg, i| {
        union_fields[i] = .{
            .name = @tagName(msg.@"1"),
            .type = msg.@"3",
            .alignment = 0,
        };
    }

    const union_info = Type{ .@"union" = .{
        .layout = .auto,
        .tag_type = EnumT,
        .fields = &union_fields,
        .decls = &.{},
    } };
    return @Type(union_info);
}

pub fn simpleMessage(comptime messages: []const SimpleMessageInfo) blk: {
    const Message = SimpleMessageT(messages);
    const PrintFn = fn (*const Message, anytype) anyerror!void;
    const LevelFn = fn (*const Message) Level;
    break :blk struct {
        type: type,
        print: PrintFn,
        level: LevelFn,
    };
} {
    const Message = SimpleMessageT(messages);

    const Functions = struct {
        pub fn print(self: *const Message, writer: anytype) !void {
            inline for (messages) |msg| {
                if (self.* == msg.@"1") {
                    const data = @field(self.*, @tagName(msg.@"1"));
                    switch (@TypeOf(data)) {
                        void => try writer.writeAll(msg.@"2"),
                        else => try writer.print(msg.@"2", data),
                    }
                }
            }
        }

        pub fn level(self: *const Message) Level {
            inline for (messages) |msg|
                if (self.* == msg.@"1") return msg.@"0";
            unreachable;
        }
    };

    return .{
        .type = Message,
        .print = Functions.print,
        .level = Functions.level,
    };
}
