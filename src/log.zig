const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

const Script = @import("Script.zig");

const ast = @import("ast.zig");
const Pos = ast.Pos;
const Span = ast.Span;

const unicode = @import("unicode.zig");

// LOGGER STRUCTURE

fn MsgContainer(
    comptime scope: @TypeOf(.EnumLiteral),
    comptime Message: type,
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

            if (builtin.mode == .Debug) // print scope in debug mode
                try writer.writeAll("[" ++ @tagName(scope) ++ "] ");

            try self.msg.format("", .{}, writer); // print message

            // print script name & position
            try writer.print(" [{s} - {}:{}]\n", .{
                self.script.name,
                self.span[0].row + 1,
                self.span[0].col + 1,
            });

            // find affected line bounds
            const src = self.script.src;
            var start = self.span[0].raw - self.span[0].col;
            while (start > 0 and !unicode.isNewLine(src[start - 1])) start -= 1;
            var end: usize = start;
            while (end < src.len and !unicode.isNewLine(src[end])) end += 1;

            // print affected line
            try writer.writeAll(src[start..end]);
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

        pub inline fn level(self: Self) std.log.Level {
            if (@hasDecl(Message, "level"))
                return self.msg.level();
            return .err;
        }
    };
}

pub fn Logger(
    comptime scope: @TypeOf(.EnumLiteral),
    comptime Message: type,
) type {
    if (!@hasDecl(Message, "format"))
        @compileError("format not implemented for logger message");

    return struct {
        script: *Script,
        allocator: Allocator,
        errors: std.ArrayList(Container),
        others: std.ArrayList(Container),

        const Self = @This();

        const Container = MsgContainer(scope, Message);

        pub fn init(allocator: Allocator, script: *Script) Self {
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

            if (level == .err) {
                try self.errors.append(c);
            } else {
                try self.others.append(c);
            }

            switch (c.level()) {
                .err => std.log.err("{}", .{c}),
                .warn => std.log.warn("{}", .{c}),
                .info => std.log.info("{}", .{c}),
                .debug => std.log.debug("{}", .{c}),
            }
        }
    };
}
