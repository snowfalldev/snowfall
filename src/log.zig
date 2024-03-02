const std = @import("std");
const source = @import("ast/source.zig");

pub fn customLogger(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const prefix = "[" ++ @tagName(scope) ++ "] " ++ comptime level.asText() ++ ": ";

    // Print the message to stderr, silently ignoring any errors
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

pub fn Logger(comptime scope: @Type(.EnumLiteral)) type {
    const inner = std.log.scoped(scope);

    return struct {
        allocator: std.mem.Allocator,
        src: ?*const source.Source,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, src: ?*const source.Source) Self {
            return .{
                .allocator = allocator,
                .src = src,
            };
        }

        pub fn err(self: *Self, comptime fmt: []const u8, args: anytype, span: ?source.Span) anyerror!void {
            const str = try std.fmt.allocPrint(self.allocator, fmt, args);
            inner.err("{s}{s}\n{s}", .{ str, try self.infoString(span), try self.dataString(span) });
        }

        pub fn fatal(self: *Self, comptime fmt: []const u8, args: anytype, span: ?source.Span) noreturn {
            _ = self.err(fmt, args, span) catch @panic("OOM on fatal error");
            std.os.exit(1);
        }

        pub fn warn(self: *Self, comptime fmt: []const u8, args: anytype, span: ?source.Span) anyerror!void {
            const str = try std.fmt.allocPrint(self.allocator, fmt, args);
            inner.warn("{s}{s}\n{s}", .{ str, try self.infoString(span), try self.dataString(span) });
        }

        pub fn info(self: *Self, comptime fmt: []const u8, args: anytype, span: ?source.Span) anyerror!void {
            const str = try std.fmt.allocPrint(self.allocator, fmt, args);
            inner.info("{s}{s}\n{s}", .{ str, try self.infoString(span), try self.dataString(span) });
        }

        pub fn debug(self: *Self, comptime fmt: []const u8, args: anytype, span: ?source.Span) anyerror!void {
            const str = try std.fmt.allocPrint(self.allocator, fmt, args);
            inner.debug("{s}{s}\n{s}", .{ str, try self.infoString(span), try self.dataString(span) });
        }

        fn infoString(self: *Self, span: ?source.Span) anyerror![]u8 {
            if (self.src == null and span == null) return &[0]u8{};

            var out = std.ArrayList(u8).init(self.allocator);
            errdefer out.deinit();

            var name = false;
            if (self.src) |src| {
                if (src.name) |nm| {
                    name = true;
                    try out.appendSlice(" (");
                    try out.appendSlice(nm);
                }
            }

            if (span) |s| {
                if (name) {
                    try out.appendSlice(try std.fmt.allocPrint(self.allocator, ", {}:{})", .{ s[0].row + 1, s[0].col + 1 }));
                } else {
                    try out.appendSlice(try std.fmt.allocPrint(self.allocator, " ({}:{})", .{ s[0].row + 1, s[0].col + 1 }));
                }
            } else if (name) {
                try out.append(')');
            }

            return out.toOwnedSlice();
        }

        fn dataString(self: *Self, span: ?source.Span) anyerror![]u8 {
            if (self.src == null or span == null) return &[0]u8{};
            const s = span.?;
            const d = self.src.?.data;

            const line_start_pos = s[0].raw - s[0].col;
            var line_end_pos: usize = line_start_pos;
            while (line_end_pos < d.len and d[line_end_pos] != '\n') : (line_end_pos += 1) {}

            var out = std.ArrayList(u8).init(self.allocator);
            errdefer out.deinit();
            try out.appendSlice(d[line_start_pos..line_end_pos]);
            try out.append('\n');

            var i: usize = 0;
            while (i < s[0].col) : (i += 1) try out.append(' ');
            if (s[1].col > s[0].col) {
                try out.append('^');
                i += 1;
                while (i < s[1].col + 1) : (i += 1) try out.append('~');
            }
            try out.append('\n');

            return out.toOwnedSlice();
        }
    };
}
