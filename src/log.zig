const std = @import("std");
const Span = @import("common.zig").Span;

pub fn customLogger(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const prefix = comptime level.asText() ++ " [" ++ @tagName(scope) ++ "] ";

    // Print the message to stderr, silently ignoring any errors
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

pub fn info(comptime fmt: []const u8, args: anytype, src: ?[]const u8, span: ?Span) anyerror!void {
    const str = try std.fmt.allocPrint(self.allocator, fmt, args);
    std.debug.print("INFO: {s}{s}\n", .{ str, try infoString(self.allocator, self.name, start) });
    std.debug.print("{s}", .{try dataString(self.allocator, span, self.data)});
}

pub fn warn(comptime fmt: []const u8, args: anytype, src: ?[]const u8, span: ?Span) anyerror!void {
    const str = try std.fmt.allocPrint(self.allocator, fmt, args);
    std.debug.print("WARN: {s}{s}\n", .{ str, try infoString(self.allocator, self.name, start) });
    std.debug.print("{s}", .{try dataString(self.allocator, span, self.data)});
}

pub fn err(comptime fmt: []const u8, args: anytype, src: ?[]const u8, span: ?Span) anyerror!void {
    std.log.err(fmt ++ "{s}{s}\n{s}", .{});
    const str = try std.fmt.allocPrint(self.allocator, fmt, args);
    std.debug.print("ERROR: {s}{s}\n", .{ str, try infoString(self.allocator, self.name, start) });
    std.debug.print("{s}", .{try dataString(self.allocator, span, self.data)});
}

pub fn fatal(comptime fmt: []const u8, args: anytype, span: ?Span) noreturn {
    _ = try err(fmt, args, span);
    std.os.exit(1);
}

inline fn unwrap_span(span: ?Span) struct { struct { ?usize, ?usize }, struct { ?usize, ?usize } } {
    if (span) |s| {
        return .{ .{ s.start.row, s.start.col }, .{ s.end.row, s.end.col } };
    }

    return .{ .{ null, null }, .{ null, null } };
}

fn infoString(allocator: std.mem.Allocator, name: ?[]const u8, span: ?Span) anyerror![]u8 {
    if (name == null and span == null) return &[0]u8{};

    var out = std.ArrayList(u8).init(allocator);
    errdefer out.deinit();

    if (name) |nm| {
        try out.appendSlice(" (");
        try out.appendSlice(nm);
    }

    if (span) |s| {
        if (name != null) {
            try out.appendSlice(try std.fmt.allocPrint(allocator, ", {}:{})", .{ s.start.row + 1, s.start.col + 1 }));
        } else {
            try out.appendSlice(try std.fmt.allocPrint(allocator, " ({}:{})", .{ s.start.row + 1, s.start.col + 1 }));
        }
    } else if (name != null) {
        try out.append(')');
    }

    return out.toOwnedSlice();
}

fn dataString(allocator: std.mem.Allocator, span: ?Span, data: ?[]const u8) anyerror![]u8 {
    if (data == null or span == null) return &[0]u8{};
    const s = span.?;
    const d = data.?;

    const line_start_pos = s.start.raw - s.start.col;
    var line_end_pos: usize = line_start_pos;
    while (line_end_pos < d.len and d[line_end_pos] != '\n') : (line_end_pos += 1) {}

    var out = std.ArrayList(u8).init(allocator);
    errdefer out.deinit();
    try out.appendSlice(d[line_start_pos..line_end_pos]);
    try out.append('\n');

    var i: usize = 0;
    while (i < s.start.col) : (i += 1) try out.append(' ');
    if (s.end.col > s.start.col) {
        try out.append('^');
        i += 1;
        while (i < s.end.col + 1) : (i += 1) try out.append('~');
    }
    try out.append('\n');

    return try std.unicode.utf16leToUtf8Alloc(allocator, try out.toOwnedSlice());
}
