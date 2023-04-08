const common = @import("common.zig");
const std = @import("std");

pub const Messages = struct {
    allocator: std.mem.Allocator,
    name: ?[]const u8,
    data: ?[]const u8,

    notes: std.ArrayList([]u8),
    warns: std.ArrayList([]u8),
    errors: std.ArrayList([]u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, name: ?[]const u8, data: ?[]const u8) Self {
        return .{
            .allocator = allocator,
            .name = name,
            .data = data,

            .notes = std.ArrayList([]u8).init(allocator),
            .warns = std.ArrayList([]u8).init(allocator),
            .errors = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.notes.deinit();
        self.warns.deinit();
        self.errors.deinit();
    }

    pub fn printNote(self: *Self, comptime fmt: []const u8, args: anytype, start: ?common.Position, end: ?common.Position) anyerror!void {
        var note = try std.fmt.allocPrint(self.allocator, fmt, args);
        std.debug.print("NOTE: {s}{s}\n", .{ note, try infoString(self.allocator, self.name, start) });
        std.debug.print("{s}", .{try dataString(self.allocator, start, end, self.data)});
        try self.notes.append(note);
    }

    pub fn printWarn(self: *Self, comptime fmt: []const u8, args: anytype, start: ?common.Position, end: ?common.Position) anyerror!void {
        var warn = try std.fmt.allocPrint(self.allocator, fmt, args);
        std.debug.print("WARN: {s}{s}\n", .{ warn, try infoString(self.allocator, self.name, start) });
        std.debug.print("{s}", .{try dataString(self.allocator, start, end, self.data)});
        try self.warns.append(warn);
    }

    pub fn printError(self: *Self, comptime fmt: []const u8, args: anytype, start: ?common.Position, end: ?common.Position) anyerror!void {
        var err = try std.fmt.allocPrint(self.allocator, fmt, args);
        std.debug.print("ERROR: {s}{s}\n", .{ err, try infoString(self.allocator, self.name, start) });
        std.debug.print("{s}", .{try dataString(self.allocator, start, end, self.data)});
        try self.errors.append(err);
    }

    pub fn printFatal(self: *Self, comptime fmt: []const u8, args: anytype, start: ?common.Position, end: ?common.Position) anyerror!void {
        _ = try self.printError(fmt, args, start, end);
        std.os.exit(1);
    }

    pub fn clearAndFree(self: *Self) void {
        self.notes.clearAndFree();
        self.warns.clearAndFree();
        self.errors.clearAndFree();
    }

    pub fn clearRetainingCapacity(self: *Self) void {
        self.notes.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.errors.clearRetainingCapacity();
    }
};

pub const MessageError = error{
    no_position,
};

fn infoString(allocator: std.mem.Allocator, name: ?[]const u8, pos: ?common.Position) anyerror![]u8 {
    if (name == null and pos == null) return &[0]u8{};

    var info_bytes = common.Bytes.init(allocator);

    if (name) |nm| {
        try info_bytes.appendSlice(" (");
        try info_bytes.appendSlice(nm);
    }

    if (pos) |p| {
        if (name != null) {
            try info_bytes.appendSlice(try std.fmt.allocPrint(allocator, ", {}:{})", .{ p.row + 1, p.col + 1 }));
        } else {
            try info_bytes.appendSlice(try std.fmt.allocPrint(allocator, " ({}:{})", .{ p.row + 1, p.col + 1 }));
        }
    } else if (name != null) {
        try info_bytes.append(')');
    }

    return info_bytes.toOwnedSlice();
}

fn dataString(allocator: std.mem.Allocator, start: ?common.Position, end: ?common.Position, data: ?[]const u8) anyerror![]u8 {
    if (data == null or start == null) return &[0]u8{};

    var line_start_pos = start.?.raw - start.?.col;
    var line_end_pos: usize = line_start_pos;

    while (line_end_pos < data.?.len and data.?[line_end_pos] != '\n') : (line_end_pos += 1) {}

    var start_pos = start.?.col;

    var data_bytes = common.Bytes.init(allocator);
    try data_bytes.appendSlice(data.?[line_start_pos..line_end_pos]);
    try data_bytes.append('\n');

    var i: usize = 0;
    while (i < start_pos) : (i += 1) try data_bytes.append(' ');
    if (end) |e| while (i < e.col + 1) : (i += 1) try data_bytes.append('~');

    try data_bytes.append('\n');

    return try data_bytes.toOwnedSlice();
}
