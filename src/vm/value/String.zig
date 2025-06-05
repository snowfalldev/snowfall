const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;

inner: []const u8,

const Self = @This();

const hash_seed_str: []align(8) const u8 = @alignCast("stringss");
const hash_seed = @as(*const u64, @ptrCast(hash_seed_str)).*;

pub inline fn hash(self: Self) u64 {
    return std.hash.Wyhash.hash(hash_seed, self.inner);
}

pub inline fn print(self: Self, writer: anytype) !void {
    try writer.writeAll(self.inner);
}

pub fn add(self: Self, val: Value, prepend: bool, allocator: Allocator) !Self {
    var out = std.ArrayList(u8).init(allocator);
    if (prepend) try val.print(false, out.writer());
    try out.appendSlice(self.inner);
    if (!prepend) try val.print(false, out.writer());
    return .{ .inner = out.toOwnedSlice() };
}
