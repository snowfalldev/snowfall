const std = @import("std");
const Allocator = std.mem.Allocator;

const value = @import("../value.zig");
const Value = value.Value;

inner: []const u8,

const Self = @This();

pub inline fn hash(self: Self) u64 {
    return std.hash.Wyhash.hash(0xCAFEBABEBADDC0DE, self.inner);
}

pub inline fn print(self: Self, comptime tagged: bool, writer: anytype) !void {
    if (tagged) try writer.writeAll("string: \"");
    try writer.writeAll(self.inner);
    if (tagged) try writer.writeByte('"');
}

pub fn addValue(self: Self, val: Value, allocator: Allocator) !Self {
    var out = try std.ArrayList(u8).initCapacity(allocator, self.inner.len);
    out.appendSliceAssumeCapacity(self.inner);
    val.print(false, out.writer());
    return .{ .inner = out.toOwnedSlice() };
}
