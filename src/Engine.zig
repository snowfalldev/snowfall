const std = @import("std");

const Script = @import("Script.zig");

allocator: std.mem.Allocator,
scripts: std.StringHashMap(*Script),

const Self = @This();

pub inline fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .scripts = std.StringHashMap(*Script).init(allocator),
    };
}

pub inline fn registerScript(self: *Self, name: []const u8, src: []const u8) !*Script {
    const mod = try Script.init(self, name, src);
    try self.scripts.put(name, mod);
    return mod;
}

pub inline fn getScript(self: *Self, name: []const u8) ?*Script {
    return self.scripts.get(name);
}

pub fn deinit(self: *Self) void {
    var scripts = self.scripts.iterator();
    while (scripts.next()) |mod|
        mod.value_ptr.*.deinit();

    self.scripts.deinit();
}
