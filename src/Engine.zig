const std = @import("std");

const hash = @import("util.zig").hash;
const Script = @import("Script.zig");

allocator: std.mem.Allocator,
scripts: hash.StringHashMapUnmanaged(*Script, hash.rapidhash) = .{},

const Self = @This();

// INIT / DEINIT

pub inline fn init(allocator: std.mem.Allocator) Self {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Self) void {
    var scripts = self.scripts.iterator();
    while (scripts.next()) |script|
        script.value_ptr.*.deinit();

    self.scripts.deinit(self.allocator);
}

// REGISTER SCRIPTS

pub inline fn registerScript(self: *Self, name: []const u8, src: []const u8) !*Script {
    const script = try Script.init(self, name, src);
    try self.scripts.put(self.allocator, name, script);
    return script;
}

// GET SCRIPTS

pub inline fn getScript(self: *Self, name: []const u8) ?*Script {
    return self.scripts.get(name);
}
