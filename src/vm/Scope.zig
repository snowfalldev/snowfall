const std = @import("std");
const Arena = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;

const Script = @import("../Script.zig");

const Value = @import("values.zig").Value;

const Self = @This();

script: *Script,
parent: ?*Self = null,

arena: Arena,
allocator: Allocator,

vars: std.StringHashMapUnmanaged(Variable),
// lock for reading the vars map, not for var mutation
// only writer should be the thread the scope runs in
lock: std.Thread.RwLock = .{},

pub const Variable = struct {
    mutable: bool = false,
    value: Value,
    // per-variable lock
    lock: std.Thread.RwLock = .{},
};

pub fn deinit(self: *Self) void {
    self.vars.clearAndFree(self.allocator);
}

pub fn reset(self: *Self) void {
    self.vars.clearRetainingCapacity();
}

pub fn findScopeWithVar(self: *Self, name: []const u8) ?*Self {
    var scope: ?*Self = self;
    while (scope) |s| {
        s.lock.lockShared();
        defer s.lock.unlockShared();
        if (s.vars.contains(name)) break;
        scope = s.parent;
    }
    return scope;
}

pub fn initVar(self: *Self, name: []const u8, v: Variable) !void {
    if (self.findScopeWithVar(name) != null)
        return error.AlreadyExists;

    self.lock.lock();
    defer self.lock.unlock();
    try self.vars.putNoClobber(name, v);
}

pub fn getVar(self: *Self, name: []const u8) ?*Variable {
    _ = self;
    _ = name;
    //const scope = self.findScopeWithVar(name) orelse return null;
}
