const std = @import("std");
const builtin = @import("builtin");

const snowfall = @import("snowfall");
const Engine = snowfall.Engine;

pub const std_options = std.Options{ .logFn = snowfall.log.customLogFn };

pub fn main() !void {
    const data = @embedFile("testfile.he");

    const allocator = if (builtin.mode == .Debug) blk: {
        var dbg_alloc = std.heap.DebugAllocator(.{}).init;
        break :blk dbg_alloc.allocator();
    } else std.heap.smp_allocator;

    var engine = Engine.init(allocator);
    defer engine.deinit();
    var module = try engine.registerScript("main", data);
    try module.prepare();

    for (module.tokens) |token|
        std.debug.print("({}, {}..{}) {}\n", .{
            token.span[0].raw,
            token.span[0],
            token.span[1],
            token.token,
        });
}
