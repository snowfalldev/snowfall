const std = @import("std");
const builtin = @import("builtin");

const helium = @import("helium");
const Engine = helium.Engine;

pub fn main() !void {
    const data = @embedFile("testfile.he");

    const allocator = if (builtin.mode == .Debug) blk: {
        var dbg_alloc = std.heap.DebugAllocator(.{}).init;
        break :blk dbg_alloc.allocator();
    } else std.heap.smp_allocator;

    var engine = Engine.init(allocator);
    var module = try engine.registerScript("main", data);
    const tokens = try module.finishLexer();

    defer engine.deinit();

    if (true) return;

    const padding: [25]u8 = .{' '} ** 25;

    for (tokens) |token| {
        const string = try token.token.toDebugString(allocator);
        defer allocator.free(string);
        const offset = @min(25 -| string.len, 25);
        std.debug.print(" - {s} {s}[{}..{}]\n", .{ string, padding[0..offset], token.span[0], token.span[1] });
    }
}
