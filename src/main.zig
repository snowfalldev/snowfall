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

    for (tokens.items) |token| {
        const string = try token.token.toDebugString(allocator);
        defer allocator.free(string);
        std.debug.print(" - {{ {}..{}, {s} }}\n", .{ token.span[0], token.span[1], string });
    }
}
