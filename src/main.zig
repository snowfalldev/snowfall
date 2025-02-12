const std = @import("std");
const builtin = @import("builtin");

const helium = @import("helium");
const Engine = helium.Engine;

pub fn main() !void {
    const data = @embedFile("testfile.he");

    const allocator = std.heap.smp_allocator;

    //var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    //defer arena.deinit();
    //const allocator = arena.allocator();

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
