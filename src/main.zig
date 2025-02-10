const std = @import("std");
const builtin = @import("builtin");

const helium = @import("helium");
const Module = helium.Module;

pub fn main() !void {
    const data = @embedFile("testfile.he");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var mod = try Module.init(data, "main", std.heap.page_allocator);
    defer mod.deinit();
    const tokens = try mod.finishLexer();

    for (tokens) |token| {
        const string = try token.token.toString(allocator);
        defer allocator.free(string);
        std.debug.print(" - {{ {}..{}, {s} }}\n", .{ token.span[0], token.span[1], string });
    }
}
