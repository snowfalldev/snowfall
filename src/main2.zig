const std = @import("std");
const Lexer = @import("js/lexer.zig").Lexer;
const Parser = @import("js/parser.zig").Parser;

pub const std_options = struct {
    pub const fmt_max_depth = 3;
    pub const log_level = .info;
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = Lexer.init(allocator, "main", std.unicode.utf8ToUtf16LeStringLiteral(
        \\function bruh(a) {
        \\  console.log(a);
        \\}
        \\
        \\bruh("haii")
    ));
    //defer lexer.deinit();

    const tokens = try lexer.lexFull();

    for (tokens.items) |token| {
        std.debug.print(" - {{{}..{}, {s}}}\n", .{ token.start.col, token.end.col, try token.token.toString(allocator) });
    }

    var parser = try Parser.init(allocator, tokens, "main", lexer.data);
    //defer parser.deinit();
    const tree = try parser.parseFull();
    lexer.deinit();
    parser.deinit();

    std.debug.print("AST (items: {})\n", .{parser.output.items.len});

    for (tree.items) |node| {
        std.debug.print(" - {}\n", .{node});
    }

    // BENCHMARK
    //var i: usize = 0;
    //while (i < 10000) : (i += 1) {
    //    _ = try lexer.lexFull();
    //    lexer.clearRetainingCapacity();
    //}

    // PRINT TOKENS
    //for ((try lexer.lexFull()).items) |token| {
    //    std.debug.print(" - {{{}..{}, {s}}}\n", .{ token.start.col, token.end.col, try token.token.toString(allocator) });
    //}
}
