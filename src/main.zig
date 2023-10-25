const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

pub const std_options = struct {
    pub const fmt_max_depth = 3;
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = Lexer.init(allocator, "main",
        \\pub const test: []const u8 = "haiii";
        \\
        \\pub fn testFunc(string: []const u8, idfk: usize) void {
        \\  string[0] = 'b';
        \\  let mut a = 0;
        \\  a += b;
        \\  std.println(string);
        \\}
        \\
        \\pub fn main() void {
        \\  testFunc(test, 0);
        \\  std.println("\u{f09f9280} <- skull");
        \\  std.println("Hello, world!");
        \\  std.printf("1 + 2 = {f}\n", 3);
        \\  std.printf("0.1 * 5.5 = {f}\n", 0.55);
        \\  std.printf("3 + 1 = {f}\n", 4usize);
        \\  std.printf("0.2 * 3.3 = {f}\n", 0.33f32);
        \\  @panic("welp");
        \\}
    );
    //defer lexer.deinit();

    var tokens = try lexer.lexFull();

    for (tokens.items) |token| {
        std.debug.print(" - {{{}..{}, {s}}}\n", .{ token.start.col, token.end.col, try token.token.toString(allocator) });
    }

    var parser = try Parser.init(allocator, tokens, "main", lexer.data);
    //defer parser.deinit();
    var tree = try parser.parseFull();
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
