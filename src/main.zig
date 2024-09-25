const std = @import("std");
const Lexer = @import("ast.zig").lexer.Lexer;

pub const std_options = .{
    .fmt_max_depth = 3,
    .logFn = @import("log.zig").customLogger,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var lexer = Lexer.init(allocator, .{ .name = "main", .data = 
    \\pub const test = "haiii";
    \\
    \\pub func testFunc(str: string, num: number) void {
    \\  str[0] = '\x62';
    \\  var a = +43;
    \\  a += b-2;
    \\  std.printf("%s %d\n", str, a);
    \\}
    \\
    \\pub func main() void {
    \\  testFunc(test, -1);
    \\  std.print('\u{1F480}');
    \\  std.println(" <- skull");
    \\  std.println("Hello, world!");
    \\  std.printf("1 + 2 = %d\n", 3);
    \\  std.printf("0.1 * 5.5 = %f\n", 0.55);
    \\  std.printf("3 + 1 = %d\n", 4u32);
    \\  std.printf("0.2 * 3.3 = %f\n", 0.33f16);
    \\  @panic("welp");
    \\}
    });
    defer lexer.deinit();

    const tokens = try lexer.lexFull();

    for (tokens.items) |token| {
        std.debug.print(" - {{ {}..{}, {s} }}\n", .{ token.span[0].col, token.span[1].col, try token.token.toString(allocator) });
    }

    //var parser = try Parser.init(allocator, tokens, "main", lexer.data);
    //defer parser.deinit();
    //const tree = try parser.parseFull();
    //lexer.deinit();
    //parser.deinit();

    //std.debug.print("AST (items: {})\n", .{parser.output.items.len});

    //for (tree.items) |node| {
    //    std.debug.print(" - {}\n", .{node});
    //}

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
