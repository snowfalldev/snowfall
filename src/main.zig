const std = @import("std");
const ast = @import("ast.zig");
const jdz = @import("jdz_allocator");

pub const std_options = .{
    .fmt_max_depth = 3,
    .logFn = @import("log.zig").customLogger,
};

pub fn main() !void {
    var jdzalloc = jdz.JdzAllocator(.{}).init();
    defer jdzalloc.deinit();
    const allocator = jdzalloc.allocator();

    const data =
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
        \\  const um = '\n';
        \\  const um2 = 'n';
        \\  std.print('\u{1F480}');
        \\  std.println(" <- skull\n\u{1F480}\u{1F480} <- oh it's here twice");
        \\  std.println("Hello, world!");
        \\  std.printf("1 + 2 = %d\n", 3);
        \\  std.printf("0.1 * 5.5 = %f\n", 0.55);
        \\  std.printf("3 + 1 = %d\n", 4u32);
        \\  std.printf("0.2 * 3.3 = %f\n", 0.33f16);
        \\  @panic("welp");
        \\}
    ;

    const src = try ast.source.Source.init(.{ .utf8 = data }, .{ .utf8 = "main" }, allocator);
    defer src.deinit();
    var lexer = ast.Lexer.init(allocator, src);
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
