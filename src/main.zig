const std = @import("std");
const jemalloc = @import("jemalloc").allocator;

const Module = @import("Module.zig");

pub const std_options = .{
    .fmt_max_depth = 3,
    .logFn = @import("log.zig").customLogger,
};

pub fn main() !void {
    const data =
        \\pub const test = "haiii 👋";
        \\
        \\// this is a regular comment, doesn't get tokenized
        \\/// this is a doc comment, gets tokenized and attached to the function below
        \\pub func testFunc(str: string, num: number) void {
        \\  str[0] = '\x62';
        \\  var a = +43;
        \\  a += b-2;
        \\  std.printf("%s %d\n", str, a);
        \\}
        \\
        \\/*
        \\bigger comment
        \\look it's multiline waow
        \\*/
        \\/**
        \\bigger doc comment
        \\im multiline too!!
        \\*/
        \\pub func main() void {
        \\  testFunc(test, -1);
        \\  const um = '\n';
        \\  const um2 = 'n';
        \\  std.print('\u{1F480}');
        \\  std.println(" <- skull\n\u{1F480}\u{1F480}💀 <- oh it's here thrice");
        \\  std.println("Hello, world!");
        \\  std.printf("1 + 2 = %d\n", 3);
        \\  std.printf("0.1 * 5.5 = %d\n", 0.55);
        \\  std.printf("3 + 1 = %d\n", 4u32);
        \\  std.printf("0.2 * 3.3 = %d\n", 6.6e-1f16);
        \\  @panic("welp");
        \\}
    ;

    var mod = try Module.init(.{ .utf8 = data }, .{ .utf8 = "main" }, std.heap.page_allocator);
    defer mod.deinit();
    const tokens = try mod.finishLexer();

    for (tokens) |token| {
        const string = try token.token.toString(jemalloc);
        defer jemalloc.free(string);
        std.debug.print(" - {{ {}..{}, {s} }}\n", .{ token.span[0].col, token.span[1].col, string });
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
