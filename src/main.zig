const std = @import("std");
const jemalloc = @import("jemalloc").allocator;

const helium = @import("helium");
const Module = helium.Module;

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

    var mod = try Module.init(data, "main", std.heap.page_allocator);
    defer mod.deinit();
    const tokens = try mod.finishLexer();

    for (tokens) |token| {
        const string = try token.token.toString(jemalloc);
        defer jemalloc.free(string);
        std.debug.print(" - {{ {}..{}, {s} }}\n", .{ token.span[0].col, token.span[1].col, string });
    }
}
