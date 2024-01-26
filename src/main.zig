const std = @import("std");
const unicode = @import("unicode.zig");

pub const std_options = struct {
    pub const fmt_max_depth = 3;
    pub const log_level = .info;
    //pub const logFn = @import("log.zig").customLogger;
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const js =
        \\function bruh(a) {
        \\  console.log(a);
        \\}
        \\
        \\bruh("mЖ丽 😀")
    ;

    const string_utf8 = try unicode.StaticString.fromUtf8(allocator, js);
    const string_utf16 = try unicode.StaticString.fromUtf16le(allocator, try string_utf8.toUtf16le(allocator));

    std.debug.print("{s}", .{try string_utf16.toUtf8(allocator)});
}
