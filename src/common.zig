const std = @import("std");

pub fn containsChar(comptime haystack: []const u8, needle: u8) bool {
    inline for (haystack) |char| if (char == needle) return true;
    return false;
}

pub inline fn containsCharRuntime(haystack: []u8, needle: u8) bool {
    for (haystack) |char| if (char == needle) return true;
    return false;
}

pub fn containsString(comptime haystack: [][]const u8, needle: []u8) bool {
    inline for (haystack) |string| if (std.mem.eql(u8, string, needle)) return true;
    return false;
}

pub inline fn isNumberChar(char: u8) bool {
    return char >= '0' and char <= '9';
}

pub inline fn isHexadecimalChar(char: u8) bool {
    return (char >= '0' and char <= '9') or (char >= 'A' and char <= 'F') or (char >= 'a' and char <= 'f');
}

pub inline fn isLetterChar(char: u8) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z');
}

pub inline fn isAlphanumericChar(char: u8) bool {
    return isNumberChar(char) or isLetterChar(char);
}

// Optionally get value from "content" (array of T).
pub inline fn getOptional(comptime T: type, content: []T, pos: usize) ?T {
    if (content.len < pos + 1) return null;
    return content[pos];
}
