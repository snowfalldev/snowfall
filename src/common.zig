const std = @import("std");

// Number types.
// *size types are platform based.
pub const number_types = [_][]const u8{
    "usize", // unsigned platform-based integer
    "u128", // unsigned 128-bit integer
    "u64", // unsigned 64-bit integer
    "u32", // unsigned 32-bit integer
    "u16", // unsigned 16-bit integer
    "u8", // unsigned 8-bit integer
    "isize", // signed platform-based integer
    "i128", // signed 128-bit integer
    "i64", // signed 64-bit integer
    "i32", // signed 32-bit integer
    "i16", // signed 16-bit integer
    "i8", // signed 8-bit integer
    "f128", // 128-bit float
    "f64", // 64-bit float
    "f32", // 32-bit float
    "f16", // 16-bit float
};

pub const builtin_types = number_types ++ [_][]const u8{
    "type",
    "bool",
    "void",
    "any",
};

pub const Bytes = std.ArrayList(u8);

pub const Position = struct {
    raw: usize,
    row: usize,
    col: usize,

    // Returns the position, back N characters.
    // Raw and row must be over 0.
    pub fn back(pos: *Position, n: usize) Position {
        return .{
            .raw = pos.raw - n,
            .row = pos.row,
            .col = pos.col - n,
        };
    }

    // Returns the position, forward N characters.
    pub fn forward(pos: *Position, n: usize) Position {
        return .{
            .raw = pos.raw + n,
            .row = pos.row,
            .col = pos.col + n,
        };
    }
};

pub fn containsChar(comptime haystack: []u8, needle: u8) bool {
    inline for (haystack) |char| if (char == needle) return true;
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
