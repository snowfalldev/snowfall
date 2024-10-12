const std = @import("std");

pub inline fn isNumberChar(char: u21) bool {
    return char >= '0' and char <= '9';
}

pub inline fn isHexadecimalChar(char: u21) bool {
    return (char >= '0' and char <= '9') or (char >= 'A' and char <= 'F') or (char >= 'a' and char <= 'f');
}

pub inline fn isLetterChar(char: u21) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z');
}

pub inline fn isAlphanumericChar(char: u21) bool {
    return isNumberChar(char) or isLetterChar(char);
}

pub inline fn isLineSeparator(char: u21) bool {
    // keep in sync with Zl Unicode category
    return char == '\n' or char == '\r' or char == 0x2028;
}

// Optionally get value from "content" (array of T).
pub inline fn getOptional(comptime T: type, content: []const T, pos: usize) ?T {
    if (content.len < pos + 1) return null;
    return content[pos];
}

pub inline fn mkStringMap(comptime T: type) std.StaticStringMap(T) {
    const type_info = @typeInfo(T);
    if (type_info != .@"enum") @compileError("mkStringMap with non-enum type");

    const fields = type_info.@"enum".fields;
    comptime var out: [fields.len]struct { []const u8, T } = undefined;

    inline for (fields, 0..) |field, i| {
        out[i] = .{ field.name, @field(T, field.name) };
    }

    return std.StaticStringMap(T).initComptime(out);
}
