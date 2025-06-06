//! A few things from zig-utftools and some codepoint categorization.

const std = @import("std");

// CODEPOINTS

const runerip = @import("runerip");

pub const CodePoint = struct {
    code: u21,
    offset: usize,
    len: usize,

    pub inline fn decodeCursor(slice: []const u8, cursor: *usize) ?CodePoint {
        const offset = cursor.*;
        const code = runerip.decodeRuneCursor(slice, cursor) catch return null;
        return .{ .code = code, .offset = offset, .len = cursor.* - offset };
    }
};

// I/O TOOLS

// based on std.unicode.{utf8CodepointSequenceLength, utf8Encode}
pub fn writeCodePointToUtf8(code: u21, writer: anytype) !void {
    if (code < 0x80) {
        try writer.writeByte(@as(u8, @intCast(code)));
    } else if (code < 0x800) {
        try writer.writeByte(@as(u8, @intCast(0b11000000 | (code >> 6))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (code & 0b111111))));
    } else if (code < 0x10000) {
        if (0xd800 <= code and code <= 0xdfff) return error.CannotEncodeSurrogateHalf;
        try writer.writeByte(@as(u8, @intCast(0b11100000 | (code >> 12))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((code >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (code & 0b111111))));
    } else if (code < 0x110000) {
        try writer.writeByte(@as(u8, @intCast(0b11110000 | (code >> 18))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((code >> 12) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((code >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (code & 0b111111))));
    } else {
        return error.codeTooLarge;
    }
}

pub inline fn debugChar(char: u21) !void {
    var stderr = std.io.getStdErr();
    try writeCodePointToUtf8(char, stderr.writer());
}

// CHARACTER CATEGORIZATION

// Does this character function as some gap in text?
pub fn isGap(char: u21) bool {
    return isSpace(char) or isNewLine(char);
}

// Is this character an "effective" space?
// Unicode Zs + Zp categories with tab added.
pub fn isSpace(char: u21) bool {
    return switch (char) {
        ' ',
        '\t',
        0x00A0,
        0x1680,
        0x2000...0x200A,
        0x202F,
        0x205F,
        0x3000,
        0x2029,
        => true,
        else => false,
    };
}

// Does this character start a new line?
pub fn isNewLine(char: u21) bool {
    return switch (char) {
        '\n',
        '\r',
        0x2028,
        => true,
        else => false,
    };
}

pub inline fn isNumber(char: u21) bool {
    return char >= '0' and char <= '9';
}

pub inline fn isHexadecimal(char: u21) bool {
    return (char >= '0' and char <= '9') or (char >= 'A' and char <= 'F') or (char >= 'a' and char <= 'f');
}

pub inline fn isLetter(char: u21) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z');
}

pub inline fn isAlphanumeric(char: u21) bool {
    return isNumber(char) or isLetter(char);
}
