//! A few things from zig-utftools and some character categorization.

const std = @import("std");

// RUNES

const runerip = @import("runerip");

pub const Rune = packed struct {
    offset: usize,
    code: u21,
    len: u3,

    pub inline fn decodeCursor(slice: []const u8, cursor: *usize) ?Rune {
        const offset = cursor.*;
        const code = runerip.decodeRuneCursor(slice, cursor) catch return null;
        return .{ .offset = offset, .code = code, .len = @truncate(cursor.* - offset) };
    }
};

// I/O TOOLS

// based on std.unicode.{utf8CodepointSequenceLength, utf8Encode}
pub fn writeCharUtf8(char: u21, writer: anytype) !void {
    if (char < 0x80) {
        try writer.writeByte(@as(u8, @intCast(char)));
    } else if (char < 0x800) {
        try writer.writeByte(@as(u8, @intCast(0b11000000 | (char >> 6))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (char & 0b111111))));
    } else if (char < 0x10000) {
        if (char >= 0xD800 and char <= 0xDFFF) return error.CannotEncodeSurrogateHalf;
        try writer.writeByte(@as(u8, @intCast(0b11100000 | (char >> 12))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((char >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (char & 0b111111))));
    } else if (char < 0x110000) {
        try writer.writeByte(@as(u8, @intCast(0b11110000 | (char >> 18))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((char >> 12) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((char >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (char & 0b111111))));
    } else {
        return error.CharTooLarge;
    }
}

pub inline fn debugChar(char: u21) !void {
    var stderr = std.io.getStdErr();
    try writeCharUtf8(char, stderr.writer());
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
