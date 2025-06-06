//! A few things from zig-utftools and some codepoint categorization.

const std = @import("std");

// I/O TOOLS

// based on std.unicode.{utf8ByteSequenceLength, utf8Decode}
pub fn codepointFromUtf8(in: []const u8) !struct { u21, usize } {
    var codepoint: u21 = 0;
    var length: usize = 1;

    switch (in[0]) {
        0b0000_0000...0b0111_1111 => {
            codepoint = @as(u21, in[0]);
        },
        0b1100_0000...0b1101_1111 => {
            if (in.len < 2) return error.InvalidCodepoint;
            codepoint = try std.unicode.utf8Decode2(in[0..2].*);
            length = 2;
        },
        0b1110_0000...0b1110_1111 => {
            if (in.len < 3) return error.InvalidCodepoint;
            codepoint = try std.unicode.utf8Decode3(in[0..3].*);
            length = 3;
        },
        0b1111_0000...0b1111_0111 => {
            if (in.len < 4) return error.InvalidCodepoint;
            codepoint = try std.unicode.utf8Decode4(in[0..4].*);
            length = 4;
        },
        else => return error.InvalidStartByte,
    }

    return .{ codepoint, length };
}

// based on std.unicode.{utf8CodepointSequenceLength, utf8Encode}
pub fn writeCodepointToUtf8(codepoint: u21, writer: anytype) !void {
    if (codepoint < 0x80) {
        try writer.writeByte(@as(u8, @intCast(codepoint)));
    } else if (codepoint < 0x800) {
        try writer.writeByte(@as(u8, @intCast(0b11000000 | (codepoint >> 6))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (codepoint & 0b111111))));
    } else if (codepoint < 0x10000) {
        if (0xd800 <= codepoint and codepoint <= 0xdfff) return error.CannotEncodeSurrogateHalf;
        try writer.writeByte(@as(u8, @intCast(0b11100000 | (codepoint >> 12))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((codepoint >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (codepoint & 0b111111))));
    } else if (codepoint < 0x110000) {
        try writer.writeByte(@as(u8, @intCast(0b11110000 | (codepoint >> 18))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((codepoint >> 12) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | ((codepoint >> 6) & 0b111111))));
        try writer.writeByte(@as(u8, @intCast(0b10000000 | (codepoint & 0b111111))));
    } else {
        return error.CodepointTooLarge;
    }
}

pub inline fn debugChar(char: u21) !void {
    var stderr = std.io.getStdErr();
    try writeCodepointToUtf8(char, stderr.writer());
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
