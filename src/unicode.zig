const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Codepoint = packed struct {
    codepoint: u21,

    const Self = @This();

    // based on std.unicode.{utf8ByteSequenceLength, utf8Decode}
    pub fn fromUtf8(utf8: []const u8) !struct { Self, usize } {
        var codepoint = Self{ .codepoint = 0 };
        var length: usize = 1;

        switch (utf8[0]) {
            0b0000_0000...0b0111_1111 => {
                codepoint.codepoint = @as(u21, utf8[0]);
            },
            0b1100_0000...0b1101_1111 => {
                codepoint.codepoint = try std.unicode.utf8Decode2(utf8[0..2]);
                length = 2;
            },
            0b1110_0000...0b1110_1111 => {
                codepoint.codepoint = try std.unicode.utf8Decode3(utf8[0..3]);
                length = 3;
            },
            0b1111_0000...0b1111_0111 => {
                codepoint.codepoint = try std.unicode.utf8Decode4(utf8[0..4]);
                length = 4;
            },
            else => return error.InvalidStartByte,
        }

        return .{ codepoint, length };
    }

    // based on std.unicode.{utf8CodepointSequenceLength, utf8Encode}
    pub fn appendToUtf8(self: *const Self, out: *std.ArrayList(u8)) !void {
        if (self.codepoint < 0x80) {
            try out.append(@intCast(self.codepoint));
        } else if (self.codepoint < 0x800) {
            try out.ensureUnusedCapacity(2);
            out.appendAssumeCapacity(@intCast(0b11000000 | (self.codepoint >> 6)));
            out.appendAssumeCapacity(@intCast(0b10000000 | (self.codepoint & 0b111111)));
        } else if (self.codepoint < 0x10000) {
            if (0xd800 <= self.codepoint and self.codepoint <= 0xdfff) return error.CannotEncodeSurrogateHalf;
            try out.ensureUnusedCapacity(3);
            out.appendAssumeCapacity(@intCast(0b11100000 | (self.codepoint >> 12)));
            out.appendAssumeCapacity(@intCast(0b10000000 | ((self.codepoint >> 6) & 0b111111)));
            out.appendAssumeCapacity(@intCast(0b10000000 | (self.codepoint & 0b111111)));
        } else if (self.codepoint < 0x110000) {
            try out.ensureUnusedCapacity(4);
            out.appendAssumeCapacity(@intCast(0b11110000 | (self.codepoint >> 18)));
            out.appendAssumeCapacity(@intCast(0b10000000 | ((self.codepoint >> 12) & 0b111111)));
            out.appendAssumeCapacity(@intCast(0b10000000 | ((self.codepoint >> 6) & 0b111111)));
            out.appendAssumeCapacity(@intCast(0b10000000 | (self.codepoint & 0b111111)));
        } else {
            return error.CodepointTooLarge;
        }
    }

    // based on std.unicode.utf16DecodeSurrogatePair + wikipedia
    pub fn fromUtf16le(utf16le: []const u16) !struct { Self, usize } {
        var codepoint = Self{ .codepoint = 0 };
        var length: usize = 1;

        switch (utf16le[0]) {
            0x0000...0xD7FF, 0xE000...0xFFFF => {
                codepoint.codepoint = @as(u21, utf16le[0]);
            },
            else => if (utf16le[0] & ~@as(u16, 0x03ff) == 0xd800) {
                if (utf16le[1] & ~@as(u16, 0x03ff) != 0xdc00) return error.ExpectedSecondSurrogateHalf;
                codepoint.codepoint = 0x10000 + ((@as(u21, utf16le[0]) & 0x03ff) << 10) | (utf16le[1] & 0x03ff);
                length = 2;
            } else {
                return error.InvalidStartByte;
            },
        }

        return .{ codepoint, length };
    }

    // based on std.unicode.utf16CodepointSequenceLength + wikipedia
    pub fn appendToUtf16le(self: *const Self, out: *std.ArrayList(u16)) !void {
        if (self.codepoint < 0xFFFF) {
            try out.append(@intCast(self.codepoint));
        } else if (self.codepoint < 0x10FFFF) {
            try out.ensureUnusedCapacity(2);
            const codepoint = self.codepoint - 0x10000;
            out.appendAssumeCapacity(@intCast(0xd800 + (codepoint >> 10)));
            out.appendAssumeCapacity(@intCast(0xdc00 + (codepoint & 0b1111111111)));
        } else {
            return error.CodepointTooLarge;
        }
    }
};

// IMPLEMENTATIONS

inline fn codepointsFromUtf8(allocator: Allocator, utf8: []const u8) !std.ArrayList(Codepoint) {
    var codepoints = try std.ArrayList(Codepoint).initCapacity(allocator, utf8.len);

    var i: usize = 0;
    while (i < utf8.len) {
        const codepoint = try Codepoint.fromUtf8(utf8[i..]);
        codepoints.appendAssumeCapacity(codepoint[0]);
        i += codepoint[1];
    }

    return codepoints;
}

inline fn codepointsToUtf8(allocator: Allocator, codepoints: []const Codepoint) ![]u8 {
    var bytes = try std.ArrayList(u8).initCapacity(allocator, codepoints.len);

    for (codepoints) |codepoint| {
        try codepoint.appendToUtf8(&bytes);
    }

    return try bytes.toOwnedSlice();
}

inline fn codepointsFromUtf16le(allocator: Allocator, utf16le: []const u16) !std.ArrayList(Codepoint) {
    var codepoints = try std.ArrayList(Codepoint).initCapacity(allocator, utf16le.len);

    var i: usize = 0;
    while (i < utf16le.len) {
        const codepoint = try Codepoint.fromUtf16le(utf16le[i..]);
        codepoints.appendAssumeCapacity(codepoint[0]);
        i += codepoint[1];
    }

    return codepoints;
}

inline fn codepointsToUtf16le(allocator: Allocator, codepoints: []const Codepoint) ![]u16 {
    var bytes = try std.ArrayList(u16).initCapacity(allocator, codepoints.len);

    for (codepoints) |codepoint| {
        try codepoint.appendToUtf16le(&bytes);
    }

    return try bytes.toOwnedSlice();
}

// STRING TYPES

pub const MutableString = struct {
    codepoints: std.ArrayList(Codepoint),

    const Self = @This();

    pub inline fn to_static(self: Self) !StaticString {
        return .{
            .codepoints = try self.codepoints.toOwnedSlice(),
        };
    }

    pub fn fromUtf8(allocator: Allocator, utf8: []const u8) !Self {
        return .{
            .codepoints = try codepointsFromUtf8(allocator, utf8),
        };
    }

    pub fn toUtf8(self: *const Self) ![]u8 {
        return codepointsToUtf8(self.codepoints.allocator, self.codepoints.items);
    }

    pub fn fromUtf16le(allocator: Allocator, utf16le: []const u16) !Self {
        return .{
            .codepoints = try codepointsFromUtf16le(allocator, utf16le),
        };
    }

    pub fn toUtf16le(self: *const Self) ![]u16 {
        return codepointsToUtf16le(self.codepoints.allocator, self.codepoints.items);
    }
};

pub const StaticString = struct {
    codepoints: []const Codepoint,

    const Self = @This();

    pub inline fn to_mutable(self: Self, allocator: Allocator) MutableString {
        return .{
            .codepoints = std.ArrayList(Codepoint).fromOwnedSlice(allocator, self.codepoints),
        };
    }

    pub fn fromUtf8(allocator: Allocator, utf8: []const u8) !Self {
        return .{
            .codepoints = (try codepointsFromUtf8(allocator, utf8)).items,
        };
    }

    pub fn toUtf8(self: *const Self, allocator: Allocator) ![]u8 {
        return codepointsToUtf8(allocator, self.codepoints);
    }

    pub fn fromUtf16le(allocator: Allocator, utf16le: []const u16) !Self {
        return .{
            .codepoints = (try codepointsFromUtf16le(allocator, utf16le)).items,
        };
    }

    pub fn toUtf16le(self: *const Self, allocator: Allocator) ![]u16 {
        return codepointsToUtf16le(allocator, self.codepoints);
    }
};

// Pattern_White_Space
pub inline fn pattern_white_space(codepoint: Codepoint) bool {
    return switch (codepoint.codepoint) {
        0x0009...0x000D => true,
        0x0020 => true,
        0x0085 => true,
        0x200E...0x200F => true,
        0x2028 => true,
        0x2029 => true,
        else => false,
    };
}

// Pattern_Syntax
pub inline fn pattern_syntax(codepoint: Codepoint) bool {
    return switch (codepoint.codepoint) {};
}
