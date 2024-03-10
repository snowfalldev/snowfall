const std = @import("std");

mode: enum { rv32, rv64, rv128 } = .rv64,
embedded: bool = false,
features: std.EnumSet(Feature) = std.EnumSet(Feature).initEmpty(),

pub const Feature = enum {
    integer,
    multi,
    atomic,
    float,
    double,
    quad,
    comp,
    vector,
};

const Self = @This();

pub fn fromString(string: []const u8) !Self {
    if (string.len < 5) return error.NoBase; // must be rv__(i/e) or rv128
    var out = Self{};

    if (string[0] != 'r' and string[1] != 'v') return error.NotRISCV;

    var i: usize = 5;

    if (string[3] == '2') {
        if (string[2] == '1' and string[4] == '8') {
            if (string.len < 6) return error.NoBase; // must be rv128(i/e)
            i = 6;
            out.mode = .rv128;
        } else if (string[2] == '3') {
            out.mode = .rv32;
        }
    } else if (string[2] != '6' and string[3] != '4') {
        return error.InvalidBits;
    }

    while (i < string.len) : (i += 1) {
        const feature = switch (string[i]) {
            'e' => blk: {
                if (out.features.contains(.integer)) return error.EmbeddedInInteger;
                out.embedded = true;
                break :blk Feature.integer;
            },
            'i' => if (out.embedded) {
                return error.IntegerInEmbedded;
            } else Feature.integer,
            'm' => Feature.multi,
            else => return error.InvalidFeature,
        };

        out.features.insert(feature);
    }
}
