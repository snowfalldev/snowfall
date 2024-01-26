pub const SourceFile = struct {
    name: ?[]const u8,
    data: @import("unicode.zig").StringUnmanaged,
};

pub const Position = struct {
    raw: usize,
    row: usize,
    col: usize,

    // Returns the position, back N characters.
    // Raw and row must be over 0.
    pub inline fn back(pos: *const Position, n: usize) Position {
        return .{
            .raw = pos.raw - n,
            .row = pos.row,
            .col = pos.col - n,
        };
    }

    // Returns the position, forward N characters.
    pub inline fn forward(pos: *const Position, n: usize) Position {
        return .{
            .raw = pos.raw + n,
            .row = pos.row,
            .col = pos.col + n,
        };
    }
};

pub const Span = struct { Position, Position };
