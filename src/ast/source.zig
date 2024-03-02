pub const Source = struct {
    name: ?[]const u8,
    data: []const u8,
};

pub const Position = struct {
    raw: usize = 0,
    row: usize = 0,
    col: usize = 0,

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

pub const Span = struct { Position = .{}, Position = .{} };
