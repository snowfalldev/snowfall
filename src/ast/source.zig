pub const Source = struct {
    name: ?[]const u8,
    data: []const u8,
};

pub const Position = struct {
    raw: usize = 0,
    row: u32 = 0,
    col: u32 = 0,

    // Returns the position, forward N characters.
    pub inline fn forward(pos: Position, n: usize) Position {
        return .{
            .raw = pos.raw + n,
            .row = pos.row,
            .col = pos.col + @as(u32, @intCast(n)),
        };
    }
};

pub const Span = struct { Position = .{}, Position = .{} };
