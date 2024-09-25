pub const Source = struct {
    name: ?[]const u8,
    data: []const u8,
};

pub const Position = struct {
    raw: u32 = 0,
    row: u32 = 0,
    col: usize = 0,
};

pub const Span = struct { Position = .{}, Position = .{} };
