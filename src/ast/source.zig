const std = @import("std");
const utftools = @import("utftools");

const unicode = std.unicode;
const Allocator = std.mem.Allocator;

inline fn convertMaybeWtf8(utf8: []const u8, allocator: Allocator) !?struct { []const u8, bool } {
    if (unicode.utf8ValidateSlice(utf8))
        return .{ utf8, false };
    if (unicode.wtf8ValidateSlice(utf8))
        return .{ try unicode.wtf8ToUtf8LossyAlloc(allocator, utf8), true };
    return null;
}

fn convertDataToUtf8(data: SourceData, allocator: Allocator) !struct { []const u8, bool } {
    return switch (data) {
        .utf8 => |v| try convertMaybeWtf8(v, allocator) orelse error.InvalidUtf8,
        .utf16le => |v| utf16le: {
            const wtf8 = unicode.wtf16LeToWtf8Alloc(allocator, v) catch |e| {
                if (e == Allocator.Error.OutOfMemory) return e;
                return error.InvalidUtf16Le;
            };

            const converted = (try convertMaybeWtf8(wtf8, allocator)) orelse return error.InvalidUtf16Le;
            if (converted[1]) allocator.free(wtf8);

            break :utf16le .{ converted[0], true };
        },
        .utf32le => |v| utf32le: {
            var utf8 = try std.ArrayList(u8).initCapacity(allocator, v.len);
            const writer = utf8.writer();
            for (v) |i| utftools.writeCodepointToUtf8(@truncate(i), writer) catch |e| {
                if (e == Allocator.Error.OutOfMemory) return e;
                return error.InvalidUtf32Le;
            };
            break :utf32le .{ try utf8.toOwnedSlice(), true };
        },
    };
}

pub const SourceData = union(enum) {
    utf8: []const u8,
    utf16le: []const u16,
    utf32le: []const u32,
};

pub const Source = struct {
    data: []const u8,
    name: ?[]const u8,
    allocator: Allocator,
    _data_managed: bool,
    _name_managed: bool,

    pub fn init(data: SourceData, name: ?SourceData, allocator: Allocator) !Source {
        const convData = try convertDataToUtf8(data, allocator);
        const convName = if (name) |n| try convertDataToUtf8(n, allocator) else null;

        return .{
            .data = convData[0],
            .name = if (convName) |n| n[0] else null,
            .allocator = allocator,
            ._data_managed = convData[1],
            ._name_managed = if (convName) |n| n[1] else false,
        };
    }

    pub fn deinit(self: Source) void {
        if (self._data_managed) self.allocator.free(self.data);
        if (self.name) |n| if (self._name_managed) self.allocator.free(n);
    }
};

pub const Position = struct {
    raw: u32 = 0,
    row: u32 = 0,
    col: u32 = 0,
};

pub const Span = struct { Position = .{}, Position = .{} };
