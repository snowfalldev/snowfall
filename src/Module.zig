const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const ast = @import("ast.zig");
pub const Lexer = ast.Lexer;

const utftools = @import("utftools");

const Self = @This();

const DataStorage = struct {
    buf: []const u8,
    managed: bool,

    pub inline fn deinit(self: DataStorage, allocator: Allocator) void {
        if (self.managed) allocator.free(self.buf);
    }
};

inline fn convertMaybeWtf8(utf8: []const u8, allocator: Allocator) !?DataStorage {
    if (unicode.utf8ValidateSlice(utf8))
        return .{ .buf = utf8, .managed = false };
    if (unicode.wtf8ValidateSlice(utf8))
        return .{ .buf = try unicode.wtf8ToUtf8LossyAlloc(allocator, utf8), .managed = true };
    return null;
}

pub fn convertDataToUtf8(data: Source, allocator: Allocator) !DataStorage {
    return switch (data) {
        .utf8 => |v| try convertMaybeWtf8(v, allocator) orelse error.InvalidUtf8,
        .utf16le => |v| utf16le: {
            const wtf8 = unicode.wtf16LeToWtf8Alloc(allocator, v) catch |e| {
                if (e == Allocator.Error.OutOfMemory) return e;
                return error.InvalidUtf16Le;
            };

            const converted = (try convertMaybeWtf8(wtf8, allocator)) orelse return error.InvalidUtf16Le;
            if (converted.managed) allocator.free(wtf8);

            break :utf16le .{ .buf = converted.buf, .managed = true };
        },
        .utf32le => |v| utf32le: {
            var utf8 = try std.ArrayList(u8).initCapacity(allocator, v.len);
            const writer = utf8.writer();
            for (v) |i| utftools.writeCodepointToUtf8(@truncate(i), writer) catch |e| {
                if (e == Allocator.Error.OutOfMemory) return e;
                return error.InvalidUtf32Le;
            };
            break :utf32le .{ .buf = try utf8.toOwnedSlice(), .managed = true };
        },
    };
}

pub const Source = union(enum) {
    utf8: []const u8,
    utf16le: []const u16,
    utf32le: []const u32,
};

arena: *Arena,
allocator: Allocator,

data: DataStorage,
name: DataStorage,
path: ?[]const u8,

deps: std.ArrayList(*Self),

lexer: Lexer,

inline fn initInner(src: Source, name: Source, arena: *Arena) !*Self {
    const allocator = arena.allocator();
    var mod = try allocator.create(Self);
    mod.* = .{
        .arena = arena,
        .allocator = allocator,

        .data = try convertDataToUtf8(src, allocator),
        .name = try convertDataToUtf8(name, allocator),
        .path = null,

        .deps = std.ArrayList(*Self).init(allocator),
        .lexer = undefined,
    };

    mod.lexer = try Lexer.init(mod);

    return mod;
}

pub fn init(src: Source, name: Source, allocator: Allocator) !*Self {
    const arena = try allocator.create(Arena);
    arena.* = Arena.init(allocator);
    return Self.initInner(src, name, arena);
}

pub fn initFile(path: []const u8, allocator: Allocator) !*Self {
    const arena = try allocator.create(Arena);
    arena.* = Arena.init(allocator);
    const alloc = arena.allocator();

    var file: std.fs.File = undefined;
    if (!std.fs.path.isAbsolute(path)) {
        const cwd = std.fs.cwd();
        file = cwd.openFile(path, .{});
    } else file = std.fs.openFileAbsolute(path, .{});

    const data = file.readToEndAlloc(alloc, std.math.maxInt(usize));

    const self = try Self.initInner(.{ .utf8 = data }, .{ .utf8 = path }, arena);
    self.data.managed = true;
    return self;
}

pub inline fn deinit(self: *Self) void {
    const arena = self.arena;
    const allocator = arena.child_allocator;
    arena.deinit();
    allocator.destroy(arena);
}

pub inline fn finishLexer(self: *Self) ![]ast.Lexer.LocatedToken {
    return self.lexer.finish();
}
