const builtin = @import("builtin");

const Engine = @import("Engine.zig");

const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const ast = @import("ast.zig");
pub const Lexer = ast.Lexer;

const Self = @This();

const DataStorage = struct {
    buf: []const u8,
    managed: bool,

    pub inline fn fromUtf8(utf8: []const u8, allocator: Allocator) !DataStorage {
        if (unicode.utf8ValidateSlice(utf8))
            return .{ .buf = utf8, .managed = false };
        if (builtin.os.tag == .windows and unicode.wtf8ValidateSlice(utf8))
            return .{ .buf = try unicode.wtf8ToUtf8LossyAlloc(allocator, utf8), .managed = true };
        return error.InvalidUtf8;
    }

    pub inline fn deinit(self: DataStorage, allocator: Allocator) void {
        if (self.managed) allocator.free(self.buf);
    }
};

arena: *Arena,
engine: *Engine,

data: DataStorage,
name: DataStorage,
path: ?[]const u8,

deps: std.ArrayList(*Self),

lexer: Lexer,

inline fn initInner(engine: *Engine, name: []const u8, src: []const u8, arena: *Arena) !*Self {
    const allocator = arena.allocator();
    var mod = try allocator.create(Self);
    mod.* = .{
        .arena = arena,
        .engine = engine,

        .data = try DataStorage.fromUtf8(src, allocator),
        .name = try DataStorage.fromUtf8(name, allocator),
        .path = null,

        .deps = std.ArrayList(*Self).init(allocator),
        .lexer = undefined,
    };

    mod.lexer = try Lexer.init(mod);

    return mod;
}

pub fn init(engine: *Engine, name: []const u8, src: []const u8) !*Self {
    const arena = try engine.allocator.create(Arena);
    arena.* = Arena.init(engine.allocator);
    return Self.initInner(engine, name, src, arena);
}

pub fn initFile(engine: *Engine, path: []const u8) !*Self {
    const arena = try engine.allocator.create(Arena);
    arena.* = Arena.init(engine.allocator);
    const allocator = arena.allocator();

    var file: std.fs.File = undefined;
    if (!std.fs.path.isAbsolute(path)) {
        const cwd = std.fs.cwd();
        file = cwd.openFile(path, .{});
    } else file = std.fs.openFileAbsolute(path, .{});

    const data = file.readToEndAlloc(allocator, std.math.maxInt(usize));

    const self = try Self.initInner(engine, path, data, arena);
    self.data.managed = true;
    return self;
}

pub inline fn deinit(self: *Self) void {
    const arena = self.arena;
    const allocator = arena.child_allocator;
    arena.deinit();
    allocator.destroy(arena);
}

pub inline fn finishLexer(self: *Self) !*std.ArrayList(ast.Lexer.LocatedToken) {
    try self.lexer.finish();
    return &self.lexer.output;
}
