const builtin = @import("builtin");

const Engine = @import("Engine.zig");

const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const ast = @import("ast.zig");
const Lexer = ast.Lexer;
const Parser = ast.Parser;
const RowInfo = ast.RowInfo;

// STRUCTURE

engine: *Engine,

name: []const u8,
src: []const u8,

arena: Arena,

stage: Stage = undefined,
rows: std.ArrayListUnmanaged(RowInfo) = .{},
tokens: []const Lexer.LocatedToken = &.{},
failed: bool = false,

const Stage = union(enum) {
    lexer: *Lexer,
    parser: *Parser,
};

const Self = @This();

// INIT / DEINIT

pub fn init(engine: *Engine, name: []const u8, src: []const u8) !*Self {
    if (src.len > std.math.maxInt(u48)) return error.ScriptTooLarge;

    const script = try engine.allocator.create(Self);
    script.* = .{
        .engine = engine,
        .name = name,
        .src = src,
        .arena = .init(engine.allocator),
    };

    // give rows some initial capacity; 50 bytes per row is conservative (?)
    try script.rows.ensureUnusedCapacity(script.allocator(), src.len / 50);
    try script.rows.append(script.allocator(), .{ .raw = 0, .len = 0 });

    return script;
}

pub inline fn deinit(self: *Self) void {
    self.engine.allocator.destroy(self);
}

// UTILITIES

pub inline fn allocator(self: *Self) Allocator {
    return self.arena.allocator();
}

pub inline fn prepare(self: *Self) !void {
    self.stage = .{ .lexer = try Lexer.init(self) };
    while (!self.stage.lexer.finished())
        self.stage.lexer.next() catch {
            self.failed = true;
            //return error.LexerFailed;
        };

    if (self.stage.lexer.logger.errors.items.len > 0) {
        self.failed = true;
        //return error.LexerFailed;
    }

    self.tokens = try self.stage.lexer.output.toOwnedSlice(self.allocator());
    self.engine.allocator.destroy(self.stage.lexer);
    self.rows.shrinkAndFree(self.allocator(), self.rows.items.len);
}

// ROW TOOLS

const compareFn = struct {
    pub fn compare(byte: usize, row: RowInfo) std.math.Order {
        if (byte < row.raw) return .lt;
        const end = row.raw + row.len;
        if (byte > end) return .gt;
        return .eq;
    }
}.compare;

pub fn findRow(self: Self, byte: usize) !usize {
    if (byte > self.src.len) return error.OutOfBounds;
    return std.sort.binarySearch(RowInfo, self.rows.items, byte, compareFn).?;
}
