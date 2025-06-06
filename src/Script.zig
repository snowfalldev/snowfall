const builtin = @import("builtin");

const Engine = @import("Engine.zig");

const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const ast = @import("ast.zig");
const Lexer = ast.Lexer;
const Parser = ast.Parser;

const Self = @This();

engine: *Engine,

name: []const u8,
src: []const u8,

stage: Stage = undefined,
failed: bool = false,

const Stage = union(enum) {
    lexer: *Lexer,
    parser: *Parser,
};

// INIT / DEINIT

pub fn init(engine: *Engine, name: []const u8, src: []const u8) !*Self {
    var script = try engine.allocator.create(Self);

    script.* = .{
        .engine = engine,

        .name = name,
        .src = src,
    };

    script.stage.lexer = try Lexer.init(script);

    return script;
}

pub inline fn deinit(self: *Self) void {
    switch (self.stage) {
        .lexer => |l| l.deinit(),
        .parser => |p| p.deinit(),
    }

    self.engine.allocator.destroy(self);
}

// LEXING / PARSING

pub inline fn tokenize(self: *Self) ![]ast.Lexer.LocatedToken {
    while (!self.stage.lexer.finished()) try self.stage.lexer.next();
    if (self.stage.lexer.logger.errors.items.len > 0) self.failed = true;
    return self.stage.lexer.output.items;
}
