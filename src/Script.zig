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

lexer: Lexer,
parser: Parser,

// INIT / DEINIT

pub fn init(engine: *Engine, name: []const u8, src: []const u8) !*Self {
    var script = try engine.allocator.create(Self);

    script.* = .{
        .engine = engine,

        .name = name,
        .src = src,

        .lexer = undefined,
        .parser = undefined,
    };

    script.lexer = try Lexer.init(script);

    return script;
}

pub inline fn deinit(self: *Self) void {
    self.lexer.deinit();
    self.parser.deinit();
    self.engine.allocator.destroy(self);
}

// LEXING / PARSING

pub inline fn finishLexer(self: *Self) !*std.ArrayList(ast.Lexer.LocatedToken) {
    while (!self.lexer.finished()) try self.lexer.next();
    return &self.lexer.output;
}
