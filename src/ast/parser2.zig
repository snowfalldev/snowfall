const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const messages = @import("messages.zig");

const Allocator = std.mem.Allocator;

const Tree = ast.Tree;
const Node = ast.Node;

const Token = lexer.Token;
const Tokens = lexer.Tokens;

const Messages = messages.Messages;

pub const Context = struct {
    tree: Tree,
    buffer: Tokens,
    goal: ?Node,
};

pub const Parser = struct {
    allocator: Allocator,
    tokens: Tokens,
    pos: usize,
    contexts: std.ArrayList(Context),
    messages: Messages,

    const Self = @This();

    pub fn init(allocator: Allocator, tokens: Tokens, name: ?[]const u8, data: ?[]const u8) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .buffer = Tokens.init(),
            .messages = Messages.init(allocator, name, data),
        };
    }

    inline fn fatal(self: *Self, comptime str: []const u8) !void {
        var token = self.tokens.items[self.pos];
        try self.messages.printFatal(str, .{}, token.start, token.end);
    }

    pub fn parseFull(self: *Self) !Tree {
        _ = self;
    }

    pub fn parseToken(self: *Self) !void {
        var token = self.tokens.items[self.pos];

        switch (token) {
            Token.value, Token.char, Token.string, Token.uint, Token.int, Token.float => {
                var ctx = self.getContextPtr(0) orelse return self.fatal("literal with no context.");

                if (ctx.* == Context.chain) {
                    try ctx.*.chain.append(.{ .basic = BasicExpression.fromToken(token.token).? });
                } else {
                    try self.fatal("invalid syntax.");
                }
            },
        }

        self.pos += 1;
    }
};

test {}
