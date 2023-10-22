const std = @import("std");
const common = @import("common.zig");
const messages = @import("messages.zig");
const lexer = @import("lexer.zig");

const Allocator = std.mem.Allocator;

const getOptional = common.getOptional;

const Token = lexer.Token;
const LocatedToken = lexer.LocatedToken;
const Tokens = lexer.Tokens;

const Position = common.Position;
const Messages = messages.Messages;

// BLOCKS AND STATEMENTS
pub const Block = std.ArrayList(Statement);

pub const Statement = union(enum) {
    function: Function,
    call: Call,
    builtin_call: BuiltinCall,
    constant: Constant,
    variable: Variable,
};

// VALUES AND VARIABLES
pub const Comparator = enum {
    eq,
    ne,
    gt,
    lt,
    gt_eq,
    lt_eq,

    const Self = @This();

    pub inline fn fromDouble(double: []const u8) ?Self {
        if (std.mem.eql(u8, double, "==")) return Self.eq;
        if (std.mem.eql(u8, double, "!=")) return Self.ne;
        if (std.mem.eql(u8, double, ">=")) return Self.gt_eq;
        if (std.mem.eql(u8, double, "<=")) return Self.lt_eq;
        return null;
    }

    pub inline fn fromSymbol(symbol: u8) ?Self {
        return if (symbol == '>') Self.gt else if (symbol == '<') Self.lt else null;
    }
};

pub const LinkOperator = enum {
    l_and,
    l_or,

    const Self = @This();

    pub inline fn fromDouble(symbol: u8) ?Self {
        if (symbol == '&') return Self.l_and;
        if (symbol == '|') return Self.l_or;
        return null;
    }
};

const operator_symbols = [_]u8{ '&', '|', '!', '~', '^', '*', '/', '%', '+', '-' };

pub const Operator = enum {
    sum,
    sub,
    div,
    mul,
    mod,
    bw_ls,
    bw_rs,
    bw_and,
    bw_or,
    bw_not,
    l_not,
    xor,

    const Self = @This();

    pub inline fn fromDouble(symbol: u8) ?Self {
        if (symbol == '<') return Self.bw_ls;
        if (symbol == '>') return Self.bw_rs;
        return null;
    }

    pub inline fn fromSymbol(symbol: u8) ?Self {
        return switch (symbol) {
            '+' => Self.sum,
            '-' => Self.sub,
            '/' => Self.div,
            '*' => Self.mul,
            '%' => Self.mod,
            '&' => Self.bw_and,
            '|' => Self.bw_or,
            '~' => Self.bw_not,
            '!' => Self.l_not,
            '^' => Self.xor,
            else => null,
        };
    }
};

// EXPRESSIONS
pub const LiteralExpr = union(enum) {
    false,
    true,
    null,
    undefined,
    char: u8,
    string: []const u8,
    uint: u128,
    int: i128,
    float: f128,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.value => |value| {
                if (std.mem.eql(u8, value, "false")) return Self.false;
                if (std.mem.eql(u8, value, "true")) return Self.true;
                if (std.mem.eql(u8, value, "null")) return Self.null;
                if (std.mem.eql(u8, value, "undefined")) return Self.undefined;
                unreachable;
            },
            Token.char => |char| .{ .char = char },
            Token.string => |string| .{ .string = string },
            Token.uint => |uint| .{ .uint = uint },
            Token.int => |int| .{ .int = int },
            Token.float => |float| .{ .float = float },
            else => null,
        };
    }
};

// todo: ermm
pub const ImmediateExpr = union(enum) {
    identifier: []const u8,
    field_access: FieldAccess,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return if (token == Token.identifier) .{ .identifier = token.identifier } else null;
    }
};

pub const NonLiteralExpr = union(enum) {
    identifier: []const u8,
    field_access: FieldAccess,
    call: Call,
    builtin_call: BuiltinCall,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return if (token == Token.identifier) .{ .identifier = token.identifier } else null;
    }
};

pub const BasicExpression = union(enum) {
    literal: LiteralExpr,
    non_literal: NonLiteralExpr,

    const Self = @This();

    pub fn fromToken(token: Token) ?Self {
        if (LiteralExpr.fromToken(token)) |e| {
            return .{ .literal = e };
        } else if (NonLiteralExpr.fromToken(token)) |e| {
            return .{ .non_literal = e };
        }

        return null;
    }
};

pub const ExpressionChainItem = union(enum) {
    basic: BasicExpression,
    comparator: Comparator,
    link_operator: LinkOperator,
    operator: Operator,
};

pub const ExpressionChain = std.ArrayList(ExpressionChainItem);

// TYPES
pub const PointerType = struct {
    constant: bool = false,
    slice: bool = false,
    expression: NonLiteralExpr = undefined,
};

pub const ArrayType = struct {
    size: usize = 0,
    expression: NonLiteralExpr = undefined,
};

pub const Type = union(enum) {
    single: NonLiteralExpr,
    ptr: PointerType,
    array: ArrayType,
};

// CONSTANT DECLARATION
pub const Constant = struct {
    name: []const u8 = "",
    typ: Type = undefined,
    val: ExpressionChain = undefined,
    public: bool = false,
};

// VARIABLE DECLARATION
pub const Variable = struct {
    name: []const u8 = "",
    typ: ?Type = null,
    val: ?ExpressionChain = null,
    mutable: bool = false,
    public: bool = false,
};

// VARIABLE ASSIGNMENT
pub const Assignment = struct {
    variable: ImmediateExpr,
    operation: ?Operator,
    expr: ExpressionChain,
};

// FUNCTIONS
pub const FunctionArgument = struct {
    name: []const u8 = "",
    typ: Type = undefined,
};

pub const Function = struct {
    name: []const u8,
    args: std.MultiArrayList(FunctionArgument),
    body: Block,
    public: bool,
};

pub const Call = struct {
    expr: *BasicExpression = undefined,
    args: std.MultiArrayList(ExpressionChain) = undefined,
};

pub const BuiltinCall = struct {
    name: []const u8 = "",
    args: std.MultiArrayList(ExpressionChain) = undefined,
};

// OBJECTS
pub const FieldAccess = struct {
    object: *BasicExpression = undefined,
    fields: [][]const u8 = &[_][]const u8{},
};

pub const Field = struct {
    name: []const u8 = "",
    typ: Type = undefined,
    default: ?*BasicExpression = null,
};

pub const Struct = struct {
    name: []const u8,
    fields: std.MultiArrayList(Field),
    methods: std.MultiArrayList(Function),
    public: bool,
};

pub const StatementContext = union(enum) {
    chain: ExpressionChain,
    basic: BasicExpression,
    constant: Constant,
    variable: Variable,
    assignment: Assignment,
    function: Function,
    field_access: FieldAccess,
    typ: ?Type,
    public,

    const Self = @This();
};

pub const StatementContexts = std.ArrayList(StatementContext);

pub const GlobalContext = union(enum) {
    struct_definition,
    function_block,

    const Self = @This();
};

pub const GlobalContexts = std.ArrayList(GlobalContext);

pub const Parser = struct {
    // Stores the allocator used by the parser.
    allocator: Allocator,

    // Stores the output from the parser.
    output: Block,

    // Stores the tokens output by the lexer.
    tokens: Tokens,

    // Stores the current parser position.
    pos: usize = 0,

    // Stores the current parser statement contexts.
    statement_contexts: StatementContexts,

    // Stores the current parser global contexts.
    global_contexts: GlobalContexts,

    // Stores the message handler used by the parser.
    messages: Messages,

    const Self = @This();

    // Initializes a new parser.
    pub inline fn init(allocator: Allocator, tokens: Tokens, name: ?[]const u8, data: ?[]const u8) Self {
        return .{
            .allocator = allocator,
            .output = Block.init(allocator),
            .tokens = tokens,
            //.buffer = Tokens.init(allocator),
            .statement_contexts = StatementContexts.init(allocator),
            .global_contexts = GlobalContexts.init(allocator),
            .messages = Messages.init(allocator, name, data),
        };
    }

    // De-initializes the parser.
    // This should only be run after the output of the parser is done being used.
    pub inline fn deinit(self: *Self) void {
        self.output.deinit();
        //self.buffer.deinit();
        self.statement_contexts.deinit();
        self.global_contexts.deinit();
        self.messages.deinit();
    }

    // Get a pointer to the current context of the parser.
    pub inline fn getStatementContextPtr(self: *Self, back: usize) ?*StatementContext {
        if (self.statement_contexts.items.len == back) return null;
        return &self.statement_contexts.items[self.statement_contexts.items.len - (1 + back)];
    }

    // Parse all tokens.
    pub inline fn parseFull(self: *Self) !Block {
        while (self.pos < self.tokens.items.len) {
            try self.parseStatement();
        }
        return self.output;
    }

    inline fn fatal(self: *Self, comptime str: []const u8) !void {
        var token = self.tokens.items[self.pos];
        try self.messages.printFatal(str, .{}, token.start, token.end);
    }

    inline fn isAssignment(self: *Self) bool {
        if (self.pos + 1 >= self.tokens.items.len) return false;
        var next = self.tokens.items[self.pos + 1].token;
        if (next == Token.symbol and next.symbol == '=') return true;

        if (self.pos + 2 >= self.tokens.items.len) return false;
        var next2 = self.tokens.items[self.pos + 2].token;
        if ((next == Token.symbol and common.containsChar(@constCast(&operator_symbols), next.symbol)) and (next2 == Token.symbol and next2.symbol == '=')) return true;

        if (self.pos + 3 >= self.tokens.items.len) return false;
        var next3 = self.tokens.items[self.pos + 3].token;
        if ((next == Token.symbol and (next.symbol == '>' or next.symbol == '<')) and (next2 == Token.symbol and next2.symbol == next.symbol) and (next3 == Token.symbol and next3.symbol == '=')) return true;
        return false;
    }

    // Parse tokens into the next statement.
    pub fn parseStatement(self: *Self) !void {
        var initial_len = self.output.items.len;

        while (self.pos < self.tokens.items.len and initial_len == self.output.items.len) : (self.pos += 1) {
            var token = self.tokens.items[self.pos];

            switch (token.token) {
                Token.value, Token.char, Token.string, Token.uint, Token.int, Token.float => {
                    var ctx = self.getStatementContextPtr(0) orelse return self.fatal("literal with no ctx.");
                    if (ctx.* == StatementContext.basic) {
                        ctx.*.basic = BasicExpression.fromToken(token.token).?;
                    } else if (ctx.* != StatementContext.chain) {
                        try ctx.*.chain.append(.{ .basic = BasicExpression.fromToken(token.token).? });
                    } else {
                        try self.fatal("invalid syntax.");
                    }
                },
                Token.keyword => |keyword| if (std.mem.eql(u8, "pub", keyword)) {
                    try self.statement_contexts.append(StatementContext.public);
                } else if (std.mem.eql(u8, "const", keyword)) {
                    var public: bool = false;

                    if (self.getStatementContextPtr(0)) |ctx| if (ctx.* == StatementContext.public) {
                        public = true;
                        _ = self.statement_contexts.orderedRemove(0);
                    } else if (ctx.* == StatementContext.typ and ctx.*.typ != null and ctx.*.typ.? == Type.ptr) {
                        ctx.*.typ.?.ptr.constant = true;
                        continue;
                    } else {
                        try self.fatal("invalid syntax.");
                    };

                    try self.statement_contexts.append(.{ .constant = .{ .public = public } });
                } else if (std.mem.eql(u8, "let", keyword)) {
                    var public: bool = false;

                    if (self.getStatementContextPtr(0)) |ctx| if (ctx.* == StatementContext.public) {
                        public = true;
                        _ = self.statement_contexts.orderedRemove(0);
                    } else {
                        try self.fatal("invalid syntax.");
                    };

                    try self.statement_contexts.append(.{ .variable = .{ .public = public } });
                } else if (std.mem.eql(u8, "mut", keyword)) {
                    var ctx = self.getStatementContextPtr(0) orelse return self.fatal("invalid syntax.");

                    if (ctx.* == StatementContext.variable) {
                        ctx.variable.mutable = true;
                    } else {
                        try self.fatal("invalid syntax.");
                    }
                } else {
                    try self.messages.printFatal("unknown keyword.", .{}, token.start, token.end);
                },
                Token.symbol => |symbol| switch (symbol) {
                    '&', '|', '!', '~', '^', '*', '/', '%', '+', '-' => {
                        var ctx = self.getStatementContextPtr(0) orelse return self.fatal("invalid syntax.");
                        if (ctx.* != StatementContext.chain) try self.fatal("invalid syntax.");
                        if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                        var next = self.tokens.items[self.pos + 1];

                        if (next.token == Token.symbol) {
                            if (next.token.symbol == symbol and (symbol == '&' or symbol == '|')) {
                                try ctx.chain.append(.{ .link_operator = LinkOperator.fromDouble(symbol).? });
                            } else if (next.token.symbol == '=') {
                                if (symbol == '!') {}
                            }
                        } else {
                            try ctx.chain.append(.{ .operator = Operator.fromSymbol(symbol).? });
                        }
                    },
                    '>' => {},
                    '<' => {},
                    '=' => {
                        var ctx = self.getStatementContextPtr(0) orelse return self.fatal("invalid syntax.");
                        if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");

                        var next = self.tokens.items[self.pos + 1].token;
                        var next_eq = next == Token.symbol and next.symbol == '=';

                        if (ctx.* == StatementContext.constant or ctx.* == StatementContext.variable or ctx.* == StatementContext.assignment) {
                            if (next_eq) {
                                self.pos += 1;
                                try self.fatal("invalid syntax.");
                            }

                            try self.statement_contexts.append(.{ .chain = ExpressionChain.init(self.allocator) });
                            continue;
                        }

                        if (ctx.* != StatementContext.chain or !next_eq or self.pos + 2 >= self.tokens.items.len) try self.fatal("invalid syntax.");

                        var next2 = self.tokens.items[self.pos + 2].token;

                        if (next2 == Token.symbol and next2.symbol == '=') {
                            self.pos += 2;
                            try self.fatal("invalid syntax.");
                        }

                        self.pos += 1;
                        try ctx.*.chain.append(.{ .comparator = Comparator.eq });
                    },
                    '?' => {},
                    ':' => {
                        var ctx = self.getStatementContextPtr(0) orelse return self.fatal("invalid syntax.");
                        switch (ctx.*) {
                            StatementContext.constant, StatementContext.variable => try self.statement_contexts.append(.{ .chain = ExpressionChain.init(self.allocator) }),
                            else => try self.fatal("invalid syntax."),
                        }
                    },
                    '[' => {
                        if (self.pos + 1 < self.tokens.items.len) {
                            var next = self.tokens.items[self.pos + 1];
                            if (next.token == Token.symbol and next.token.symbol == ']') {
                                self.pos += 1;
                            } else {
                                try self.fatal("invalid syntax.");
                            }
                        } else {
                            try self.fatal("invalid syntax.");
                        }
                        try self.statement_contexts.append(.{ .typ = .{ .ptr = .{ .slice = true } } });
                    },
                    ']' => try self.fatal("invalid syntax."),
                    ';' => {
                        var ctx = self.getStatementContextPtr(0) orelse return self.fatal("invalid syntax.");

                        switch (ctx.*) {
                            StatementContext.chain => {
                                if (self.getStatementContextPtr(1)) |parent| switch (parent.*) {
                                    StatementContext.constant => {
                                        parent.*.constant.val = ctx.*.chain;
                                        try self.output.append(Statement{ .constant = parent.*.constant });
                                    },
                                    StatementContext.variable => {
                                        parent.*.variable.val = ctx.*.chain;
                                        try self.output.append(Statement{ .variable = parent.*.variable });
                                    },
                                    StatementContext.typ => {
                                        if (self.getStatementContextPtr(2)) |parent2| {
                                            if (parent2.* != StatementContext.variable) {
                                                try self.fatal("invalid syntax.");
                                            }
                                        }
                                    },
                                    else => try self.fatal("invalid syntax."),
                                };
                            },
                            else => try self.fatal("invalid syntax."),
                        }

                        self.statement_contexts.clearRetainingCapacity();
                        self.pos += 1;

                        std.debug.print("\n\nOUTPUT: {any}\n\n", .{self.output.items});
                        break;
                    },
                    else => try self.fatal("invalid syntax."),
                },
                Token.identifier => |identifier| {
                    var ctxn = self.getStatementContextPtr(0);

                    if (ctxn == null or ctxn.?.* == StatementContext.field_access) {
                        if (self.isAssignment()) {} else {
                            try self.fatal("invalid syntax.");
                        }
                        continue;
                    }

                    var ctx = ctxn orelse unreachable;

                    if (ctx.* == StatementContext.constant and ctx.*.constant.name.len == 0) {
                        ctx.*.constant.name = identifier;
                    } else if (ctx.* == StatementContext.variable and ctx.*.variable.name.len == 0) {
                        ctx.*.variable.name = identifier;
                    } else if (ctx.* == StatementContext.chain) {
                        try ctx.*.chain.append(.{ .basic = .{ .non_literal = .{ .identifier = identifier } } });
                        //if (self.getStatementContextPtr(1)) |parent| {
                        //    if (parent.* == StatementContext.typ) {}
                        //}
                    } else { // TODO
                        try self.fatal("invalid syntax.");
                    }
                },
            }

            std.debug.print("{any}\n", .{self.statement_contexts.items});
        }
    }
};
