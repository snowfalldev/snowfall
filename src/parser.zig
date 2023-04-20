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
    function_call: FunctionCall,
    constant: Constant,
    variable: Variable,
};

// VALUES AND VARIABLES
pub const Operation = enum {
    sum,
    sub,
    div,
    mul,
    mod,
    bw_and,
    bw_or,
    bw_not,
    l_and,
    l_or,
    l_not,
    xor,

    const Self = @This();

    pub fn fromKeyword(keyword: []const u8) ?Self {
        if (std.mem.eql(u8, keyword, "and")) return Self.l_and;
        if (std.mem.eql(u8, keyword, "or")) return Self.l_or;
        if (std.mem.eql(u8, keyword, "not")) return Self.l_not;
        return null;
    }

    pub fn fromSymbol(symbol: u8) ?Self {
        return switch (symbol) {
            '+' => Self.sum,
            '-' => Self.sub,
            '/' => Self.div,
            '*' => Self.mul,
            '%' => Self.mod,
            '&' => Self.bw_and,
            '|' => Self.bw_or,
            '~' => Self.bw_not,
            '^' => Self.xor,
            else => null,
        };
    }
};

pub const ExpressionElement = union(enum) {
    identifier: []const u8,
    uint: u128,
    int: i128,
    float: f128,
    function_call: FunctionCall,
    field_access: FieldAccess,
    method_call: MethodCall,

    const Self = @This();

    pub fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.identifier => |identifier| .{ .identifier = identifier },
            Token.uint => |uint| .{ .uint = uint },
            Token.int => |int| .{ .int = int },
            Token.float => |float| .{ .float = float },
            else => null,
        };
    }
};

pub const ExpressionChainItem = union(enum) {
    expression_element: ExpressionElement,
    operation: Operation,

    const Self = @This();

    pub fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.identifier, Token.uint, Token.int, Token.float => .{ .expression_element = ExpressionElement.fromToken(token) orelse return null },
            Token.keyword => |keyword| .{ .operation = Operation.fromKeyword(keyword) orelse return null },
            Token.symbol => |symbol| .{ .operation = Operation.fromSymbol(symbol) orelse return null },
            else => null,
        };
    }
};

pub const ExpressionChain = std.ArrayList(ExpressionChainItem);

pub const ExpressionValue = union(enum) {
    false,
    true,
    null,
    undefined,
    char: u8,
    string: []const u8,

    const Self = @This();

    pub fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.value => |value| if (std.mem.eql(u8, value, "false")) {
                return Self.false;
            } else if (std.mem.eql(u8, value, "true")) {
                return Self.true;
            } else if (std.mem.eql(u8, value, "null")) {
                return Self.null;
            } else if (std.mem.eql(u8, value, "undefined")) {
                return Self.undefined;
            } else {
                unreachable;
            },
            Token.char => |char| .{ .char = char },
            Token.string => |string| .{ .string = string },
            else => null,
        };
    }
};

pub const Expression = union(enum) {
    value: ExpressionValue, // Constant value.
    chain: ExpressionChain, // Chain of elements held together by operators.

    const Self = @This();

    pub fn fromToken(allocator: Allocator, token: Token) !?Self {
        return switch (token) {
            Token.value, Token.char, Token.string => .{ .value = ExpressionValue.fromToken(token) orelse return null },
            else => blk: {
                var chain = try ExpressionChain.initCapacity(allocator, 1);
                chain.appendAssumeCapacity(ExpressionChainItem.fromToken(token) orelse return null);
                break :blk .{ .chain = chain };
            },
        };
    }
};

pub const TypeExpression = union(enum) {
    single: Expression,
    array: struct { ?usize, Expression },
};

pub const Constant = struct {
    name: []const u8 = "",
    typ: TypeExpression = undefined,
    val: Expression = undefined,
    public: bool = false,
};

pub const Variable = struct {
    name: []const u8 = "",
    typ: ?TypeExpression = null,
    val: ?Expression = null,
    mutable: bool = false,
    public: bool = false,
};

pub const FunctionArgument = struct {
    name: []const u8 = "",
    typ: TypeExpression = undefined,
};

// FUNCTIONS
pub const Function = struct {
    name: []const u8,
    args: std.ArrayList(FunctionArgument),
    body: Block,
    public: bool,
};

pub const FunctionCall = struct {
    name: []const u8 = "",
    args: std.ArrayList(Expression) = undefined,
};

// OBJECTS
pub const FieldAccess = struct {
    object: Expression = undefined,
    field: []const u8 = "",
};

pub const MethodCall = struct {
    object: Expression = undefined,
    call: FunctionCall = undefined,
};

pub const Field = struct {
    name: []const u8,
    typ: Expression,
    default: ?Expression,
};

pub const Struct = struct {
    name: []const u8,
    fields: std.ArrayList(Field),
    methods: std.ArrayList(Function),
    public: bool,
};

pub const StatementContext = union(enum) {
    expression: Expression,
    constant: Constant,
    variable: Variable,
    function: Function,
    type_expr,
    public,

    const Self = @This();
};

pub const StatementContexts = std.ArrayList(StatementContext);

pub const GlobalContext = union(enum) {
    struct_definition,

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
    pub fn init(allocator: Allocator, tokens: Tokens, name: ?[]const u8, data: ?[]const u8) Self {
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
    pub fn deinit(self: *Self) void {
        self.output.deinit();
        //self.buffer.deinit();
        self.messages.deinit();
    }

    // Get the current context of the parser.
    pub inline fn getStatementContext(self: *Self, back: usize) ?StatementContext {
        if (self.statement_contexts.items.len == back) return null;
        return self.statement_contexts.items[self.statement_contexts.items.len - (1 + back)];
    }

    // Get a pointer to the current context of the parser.
    pub inline fn getStatementContextPtr(self: *Self, back: usize) ?*StatementContext {
        if (self.statement_contexts.items.len == back) return null;
        return &self.statement_contexts.items[self.statement_contexts.items.len - (1 + back)];
    }

    // Starts a new expression.
    pub inline fn startExpressionChain(self: *Self) !void {
        try self.statement_contexts.append(.{ .expression = .{ .chain = ExpressionChain.init(self.allocator) } });
    }

    // Parse all tokens.
    pub fn parseFull(self: *Self) !Block {
        while (self.pos < self.tokens.items.len) {
            try self.parseStatement();
        }
        return self.output;
    }

    // Parse tokens into the next statement.
    pub fn parseStatement(self: *Self) !void {
        var initial_len = self.output.items.len;

        while (self.pos < self.tokens.items.len and initial_len == self.output.items.len) : (self.pos += 1) {
            var token = self.tokens.items[self.pos];

            switch (token.token) {
                Token.value => |value| {
                    _ = value;
                },
                Token.char => |char| {
                    var context = self.getStatementContextPtr(0) orelse {
                        return self.messages.printFatal("character with no context.", .{}, token.start, token.end);
                    };

                    if (context.* != StatementContext.expression) {
                        return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    }

                    context.* = .{ .expression = .{ .value = .{ .char = char } } };
                },
                Token.string => |string| {
                    var context = self.getStatementContextPtr(0) orelse {
                        return self.messages.printFatal("string with no context.", .{}, token.start, token.end);
                    };

                    if (context.* != StatementContext.expression) {
                        return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    }

                    context.* = .{ .expression = .{ .value = .{ .string = string } } };
                },
                Token.uint, Token.int, Token.float => {
                    var context = self.getStatementContextPtr(0) orelse {
                        return self.messages.printFatal("number with no context.", .{}, token.start, token.end);
                    };

                    if (context.* != StatementContext.expression) {
                        return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    }

                    if (context.*.expression == null) {
                        context.* = .{ .expression = try Expression.fromToken(self.allocator, token.token) };
                    } else if (context.*.expression == Expression.chain) {
                        try context.*.expression.chain.append(ExpressionChainItem.fromToken(token.token).?);
                    } else {
                        return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    }
                },
                Token.keyword => |keyword| {
                    if (std.mem.eql(u8, "and", keyword) or std.mem.eql(u8, "or", keyword) or std.mem.eql(u8, "not", keyword)) {
                        var context = self.getStatementContextPtr(0) orelse {
                            return try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        };

                        if (context.* != StatementContext.expression or context.*.expression != Expression.chain) {
                            return try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        }

                        try context.*.expression.chain.append(ExpressionChainItem.fromToken(token.token).?);
                    } else if (std.mem.eql(u8, "pub", keyword)) {
                        try self.statement_contexts.append(StatementContext.public);
                    } else if (std.mem.eql(u8, "const", keyword)) {
                        try self.statement_contexts.append(.{ .constant = .{} });
                    } else if (std.mem.eql(u8, "let", keyword)) {
                        try self.statement_contexts.append(.{ .variable = .{} });
                    } else if (std.mem.eql(u8, "mut", keyword)) {
                        if (self.getStatementContextPtr(0)) |context| {
                            if (context.* == StatementContext.variable) {
                                context.variable.mutable = true;
                            } else {
                                try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                            }
                        } else {
                            try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        }
                    } else {
                        try self.messages.printFatal("unknown keyword.", .{}, token.start, token.end);
                    }
                },
                Token.symbol => |symbol| switch (symbol) {
                    '&', '|', '~', '^', '*', '/', '%', '+', '-' => if (self.getStatementContextPtr(0)) |context| {
                        if (context.* == StatementContext.expression and context.*.expression == Expression.chain) {
                            try context.expression.chain.append(.{ .operation = Operation.fromSymbol(symbol).? });
                        } else {
                            try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        }
                    } else {
                        try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    },
                    '=' => {
                        var context = self.getStatementContextPtr(0) orelse {
                            return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        };

                        switch (context.*) {
                            StatementContext.constant, StatementContext.variable => try self.startExpressionChain(),
                            else => try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end),
                        }
                    },
                    ':' => {
                        var context = self.getStatementContextPtr(0) orelse {
                            return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        };

                        switch (context.*) {
                            StatementContext.constant, StatementContext.variable => try self.startExpressionChain(),
                            else => try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end),
                        }
                    },
                    '[' => {
                        //try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    },
                    ']' => {},
                    ';' => {
                        var context = self.getStatementContextPtr(0) orelse {
                            return self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        };

                        if (self.getStatementContextPtr(1)) |parent_context| {
                            if (parent_context.* == StatementContext.public) {
                                if (context.* == StatementContext.constant) {
                                    context.*.constant.public = true;
                                } else if (context.* == StatementContext.variable) {
                                    context.*.variable.public = true;
                                }
                            }
                        }
                    },
                    else => try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end),
                },
                Token.identifier => |identifier| {
                    if (self.getStatementContextPtr(0)) |context| {
                        if (context.* == StatementContext.constant and context.*.constant.name.len == 0) {
                            context.*.constant.name = identifier;
                        } else if (context.* == StatementContext.variable and context.*.variable.name.len == 0) {
                            context.*.variable.name = identifier;
                        } else if (context.* == StatementContext.expression) {
                            if (self.getStatementContext(1)) |parent| {
                                if (parent == StatementContext.type_expr) {}
                            }
                        } else { // TODO
                            return try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                        }
                    } else { // TODO
                        return try self.messages.printFatal("invalid syntax.", .{}, token.start, token.end);
                    }
                },
            }
        }
    }
};
