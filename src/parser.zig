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

// ROOTS / BLOCKS AND INSTRUCTIONS
pub const Tree = std.ArrayList(Node);

pub const Node = union(enum) {
    function: Function,
    call: Call,
    builtin_call: BuiltinCall,
    constant: Constant,
    variable: Variable,
    assignment: Assignment,
    deref: NodeWrapper,
    pointer: NodeWrapper,

    const Self = @This();

    pub inline fn deref(self: *Self) Self {
        return .{ .deref = .{self} };
    }

    pub inline fn pointer(self: *Self) Self {
        return .{ .pointer = .{self} };
    }

    pub inline fn root(self: *Self) bool {
        return switch (self.*) {
            Self.function, Self.constant => true,
            else => false,
        };
    }

    pub inline fn block(self: *Self) bool {
        return switch (self.*) {
            Self.function => false,
            else => true,
        };
    }
};

pub const NodeWrapper = struct { *Node };

// VALUES AND VARIABLES
pub const Comparator = enum {
    eq,
    ne,
    gt,
    lt,
    gt_eq,
    lt_eq,
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

pub const Operator = enum {
    sum,
    sub,
    div,
    mul,
    mod,
    bw_ls,
    bw_lrs,
    bw_ars,
    bw_and,
    bw_or,
    bw_not,
    l_not,
    xor,

    const Self = @This();

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

pub const ImmediateExpr = union(enum) {
    identifier: []const u8,
    field_access: FieldAccess,
    deref: NonLiteralExpr,

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

// EXPRESSION CHAINS
pub const ExpressionChainItem = union(enum) {
    basic: BasicExpression,
    chain: ExpressionChain,
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
    variable: ImmediateExpr = undefined,
    operator: ?Operator = null,
    expr: ExpressionChain = undefined,
};

pub const IndexAccess = struct {
    object: *BasicExpression = undefined,
    index: ExpressionChain = undefined,
};

// FUNCTIONS
pub const FunctionArgument = struct {
    name: []const u8 = "",
    typ: Type = undefined,
};

pub const FunctionArguments = std.ArrayList(FunctionArgument);

pub const Function = struct {
    name: []const u8 = "",
    args: FunctionArguments = undefined,
    typ: Type = undefined,
    body: Tree = undefined,
    public: bool = false,
};

// FUNCTION CALLS
pub const Call = struct {
    expr: *BasicExpression = undefined,
    args: std.ArrayList(ExpressionChain) = undefined,
};

pub const BuiltinCall = struct {
    name: []const u8 = "",
    args: std.ArrayList(ExpressionChain) = undefined,
};

// OBJECTS - FIELDS
pub const FieldAccess = struct {
    object: *BasicExpression = undefined,
    field: []const u8 = "",
};

pub const Field = struct {
    name: []const u8 = "",
    typ: Type = undefined,
    default: ?*BasicExpression = null,
};

// OBJECTS - STRUCT
pub const Struct = struct {
    name: []const u8,
    fields: std.ArrayList(Field),
    methods: std.ArrayList(Function),
    public: bool,
};

// INSTRUCTION CONTEXT
pub const Context = union(enum) {
    chain: ExpressionChain,
    basic: ?BasicExpression,
    constant: Constant,
    variable: Variable,
    assignment: Assignment,
    function: Function,
    func_arg: FunctionArgument,
    call: Call,
    builtin_call: BuiltinCall,
    field_access: FieldAccess,
    index_access: IndexAccess,
    typ: ?Type,

    deref,
    pointer,

    builtin,
    public,

    const Self = @This();
};

pub const Contexts = std.ArrayList(Context);

// GLOBAL CONTEXT
pub const FullContextMode = enum { root, block };

pub const FullContext = struct {
    mode: FullContextMode,
    tree: Tree,
    contexts: Contexts,
};

pub const FullContexts = std.ArrayList(FullContext);

// PARSER
pub const Parser = struct {
    // Stores the allocator used by the parser.
    allocator: Allocator,

    // Stores the output from the parser.
    output: Tree,

    // Stores the tokens output by the lexer.
    tokens: Tokens,

    // Stores the current parser position.
    pos: usize = 0,

    // Stores the current full parser contexts.
    full_contexts: FullContexts,

    // Stores the message handler used by the parser.
    messages: Messages,

    const Self = @This();

    // Initializes a new parser.
    pub inline fn init(allocator: Allocator, tokens: Tokens, name: ?[]const u8, data: ?[]const u8) !Self {
        var full_contexts = try FullContexts.initCapacity(allocator, 1);
        full_contexts.appendAssumeCapacity(.{ .mode = .root, .tree = Tree.init(allocator), .contexts = Contexts.init(allocator) });

        return .{
            .allocator = allocator,
            .output = Tree.init(allocator),
            .tokens = tokens,
            //.buffer = Tokens.init(allocator),
            .full_contexts = full_contexts,
            .messages = Messages.init(allocator, name, data),
        };
    }

    // De-initializes the parser.
    // This should only be run after the output of the parser is done being used.
    pub inline fn deinit(self: *Self) void {
        //self.output.deinit();
        //self.buffer.deinit();
        self.full_contexts.deinit();
        self.messages.deinit();
    }

    inline fn getFullContextPtr(self: *Self, back: usize) ?*FullContext {
        if (self.full_contexts.items.len == back) return null;
        return &self.full_contexts.items[self.full_contexts.items.len - (1 + back)];
    }

    inline fn getContextPtr(self: *Self, back: usize) ?*Context {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;
        if (full_ctx.*.contexts.items.len == back) return null;
        return &full_ctx.*.contexts.items[full_ctx.*.contexts.items.len - (1 + back)];
    }

    inline fn appendContext(self: *Self, ctx: Context) !void {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;
        try full_ctx.*.contexts.append(ctx);
    }

    inline fn popContext(self: *Self) void {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;
        _ = full_ctx.*.contexts.pop();
    }

    inline fn clearContext(self: *Self) void {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;
        full_ctx.*.contexts.clearRetainingCapacity();
    }

    inline fn appendNode(self: *Self, node: Node) !void {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;

        if (full_ctx.*.mode == FullContextMode.root and @constCast(&node).root()) {
            try full_ctx.*.tree.append(node);
        } else if (full_ctx.*.mode == FullContextMode.block and @constCast(&node).block()) {
            try full_ctx.*.tree.append(node);
        } else {
            @panic("welp");
        }
    }

    inline fn inBlock(self: *Self) bool {
        var full_ctx = self.getFullContextPtr(0) orelse unreachable;
        return full_ctx.*.mode == .block;
    }

    inline fn fatal(self: *Self, comptime str: []const u8) !void {
        var token = self.tokens.items[self.pos];
        try self.messages.printFatal(str, .{}, token.start, token.end);
    }

    inline fn getTokenRelative(self: *Self, offset: isize) LocatedToken {
        var idx = @as(isize, @intCast(self.pos)) + offset;
        if (idx < 0) try self.fatal("invalid syntax.");
        return self.tokens.items[@as(usize, @intCast(idx))];
    }

    inline fn assignment(self: *Self) !?Assignment {
        if (self.pos + 2 >= self.tokens.items.len) try self.fatal("invalid syntax.");

        var next = self.tokens.items[self.pos + 1].token;
        if (next != Token.symbol) try self.fatal("invalid syntax.");

        if (next.symbol == '=') {
            return .{};
        }

        var next2 = self.tokens.items[self.pos + 2].token;
        if (next2 != Token.symbol) return null;
        if (next2.symbol == '=') {
            return .{ .operator = Operator.fromSymbol(next.symbol) orelse {
                try self.fatal("invalid syntax.");
                return null;
            } };
        }

        if (self.pos + 3 >= self.tokens.items.len) return null;
        var next3 = self.tokens.items[self.pos + 3].token;
        if (next3 != Token.symbol) return null;
        if ((next.symbol == '>' or next.symbol == '<') and next2.symbol == next.symbol and next3.symbol == '=') {
            return .{ .operator = if (next.symbol == '>') Operator.bw_lrs else Operator.bw_ls };
        }

        if (self.pos + 4 >= self.tokens.items.len) return null;
        var next4 = self.tokens.items[self.pos + 4].token;
        if (next4 != Token.symbol) return null;
        if (next.symbol == '>' and next2.symbol == '>' and next3.symbol == '>' and next4.symbol == '=') {
            return .{ .operator = Operator.bw_ars };
        }

        return null;
    }

    // Parse all tokens.
    pub inline fn parseFull(self: *Self) !Tree {
        while (self.pos < self.tokens.items.len) {
            try self.parseNode();
        }
        self.output = (self.getFullContextPtr(0) orelse unreachable).*.tree;
        return self.output;
    }

    // Parse tokens into the next node.
    pub fn parseNode(self: *Self) !void {
        var initial_len = self.output.items.len;

        while (self.pos < self.tokens.items.len and initial_len == self.output.items.len) : (self.pos += 1) {
            var token = self.tokens.items[self.pos];

            switch (token.token) {
                Token.value, Token.char, Token.string, Token.uint, Token.int, Token.float => {
                    var ctx = self.getContextPtr(0) orelse return self.fatal("literal with no context.");
                    if (ctx.* == Context.basic) {
                        ctx.*.basic = BasicExpression.fromToken(token.token).?;
                    } else if (ctx.* == Context.chain) {
                        try ctx.*.chain.append(.{ .basic = BasicExpression.fromToken(token.token).? });
                    } else {
                        try self.fatal("invalid syntax.");
                    }
                },
                Token.keyword => |keyword| if (std.mem.eql(u8, "pub", keyword)) {
                    if (self.inBlock()) try self.fatal("public keyword outside of root.");
                    try self.appendContext(Context.public);
                } else if (std.mem.eql(u8, "fn", keyword)) {
                    if (self.inBlock()) try self.fatal("fn keyword outside of root.");
                    var public: bool = false;

                    if (self.getContextPtr(0)) |ctx| if (ctx.* == Context.public) {
                        public = true;
                        self.popContext();
                    } else {
                        try self.fatal("invalid syntax.");
                    };

                    try self.appendContext(.{ .function = .{ .public = public, .args = FunctionArguments.init(self.allocator) } });
                } else if (std.mem.eql(u8, "const", keyword)) {
                    var public: bool = false;

                    if (self.getContextPtr(0)) |ctx| if (ctx.* == Context.public) {
                        public = true;
                        self.popContext();
                    } else if (ctx.* == Context.typ and ctx.*.typ != null and ctx.*.typ.? == Type.ptr) {
                        ctx.*.typ.?.ptr.constant = true;
                        continue;
                    } else {
                        try self.fatal("invalid syntax.");
                    };

                    try self.appendContext(.{ .constant = .{ .public = public } });
                } else if (std.mem.eql(u8, "let", keyword)) {
                    var public: bool = false;

                    if (!self.inBlock()) {
                        if (self.getContextPtr(0)) |ctx| {
                            if (ctx.* == Context.public) {
                                public = true;
                                self.popContext();
                            } else {
                                try self.fatal("invalid syntax.");
                            }
                        }
                    }

                    try self.appendContext(.{ .variable = .{ .public = public } });
                } else if (std.mem.eql(u8, "mut", keyword)) {
                    var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");

                    if (ctx.* == Context.variable) {
                        ctx.variable.mutable = true;
                    } else {
                        try self.fatal("invalid syntax.");
                    }
                } else {
                    try self.messages.printFatal("unknown keyword.", .{}, token.start, token.end);
                },
                Token.symbol => |symbol| switch (symbol) {
                    '&', '|', '!', '~', '^', '*', '/', '%', '+', '-', '<', '>' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        if (ctx.* != Context.chain) try self.fatal("invalid syntax.");
                        if (self.pos + 2 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                        var next = self.tokens.items[self.pos + 1];
                        var next2 = self.tokens.items[self.pos + 2];

                        if (next.token == Token.symbol) {
                            if (next.token.symbol == symbol) {
                                self.pos += 1;
                                switch (symbol) {
                                    '&' => try ctx.chain.append(.{ .link_operator = LinkOperator.l_and }),
                                    '|' => try ctx.chain.append(.{ .link_operator = LinkOperator.l_or }),
                                    '<' => try ctx.chain.append(.{ .operator = Operator.bw_ls }),
                                    '>' => if (next2.token == Token.symbol and next2.token.symbol == '>') {
                                        self.pos += 1;
                                        try ctx.chain.append(.{ .operator = Operator.bw_ars });
                                    } else {
                                        try ctx.chain.append(.{ .operator = Operator.bw_lrs });
                                    },
                                    else => {
                                        self.pos -= 1;
                                        try ctx.chain.append(.{ .operator = Operator.fromSymbol(symbol).? });
                                    },
                                }
                                continue;
                            } else if (next.token.symbol == '=') switch (symbol) {
                                '!' => try ctx.chain.append(.{ .comparator = Comparator.ne }),
                                '<' => try ctx.chain.append(.{ .comparator = Comparator.lt_eq }),
                                '>' => try ctx.chain.append(.{ .comparator = Comparator.gt_eq }),
                                else => try self.fatal("invalid syntax."),
                            };
                        } else switch (symbol) {
                            '<' => try ctx.chain.append(.{ .comparator = Comparator.lt }),
                            '>' => try ctx.chain.append(.{ .comparator = Comparator.gt }),
                            else => try ctx.chain.append(.{ .operator = Operator.fromSymbol(symbol).? }),
                        }
                    },
                    '=' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");

                        var next = self.tokens.items[self.pos + 1].token;
                        var next_eq = next == Token.symbol and next.symbol == '=';

                        if (ctx.* == Context.typ) {
                            var parent = self.getContextPtr(1) orelse return self.fatal("invalid syntax.");
                            if (parent.* == Context.constant) {
                                parent.*.constant.typ = ctx.*.typ.?;
                            } else if (parent.* == Context.variable) {
                                parent.*.variable.typ = ctx.*.typ.?;
                            } else {
                                try self.fatal("invalid syntax.");
                            }
                            ctx = parent;
                            self.popContext();
                        }

                        if (ctx.* == Context.constant or ctx.* == Context.variable or ctx.* == Context.assignment) {
                            if (next_eq) {
                                self.pos += 1;
                                try self.fatal("invalid syntax.");
                            }

                            try self.appendContext(.{ .chain = ExpressionChain.init(self.allocator) });
                            continue;
                        }

                        if (ctx.* != Context.chain or !next_eq or self.pos + 2 >= self.tokens.items.len) try self.fatal("invalid syntax.");

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
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        switch (ctx.*) {
                            Context.constant, Context.variable => try self.appendContext(.{ .typ = null }),
                            else => try self.fatal("invalid syntax."),
                        }
                    },
                    '@' => {
                        try self.appendContext(Context.builtin);
                    },
                    '(' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        if (ctx.* == Context.function) {
                            if (ctx.*.function.args.items.len == 0) {
                                try self.appendContext(.{ .func_arg = .{} });
                            } else {
                                try self.fatal("invalid syntax.");
                            }
                        } else {
                            try self.appendContext(.{ .chain = ExpressionChain.init(self.allocator) });
                        }
                    },
                    ',' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        var parent = self.getContextPtr(1) orelse return self.fatal("invalid syntax.");

                        if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                        var next = self.tokens.items[self.pos + 1].token;

                        if (next == Token.symbol) switch (next.symbol) {
                            '.' => {},
                            else => {},
                        };

                        if (ctx.* == Context.typ and parent.* == Context.func_arg) {
                            // end of argument
                            var parent2 = self.getContextPtr(2) orelse return self.fatal("invalid syntax.");

                            parent.*.func_arg.typ = ctx.*.typ.?;

                            try parent2.*.function.args.append(parent.*.func_arg);
                            self.popContext();
                        } else if (ctx.* == Context.chain and (parent.* == Context.call or parent.* == Context.builtin_call)) {}
                    },
                    ')' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");
                        var parent = self.getContextPtr(1) orelse return self.fatal("invalid syntax.");

                        if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                        var next = self.tokens.items[self.pos + 1].token;

                        if (next == Token.symbol) switch (next.symbol) {
                            '.' => {},
                            else => {},
                        };

                        if (ctx.* == Context.typ and parent.* == Context.func_arg) {
                            // end of function
                            var parent2 = self.getContextPtr(2) orelse return self.fatal("invalid syntax.");

                            parent.*.func_arg.typ = ctx.*.typ.?;

                            try parent2.*.function.args.append(parent.*.func_arg);
                            self.popContext(); // pop typ
                            self.popContext(); // pop func_arg
                            try self.appendContext(.{ .typ = null }); // function return type
                        } else if (ctx.* == Context.func_arg and parent.* == Context.function and parent.*.function.args.items.len == 0) {
                            // end of function - no args
                            self.popContext(); // pop func_arg
                            try self.appendContext(.{ .typ = null }); // function return type
                        } else if (ctx.* == Context.chain) {
                            switch (parent.*) {
                                Context.chain => try parent.*.chain.append(.{ .chain = ctx.*.chain }),
                                Context.call => try parent.*.call.args.append(ctx.*.chain),
                                Context.builtin_call => try parent.*.builtin_call.args.append(ctx.*.chain),
                                else => try self.fatal("invalid syntax."),
                            }
                            self.popContext();
                        } else {
                            try self.fatal("invalid syntax.");
                        }
                    },
                    '{' => {
                        if (self.getContextPtr(0)) |ctx| {
                            var parento = self.getContextPtr(1);
                            if (ctx.* == Context.typ and parento != null and parento.?.* == Context.function) {
                                if (ctx.*.typ != null) parento.?.*.function.typ = ctx.*.typ.?;
                                try self.full_contexts.append(.{ .mode = .block, .tree = Tree.init(self.allocator), .contexts = Contexts.init(self.allocator) });
                            }
                        } else {
                            try self.full_contexts.append(.{ .mode = .block, .tree = Tree.init(self.allocator), .contexts = Contexts.init(self.allocator) });
                        }
                    },
                    '}' => {
                        var parent_fc = self.getFullContextPtr(1) orelse return self.fatal("context close without start.");
                        if (parent_fc.*.contexts.items.len > 0 and parent_fc.*.contexts.items[0] == Context.function) {
                            try parent_fc.*.tree.append(.{ .function = parent_fc.*.contexts.items[0].function });
                        }
                        _ = self.full_contexts.pop();
                    },
                    '[' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");

                        if (ctx.* == Context.typ) {
                            if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                            var next = self.tokens.items[self.pos + 1];
                            if (next.token != Token.symbol or next.token.symbol != ']') try self.fatal("invalid syntax.");
                            self.pos += 1;
                            ctx.*.typ = .{ .ptr = .{ .slice = true } };
                        }
                    },
                    ']' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");

                        switch (ctx.*) {
                            Context.chain => |chain| if (self.getContextPtr(1)) |parent| switch (parent.*) {
                                Context.index_access => {
                                    parent.*.index_access.index = chain;
                                },
                                else => try self.fatal("invalid syntax."),
                            },
                            else => try self.fatal("invalid syntax."),
                        }
                    },
                    ';' => {
                        var ctx = self.getContextPtr(0) orelse return self.fatal("invalid syntax.");

                        switch (ctx.*) {
                            Context.chain => |chain| if (self.getContextPtr(1)) |parent| switch (parent.*) {
                                Context.constant => {
                                    parent.*.constant.val = chain;
                                    try self.appendNode(Node{ .constant = parent.*.constant });
                                },
                                Context.variable => {
                                    parent.*.variable.val = chain;
                                    try self.appendNode(Node{ .variable = parent.*.variable });
                                },
                                Context.assignment => {
                                    parent.*.assignment.expr = chain;
                                    try self.appendNode(Node{ .assignment = parent.*.assignment });
                                },
                                Context.typ => {
                                    if (self.getContextPtr(2)) |parent2| {
                                        if (parent2.* != Context.variable) {
                                            try self.fatal("invalid syntax.");
                                        }
                                    }
                                },
                                else => try self.fatal("invalid syntax."),
                            },
                            Context.call => try self.appendNode(Node{ .call = ctx.*.call }),
                            Context.builtin_call => try self.appendNode(Node{ .builtin_call = ctx.*.builtin_call }),
                            else => try self.fatal("invalid syntax."),
                        }

                        self.clearContext();
                        self.pos += 1;

                        break;
                    },
                    else => try self.fatal("invalid syntax."),
                },
                Token.identifier => |identifier| {
                    // TODO:
                    // 1. if context is field access, put ident as field
                    // 2. next char
                    //    - if ('=' OR op + '=' OR double arrow + '=' OR '>>>='), assignment OR
                    //    - if '.', start field access (if ctx was field access, put it in object) OR
                    //    - if '[', start item access / annotation OR
                    //    - if '(' and ctx is not function, start function (builtin) call OR
                    //    - if ':' and ctx is function arg, store arg name and start type OR
                    //    - else, do other stuff

                    var ctxo = self.getContextPtr(0);

                    var expr = BasicExpression.fromToken(token.token).?;
                    var field_access = ctxo != null and ctxo.?.* == Context.field_access;
                    if (field_access) {
                        ctxo.?.field_access.field = identifier;
                        expr = .{ .non_literal = .{ .field_access = ctxo.?.field_access } };
                    }

                    if (self.pos + 1 >= self.tokens.items.len) try self.fatal("invalid syntax.");
                    var next = self.tokens.items[self.pos + 1].token;

                    if (next == Token.symbol) switch (next.symbol) {
                        '=', '&', '|', '!', '~', '^', '*', '/', '%', '+', '-', '<', '>' => if (ctxo == null or field_access) {
                            if (try self.assignment()) |a| {
                                try self.appendContext(.{ .assignment = a });
                                try self.appendContext(.{ .chain = ExpressionChain.init(self.allocator) });
                                self.pos += 1;
                                if (a.operator != null) switch (a.operator.?) {
                                    Operator.bw_ars => self.pos += 3,
                                    Operator.bw_lrs, Operator.bw_ls => self.pos += 2,
                                    else => self.pos += 1,
                                };
                                continue;
                            } else {
                                try self.fatal("invalid syntax.");
                            }
                        },
                        '.' => {
                            if (field_access) self.popContext();
                            try self.appendContext(.{ .field_access = .{ .object = &expr } });
                            self.pos += 1;
                            continue;
                        },
                        '(' => if (ctxo == null or ctxo.?.* != Context.function) {
                            if (field_access) self.popContext();
                            if (ctxo != null and ctxo.?.* == Context.builtin) {
                                self.popContext();
                                try self.appendContext(.{ .builtin_call = .{ .name = identifier, .args = std.ArrayList(ExpressionChain).init(self.allocator) } });
                            } else {
                                try self.appendContext(.{ .call = .{ .expr = &expr, .args = std.ArrayList(ExpressionChain).init(self.allocator) } });
                            }
                            try self.appendContext(.{ .chain = ExpressionChain.init(self.allocator) });
                            self.pos += 1;
                            continue;
                        },
                        '[' => {
                            if (ctxo != null and ctxo.?.* == Context.builtin) {
                                self.popContext(); // TODO
                                try self.appendContext(.{ .builtin_call = .{ .name = identifier, .args = std.ArrayList(ExpressionChain).init(self.allocator) } });
                            } else {
                                try self.appendContext(.{ .index_access = .{ .object = &expr } });
                            }
                            try self.appendContext(.{ .chain = ExpressionChain.init(self.allocator) });
                            self.pos += 1;
                            continue;
                        },
                        ':' => if (ctxo != null and ctxo.?.* == Context.func_arg) {
                            ctxo.?.*.func_arg.name = identifier;
                            try self.appendContext(.{ .typ = null });
                            self.pos += 1;
                            continue;
                            // TODO
                        },
                        else => {},
                    };

                    var ctx = ctxo orelse unreachable;

                    if (ctx.* == Context.function and ctx.*.function.name.len == 0) {
                        ctx.*.function.name = identifier;
                    } else if (ctx.* == Context.constant and ctx.*.constant.name.len == 0) {
                        ctx.*.constant.name = identifier;
                    } else if (ctx.* == Context.variable and ctx.*.variable.name.len == 0) {
                        ctx.*.variable.name = identifier;
                    } else if (ctx.* == Context.typ) {
                        if (ctx.*.typ == null) {
                            ctx.*.typ = .{ .single = expr.non_literal };
                        } else if (ctx.*.typ.? == Type.ptr) {
                            ctx.*.typ.?.ptr.expression = expr.non_literal;
                        } else if (ctx.*.typ.? == Type.array) {
                            ctx.*.typ.?.array.expression = expr.non_literal;
                        }
                    } else if (ctx.* == Context.chain) {
                        try ctx.*.chain.append(.{ .basic = .{ .non_literal = .{ .identifier = identifier } } });
                        //if (self.getContextPtr(1)) |parent| {
                        //    if (parent.* == Context.typ) {}
                        //}
                    } else { // TODO
                        std.debug.print("\n{}\n\n", .{ctx.*});
                        try self.fatal("invalid syntax.");
                    }
                },
            }
        }
    }
};
