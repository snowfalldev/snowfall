pub const Lexer = @import("ast/Lexer.zig");

const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = Lexer.Token;

// SOURCE FILE POSITIONS

pub const Pos = struct {
    raw: u32 = 0,
    row: u32 = 0,
    col: u32 = 0,

    // Returns the position after this one.
    pub inline fn next(pos: Pos) Pos {
        return .{
            .raw = pos.raw + 1,
            .row = pos.row,
            .col = pos.col + 1,
        };
    }
};

pub const Span = struct { Pos = .{}, Pos = .{} };

// ABSTRACT SYNTAX TREE

// STATEMENTS
pub const BlockStatement = union(enum) {
    call: Call,
    builtin_call: BuiltinCall,
    function: Function,
    constant: Constant,
    variable: Variable,
    assignment: Assignment,
};

pub const Block = std.ArrayList(BlockStatement);

// EXPRESSIONS
pub const Comparator = enum {
    eq,
    ne,
    gt,
    lt,
    gt_eq,
    lt_eq,
};

pub const BoolOperator = enum {
    l_and,
    l_or,

    const Self = @This();

    pub inline fn fromDouble(symbol: u8) ?Self {
        if (symbol == '&') return Self.l_and;
        if (symbol == '|') return Self.l_or;
        return null;
    }
};

pub const MathOperator = enum {
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
pub const LiteralOperand = union(enum) {
    null,
    undefined,
    bool: bool,
    char: u21,
    string: []const u8,
    uint: u128,
    int: i128,
    float: f128,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.bool => |b| .{ .bool = b },
            Token.null => Self.null,
            Token.undefined => Self.undefined,
            Token.char => |char| .{ .char = char },
            Token.string => |string| .{ .string = string },
            Token.uint => |uint| .{ .uint = uint },
            Token.int => |int| .{ .int = int },
            Token.float => |float| .{ .float = float },
            else => null,
        };
    }
};

pub const NonLiteralOperand = union(enum) {
    identifier: []const u8,
    field_access: FieldAccess,
    index_access: IndexAccess,
    call: Call,
    builtin_call: BuiltinCall,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return if (token == Token.identifier) .{ .identifier = token.identifier } else null;
    }
};

pub const Operand = union(enum) {
    literal: LiteralOperand,
    non_literal: NonLiteralOperand,

    const Self = @This();

    pub fn fromToken(token: Token) ?Self {
        if (LiteralOperand.fromToken(token)) |e| {
            return .{ .literal = e };
        } else if (NonLiteralOperand.fromToken(token)) |e| {
            return .{ .non_literal = e };
        } else return null;
    }
};

// EXPRESSIONS

pub const Expression = union(enum) {
    operation: *Operation,
    operand: *Operand,
};

pub const Operation = struct {
    operandl: Expression,
    op: union(enum) {
        cmp: Comparator,
        bool: BoolOperator,
        math: MathOperator,
    },
    operandr: Expression,
};

// TYPES
pub const PointerType = struct {
    constant: bool = false,
    slice: bool = false,
    expression: *Type = undefined,
};

pub const ArrayType = struct {
    size: usize = 0,
    expression: *Type = undefined,
};

pub const Type = union(enum) {
    single: NonLiteralOperand,
    ptr: PointerType,
    array: ArrayType,
    func: FunctionType,
};

// CONSTANT DECLARATION
pub const Constant = struct {
    typ: ?Type = undefined,
    val: Expression = undefined,
    public: bool = false,
};

// VARIABLE DECLARATION
pub const Variable = struct {
    typ: ?Type = null,
    val: ?Expression = null,
    public: bool = false,
};

// VARIABLE ASSIGNMENT
pub const Assignment = struct {
    variable: NonLiteralOperand = undefined,
    operator: ?MathOperator = null,
    expression: Expression = undefined,
};

pub const IndexAccess = struct {
    object: *Operand = undefined,
    index: Expression = undefined,
};

// FUNCTIONS
pub const Parameters = std.StringArrayHashMap(?Type);

pub const FunctionType = struct {
    params: Parameters,
    typ: ?*Type = null,
};

pub const Function = struct {
    typ: FunctionType,
    body: Block,
};

pub const FunctionDefinition = struct {
    func: Function,
    name: []const u8 = "",
    public: bool = false,
};

// FUNCTION CALLS
pub const Call = struct {
    callee: *Operand = undefined,
    args: std.ArrayList(Expression) = undefined,
};

pub const BuiltinCall = struct {
    callee: []const u8 = "",
    args: std.ArrayList(Expression) = undefined,
};

// OBJECTS
pub const ObjectStatement = union(enum) {
    function: Function,
    field: Field,
};

// OBJECTS - FIELDS
pub const FieldAccess = struct {
    object: *Operand = undefined,
    field: []const u8 = "",
};

pub const Field = struct {
    name: []const u8 = "",
    typ: Type = undefined,
    default: ?*Expression = null,
    public: bool = false,
};

// OBJECTS - STRUCT
pub const Struct = struct {
    fields: std.StringHashMap(Field),
    methods: std.StringHashMap(Function),
};

// OBJECTS - ENUM
pub const EnumOption = union(enum) {
    empty,
};

pub const Enum = struct {
    options: std.StringHashMap(EnumOption),
    methods: std.StringHashMap(Function),
};
