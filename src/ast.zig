const std = @import("std");
const Allocator = std.mem.Allocator;

const utils = @import("utils");

pub const Lexer = @import("ast/Lexer.zig");
const Token = Lexer.Token;

pub const Parser = @import("ast/Parser.zig");

// SOURCE FILE POSITIONS

pub const Pos = struct {
    raw: usize = 0,
    row: u32 = 0,
    col: u16 = 0,

    pub inline fn format(self: Pos, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{}:{}", .{ self.row + 1, self.col + 1 });
    }
};

pub const Span = struct { Pos, Pos };

pub const RowInfo = packed struct { raw: u48, len: u16 };

// ABSTRACT SYNTAX TREE

// STATEMENTS
pub const BlockStatement = union(enum) {
    call: Call,
    function: Function,
    constant: Constant,
    variable: Variable,
    assignment: Assignment,
};

pub const Block = []const BlockStatement;

// OPERANDS

pub const String = struct {
    text: []const u8 = "",
    managed: bool = false,
};

pub const Number = union(enum) {
    float: f64,
    int: std.math.big.int.Const,
};

pub const LiteralOperand = union(enum) {
    null,
    undefined,
    bool: bool,
    char: u21,
    string: String,
    number: Number,

    const Self = @This();

    pub inline fn fromToken(token: Token) ?Self {
        return switch (token) {
            Token.null => Self.null,
            Token.undefined => Self.undefined,
            Token.bool => |b| .{ .bool = b },
            Token.char => |char| .{ .char = char },
            Token.string => |string| .{ .string = string.data },
            Token.number => |number| .{ .number = number },
            else => null,
        };
    }
};

pub const Template = struct {
    strings: []String,
    exprs: []Expression,
};

pub const NonLiteralOperand = union(enum) {
    template: Template,
    identifier: []const u8,
    field_access: FieldAccess,
    index_access: IndexAccess,
    call: Call,

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
    operand: *Operand,
    operation: *Operation,
    logical_not: *Expression,
    bitwise_not: *Expression,
};

// OPERATIONS

pub const Chainer = enum { @"and", @"or" };

pub const Comparator = enum(u8) {
    // zig fmt: off
    eq, ne,
    gt, lt,
    gt_eq, lt_eq,
    // zig fmt: on

    pub inline fn toString(cmp: Comparator) []const u8 {
        return switch (cmp) {
            .eq => "==",
            .ne => "!=",

            .gt => ">",
            .lt => "<",

            .gt_eq => ">=",
            .lt_eq => "<=",
        };
    }
};

pub const BitwiseOp = enum {
    // zig fmt: off
    ls, rs, urs,
    @"and", @"or", xor,
    // zig fmt: on

    pub inline fn toString(op: BitwiseOp) []const u8 {
        return switch (op) {
            .ls => "<<",
            .rs => ">>",
            .urs => ">>>",

            .@"and" => "&",
            .@"or" => "|",
            .xor => "^",
        };
    }

    pub inline fn fromSymbol(symbol: u8) ?BitwiseOp {
        return switch (symbol) {
            '&' => .@"and",
            '|' => .@"or",
            '^' => .xor,
            else => null,
        };
    }
};

pub const MathOp = enum {
    // zig fmt: off
    add, sub,
    mul, div, mod,
    // zig fmt: on

    pub inline fn toString(op: MathOp) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",

            .mul => "*",
            .div => "/",
            .mod => "%",
        };
    }

    pub inline fn fromSymbol(symbol: u8) ?MathOp {
        return switch (symbol) {
            '+' => .add,
            '-' => .sub,
            '*' => .mul,
            '/' => .div,
            '%' => .mod,
            else => null,
        };
    }
};

pub const Operator = union(enum) { math: MathOp, bw: BitwiseOp };

pub const Operation = struct {
    lhs: Expression,
    op: union(enum) {
        cmp: Comparator,
        op: Operator,
        chain: Chainer,
    },
    rhs: Expression,
};

// TYPES
pub const BasicType = enum(u8) {
    int = 0x00,
    float = 0x05,
    string = 0x40,
    char = 0x45,
    bool = 0x50,
    void = 0x60,
    //func = 0x80,
    //any = 0xFF,

    pub const string_map = utils.EnumStringMap(BasicType, .fastest);
};

pub const PointerType = struct {
    constant: bool = false,
    kind: enum {
        single, // *Type
        slice, // []Type
        many, // [*]Type
    } = .single,
    expression: *Type = undefined,
};

pub const ArrayType = struct {
    size: usize = 0,
    expression: *Type = undefined,
};

pub const Type = union(enum) {
    builtin: BasicType,
    custom: []const u8,
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
    operator: ?Operator = null,
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

pub const Call = struct {
    callee: *Operand = undefined,
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
