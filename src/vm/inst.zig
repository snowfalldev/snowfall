const ast = @import("../ast.zig");
const value = @import("value.zig");

pub const Instruction = union(enum) {
    op: Op,
    cmp: Cmp,
    chain: Chain,
    move: Move,
};

// VALUE STORAGE

pub const Location = union(enum) {
    register: usize,
    stack_offset: usize,
};

pub const Value = union(enum) {
    immediate: value.Value,
    by_location: Location,
};

// BASIC OPERATIONS

pub const Op = struct { lhs: Value, op: ast.Operator, rhs: Value, dst: Location };
pub const Cmp = struct { lhs: Value, op: ast.Comparator, rhs: Value, dst: Location };
pub const Chain = struct { lhs: Value, op: ast.Chainer, rhs: Value, dst: Location };
pub const Move = struct { src: Value, dst: Location };

// ACCESSORS

pub const MapAccess = struct {
    value: Value,
    field: Value,
};

pub const StructAccess = struct {
    value: Value,
    field: []const u8,
};