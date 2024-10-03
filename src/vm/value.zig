const std = @import("std");

const number = @import("value/number.zig");
pub const NumberType = number.NumberType;
pub const Number = number.Number;

pub const String = @import("value/String.zig");

pub const BuiltinValue = union(enum) {
    number: Number,
    string: String,
};

pub const Value = union(enum) {
    builtin: BuiltinValue,
};
