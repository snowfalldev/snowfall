const std = @import("std");
const big = std.math.big;

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

const number = @import("value/number.zig");
pub const NumberType = number.NumberType;
pub const Number = number.Number;

pub const TypeDesc = union(enum) {
    builtin: BuiltinType,
    external: u63,

    pub inline fn toU64(self: TypeDesc) u64 {
        return switch (self) {
            .builtin => |v| @intCast(@intFromEnum(v)),
            .external => |v| @as(u64, 1 << 63) | @as(u64, @intCast(v)),
        };
    }

    pub inline fn fromU64(int: u64) TypeDesc {
        if ((int >> 63) == 0) return .{ .builtin = @truncate(int) };
        return .{ .external = @truncate(int) };
    }
};

pub const TypeSet = struct {
    inner: std.AutoHashMap(u64, void),

    pub inline fn contains(self: TypeSet, desc: TypeDesc) bool {
        return self.inner.contains(desc.toU64());
    }

    pub inline fn count(self: TypeSet) usize {
        return self.inner.count();
    }

    pub inline fn put(self: *TypeSet, desc: TypeDesc) !void {
        return self.inner.put(desc.toU64(), void{});
    }

    pub inline fn remove(self: *TypeSet, desc: TypeDesc) bool {
        return self.inner.remove(desc.toU64());
    }
};

// zig fmt: off

pub const BuiltinType = enum(u8) {
    bigint = 0x16,
    i128   = 0x15,
    i64    = 0x14,
    i32    = 0x13,
    i16    = 0x12,
    i8     = 0x11,
    u128   = 0x05,
    u64    = 0x04,
    u32    = 0x03,
    u16    = 0x02,
    u8     = 0x01,
    f128   = 0x25,
    f64    = 0x24,
    f32    = 0x23,
    f16    = 0x22,

    int   = 0x10,
    uint  = 0x00,
    float = 0x20,
    
    number = 0x30,
    string = 0x40,
    char   = 0x45,
    bool   = 0x50,
    void   = 0x60,
    type   = 0x70,
    func   = 0x80,
    any    = 0xFF,

    pub const string_map = std.StaticStringMap(BuiltinType).initComptime(.{
        .{ "bigint", .bigint },
        .{ "i128",   .i128 },
        .{ "i64",    .i64 },
        .{ "i32",    .i32 },
        .{ "i16",    .i16 },
        .{ "i8",     .i8 },
        .{ "u128",   .u128 },
        .{ "u64",    .u64 },
        .{ "u32",    .u32 },
        .{ "u16",    .u16 },
        .{ "u8",     .u8 },
        .{ "f128",   .f128 },
        .{ "f64",    .f64 },
        .{ "f32",    .f32 },
        .{ "f16",    .f16 },

        .{ "int",   .int },
        .{ "uint",  .uint },
        .{ "float", .float },

        .{ "number", .number },
        .{ "string", .string },
        .{ "bool",   .bool },
        .{ "void",   .void },
        .{ "type",   .type },
        .{ "func",   .func },
        .{ "any",    .any },
    });
};

// zig fmt: on

pub const BuiltinValue = union(enum) {
    number: Number,
};

// TODO: idk where to put this but it needs a home for later use

// Maximum values for uint types.
pub const uint_max = std.ComptimeStringMap(u128, .{
    .{ "u128", std.math.maxInt(u128) },
    .{ "u64", std.math.maxInt(u64) },
    .{ "u32", std.math.maxInt(u32) },
    .{ "u16", std.math.maxInt(u16) },
    .{ "u8", std.math.maxInt(u8) },
});

// Minimum values for int types.
pub const int_min = std.ComptimeStringMap(i128, .{
    .{ "i128", std.math.minInt(i128) },
    .{ "i64", std.math.minInt(i64) },
    .{ "i32", std.math.minInt(i32) },
    .{ "i16", std.math.minInt(i16) },
    .{ "i8", std.math.minInt(i8) },
});

// Maximum values for int types.
pub const int_max = std.ComptimeStringMap(i128, .{
    .{ "i128", std.math.maxInt(i128) },
    .{ "i64", std.math.maxInt(i64) },
    .{ "i32", std.math.maxInt(i32) },
    .{ "i16", std.math.maxInt(i16) },
    .{ "i8", std.math.maxInt(i8) },
});

// Minimum values for float types.
pub const float_min = std.ComptimeStringMap(f128, .{
    .{ "f128", std.math.floatMin(f128) },
    .{ "f64", std.math.floatMin(f64) },
    .{ "f32", std.math.floatMin(f32) },
    .{ "f16", std.math.floatMin(f16) },
});

// Maximum values for float types.
pub const float_max = std.ComptimeStringMap(f128, .{
    .{ "f128", std.math.floatMax(f128) },
    .{ "f64", std.math.floatMax(f64) },
    .{ "f32", std.math.floatMax(f32) },
    .{ "f16", std.math.floatMax(f16) },
});
