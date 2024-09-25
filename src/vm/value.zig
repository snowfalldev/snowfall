const std = @import("std");

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

// zig fmt: off

pub const BuiltinType = enum(u8) {
        u128 = 0x05,
        u64  = 0x04,
        u32  = 0x03,
        u16  = 0x02,
        u8   = 0x01,
        i128 = 0x15,
        i64  = 0x14,
        i32  = 0x13,
        i16  = 0x12,
        i8   = 0x11,
        f128 = 0x25,
        f64  = 0x24,
        f32  = 0x23,
        f16  = 0x22,

        uint  = 0x00,
        int   = 0x10,
        float = 0x20,
        
        number = 0x30,
        string = 0x40,
        type   = 0x50,
        bool   = 0x60,
        void   = 0x70,
        any    = 0xFF,

        pub const string_map = std.StaticStringMap(BuiltinType).initComptime(.{
            .{ "u128", .u128 },
            .{ "u64",  .u64 },
            .{ "u32",  .u32 },
            .{ "u16",  .u16 },
            .{ "u8",   .u8 },
            .{ "i128", .i128 },
            .{ "i64",  .i64 },
            .{ "i32",  .i32 },
            .{ "i16",  .i16 },
            .{ "i8",   .i8 },
            .{ "f128", .f128 },
            .{ "f64",  .f64 },
            .{ "f32",  .f32 },
            .{ "f16",  .f16 },

            .{ "uint",  .uint },
            .{ "int",   .int },
            .{ "float", .float },

            .{ "number", .number },
            .{ "string", .string },
            .{ "type",   .type },
            .{ "bool",   .bool },
            .{ "void",   .void },
            .{ "any",    .any },
        });
};

// zig fmt: on

pub const BuiltinValue = union(enum) {
    number: Number,

    // zig fmt: off

    pub const UintType = enum(u4) {
        u128 = 0x4,
        u64  = 0x3,
        u32  = 0x2,
        u16  = 0x1,
        u8   = 0x0,
    };

    pub const Uint = union(UintType) {
        u128: u128,
        u64:  u64,
        u32:  u32,
        u16:  u16,
        u8:   u8,
    };

    pub const IntType = enum(u4) {
        i128 = 0x4,
        i64  = 0x3,
        i32  = 0x2,
        i16  = 0x1,
        i8   = 0x0,
    };

    pub const Int = union(IntType) {
        i128: i128,
        i64:  i64,
        i32:  i32,
        i16:  i16,
        i8:   i8,
    };

    pub const FloatType = enum(u4) {
        f128 = 0x3,
        f64  = 0x2,
        f32  = 0x1,
        f16  = 0x0,
    };

    pub const Float = union(FloatType) {
        f128: f128,
        f64:  f64,
        f32:  f32,
        f16:  f16,
    };

    pub const NumberClass = enum(u4) {
        uint  = 0x0,
        int   = 0x1,
        float = 0x2,

        pub const string_map = std.StaticStringMap(NumberClass).initComptime(.{
            .{ "uint",  .uint },
            .{ "int",   .int },
            .{ "float", .float },
        });
    };

    pub const NumberType = enum(u8) {
        u128 = 0x04,
        u64  = 0x03,
        u32  = 0x02,
        u16  = 0x01,
        u8   = 0x00,
        i128 = 0x14,
        i64  = 0x13,
        i32  = 0x12,
        i16  = 0x11,
        i8   = 0x10,
        f128 = 0x23,
        f64  = 0x22,
        f32  = 0x21,
        f16  = 0x20,

        pub const string_map = std.StaticStringMap(NumberType).initComptime(.{
            .{ "u128", .u128 },
            .{ "u64",  .u64 },
            .{ "u32",  .u32 },
            .{ "u16",  .u16 },
            .{ "u8",   .u8 },
            .{ "i128", .i128 },
            .{ "i64",  .i64 },
            .{ "i32",  .i32 },
            .{ "i16",  .i16 },
            .{ "i8",   .i8 },
            .{ "f128", .f128 },
            .{ "f64",  .f64 },
            .{ "f32",  .f32 },
            .{ "f16",  .f16 },
        });
    };
 
    pub const Number = union(NumberType) {
        //uint:  Uint,
        //int:   Int,
        //float: Float,

        u128: u128,
        u64:  u64,
        u32:  u32,
        u16:  u16,
        u8:   u8,
        i128: i128,
        i64:  i64,
        i32:  i32,
        i16:  i16,
        i8:   i8,
        f128: f128,
        f64:  f64,
        f32:  f32,
        f16:  f16,

        pub fn parse(typ: NumberType, buf: []const u8, base: ?u8) !Number {
            return switch (typ) {
                .u128 => .{ .u128 = try std.fmt.parseUnsigned(u128, buf, base orelse 0) },
                .u64  => .{ .u64  = try std.fmt.parseUnsigned(u64,  buf, base orelse 0) },
                .u32  => .{ .u32  = try std.fmt.parseUnsigned(u32,  buf, base orelse 0) },
                .u16  => .{ .u16  = try std.fmt.parseUnsigned(u16,  buf, base orelse 0) },
                .u8   => .{ .u8   = try std.fmt.parseUnsigned(u8,   buf, base orelse 0) },
                .i128 => .{ .i128 = try std.fmt.parseInt(     i128, buf, base orelse 0) },
                .i64  => .{ .i64  = try std.fmt.parseInt(     i64,  buf, base orelse 0) },
                .i32  => .{ .i32  = try std.fmt.parseInt(     i32,  buf, base orelse 0) },
                .i16  => .{ .i16  = try std.fmt.parseInt(     i16,  buf, base orelse 0) },
                .i8   => .{ .i8   = try std.fmt.parseInt(     i8,   buf, base orelse 0) },
                .f128 => .{ .f128 = try std.fmt.parseFloat(   f128, buf) },
                .f64  => .{ .f64  = try std.fmt.parseFloat(   f64,  buf) },
                .f32  => .{ .f32  = try std.fmt.parseFloat(   f32,  buf) },
                .f16  => .{ .f16  = try std.fmt.parseFloat(   f16,  buf) },
            };
        }

        pub fn cast(num: *Number, typ: NumberType) !void {
            num = switch (num) {
                inline .u128, .u64, .u32, .u16, .u8,
                .i128, .i64, .i32, .i16, .i8 => |v| switch (typ) {
                    .u128 => .{ .u128 = @intCast(v) },
                    .u64  => .{ .u64  = @intCast(v) },
                    .u32  => .{ .u32  = @intCast(v) },
                    .u16  => .{ .u16  = @intCast(v) },
                    .u8   => .{ .u8   = @intCast(v) },
                    .i128 => .{ .i128 = @intCast(v) },
                    .i64  => .{ .i64  = @intCast(v) },
                    .i32  => .{ .i32  = @intCast(v) },
                    .i16  => .{ .i16  = @intCast(v) },
                    .i8   => .{ .i8   = @intCast(v) },
                    .f128 => .{ .f128 = @floatFromInt(v) },
                    .f64  => .{ .f64  = @floatFromInt(v) },
                    .f32  => .{ .f32  = @floatFromInt(v) },
                    .f16  => .{ .f16  = @floatFromInt(v) },
                },
                inline .f128, .f64, .f32, .f16 => |v| switch (typ) {
                    .u128 => .{ .u128 = @intFromFloat(v) },
                    .u64  => .{ .u64  = @intFromFloat(v) },
                    .u32  => .{ .u32  = @intFromFloat(v) },
                    .u16  => .{ .u16  = @intFromFloat(v) },
                    .u8   => .{ .u8   = @intFromFloat(v) },
                    .i128 => .{ .i128 = @intFromFloat(v) },
                    .i64  => .{ .i64  = @intFromFloat(v) },
                    .i32  => .{ .i32  = @intFromFloat(v) },
                    .i16  => .{ .i16  = @intFromFloat(v) },
                    .i8   => .{ .i8   = @intFromFloat(v) },
                    .f128 => .{ .f128 = @floatCast(v) },
                    .f64  => .{ .f64  = @floatCast(v) },
                    .f32  => .{ .f32  = @floatCast(v) },
                    .f16  => .{ .f16  = @floatCast(v) },
                },
            };
        }

        pub inline fn write(num: Number, writer: anytype) !void {
            return switch (num) {
                .u128 => |v| writer.print("u128: {}", .{v}),
                .u64  => |v| writer.print("u64: {}",  .{v}),
                .u32  => |v| writer.print("u32: {}",  .{v}),
                .u16  => |v| writer.print("u16: {}",  .{v}),
                .u8   => |v| writer.print("u8: {}",   .{v}),
                .i128 => |v| writer.print("i128: {}", .{v}),
                .i64  => |v| writer.print("i64: {}",  .{v}),
                .i32  => |v| writer.print("i32: {}",  .{v}),
                .i16  => |v| writer.print("i16: {}",  .{v}),
                .i8   => |v| writer.print("i8: {}",   .{v}),
                .f128 => |v| writer.print("f128: {}", .{v}),
                .f64  => |v| writer.print("f64: {}",  .{v}),
                .f32  => |v| writer.print("f32: {}",  .{v}),
                .f16  => |v| writer.print("f16: {}",  .{v}),
            };
        }

        pub inline fn toString(num: Number, allocator: Allocator) ![]const u8 {
            return switch (num) {
                .u128 => |v| allocPrint(allocator, "u128: {}", .{v}),
                .u64  => |v| allocPrint(allocator, "u64: {}",  .{v}),
                .u32  => |v| allocPrint(allocator, "u32: {}",  .{v}),
                .u16  => |v| allocPrint(allocator, "u16: {}",  .{v}),
                .u8   => |v| allocPrint(allocator, "u8: {}",   .{v}),
                .i128 => |v| allocPrint(allocator, "i128: {}", .{v}),
                .i64  => |v| allocPrint(allocator, "i64: {}",  .{v}),
                .i32  => |v| allocPrint(allocator, "i32: {}",  .{v}),
                .i16  => |v| allocPrint(allocator, "i16: {}",  .{v}),
                .i8   => |v| allocPrint(allocator, "i8: {}",   .{v}),
                .f128 => |v| allocPrint(allocator, "f128: {}", .{v}),
                .f64  => |v| allocPrint(allocator, "f64: {}",  .{v}),
                .f32  => |v| allocPrint(allocator, "f32: {}",  .{v}),
                .f16  => |v| allocPrint(allocator, "f16: {}",  .{v}),
            };
        }
    };

    // zig fmt: on
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
