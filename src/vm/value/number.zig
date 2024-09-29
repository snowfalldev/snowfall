const std = @import("std");
const big = std.math.big;

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

// zig fmt: off

pub const NumberType = enum(u8) {
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

    pub const string_map = std.StaticStringMap(NumberType).initComptime(.{
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
    });
};

fn numTypScalar(comptime typ: NumberType) type {
    return switch (typ) {
        .bigint => @compileError("bigint is not scalar"),
        .i128   => i128,
        .i64    => i64,
        .i32    => i32,
        .i16    => i16,
        .i8     => i8,
        .u128   => u128,
        .u64    => u64,
        .u32    => u32,
        .u16    => u16,
        .u8     => u8,
        .f128   => f128,
        .f64    => f64,
        .f32    => f32,
        .f16    => f16,
    };
}
 
pub const Number = union(NumberType) {
    bigint: big.int.Managed,
    i128:   i128,
    i64:    i64,
    i32:    i32,
    i16:    i16,
    i8:     i8,
    u128:   u128,
    u64:    u64,
    u32:    u32,
    u16:    u16,
    u8:     u8,
    f128:   f128,
    f64:    f64,
    f32:    f32,
    f16:    f16,

    pub fn parse(typ: NumberType, buf: []const u8, base: ?u8, allocator: Allocator) !Number {
        return switch (typ) {
            // zig fmt: on
            .bigint => .{ .bigint = out: {
                const limbs_len = big.int.calcSetStringLimbsBufferLen(base orelse 10, buf.len);
                const limbs = try allocator.alloc(big.Limb, limbs_len);
                var out = big.int.Mutable{
                    .limbs = limbs,
                    .len = 0,
                    .positive = true,
                };
                try out.setString(base orelse 10, buf, limbs, allocator);
                break :out out.toManaged(allocator);
            } },
            // zig fmt: off
            .i128 => .{ .i128 = try std.fmt.parseInt(     i128, buf, base orelse 0) },
            .i64  => .{ .i64  = try std.fmt.parseInt(     i64,  buf, base orelse 0) },
            .i32  => .{ .i32  = try std.fmt.parseInt(     i32,  buf, base orelse 0) },
            .i16  => .{ .i16  = try std.fmt.parseInt(     i16,  buf, base orelse 0) },
            .i8   => .{ .i8   = try std.fmt.parseInt(     i8,   buf, base orelse 0) },
            .u128 => .{ .u128 = try std.fmt.parseUnsigned(u128, buf, base orelse 0) },
            .u64  => .{ .u64  = try std.fmt.parseUnsigned(u64,  buf, base orelse 0) },
            .u32  => .{ .u32  = try std.fmt.parseUnsigned(u32,  buf, base orelse 0) },
            .u16  => .{ .u16  = try std.fmt.parseUnsigned(u16,  buf, base orelse 0) },
            .u8   => .{ .u8   = try std.fmt.parseUnsigned(u8,   buf, base orelse 0) },
            .f128 => .{ .f128 = try std.fmt.parseFloat(   f128, buf) },
            .f64  => .{ .f64  = try std.fmt.parseFloat(   f64,  buf) },
            .f32  => .{ .f32  = try std.fmt.parseFloat(   f32,  buf) },
            .f16  => .{ .f16  = try std.fmt.parseFloat(   f16,  buf) },
        };
    }

    // zig fmt: on
    fn mkBigInt(scalar: anytype, comptime target: type, allocator: Allocator) !big.int.Managed {
        const int: target = switch (@typeInfo(@TypeOf(scalar))) {
            .int => @intCast(scalar),
            .float => @intFromFloat(scalar),
        };

        const limbs_len = big.int.calcLimbLen(int);
        const limbs = allocator.alloc(big.Limb, limbs_len);
        const out = big.int.Mutable.init(limbs, int);
        return out.toManaged(allocator);
    }

    inline fn bigIntMod(lhs: big.int.Managed, rhs: big.int.Managed, allocator: Allocator) !big.int.Managed {
        var d = try mkBigInt(0, u1, allocator);
        var m = try mkBigInt(0, u1, allocator);
        var o = lhs;

        d.divFloor(&lhs, &rhs);
        m.mul(&d, &rhs);
        o.sub(&lhs, &m);

        d.deinit();
        m.deinit();
        return o;
    }
    // zig fmt: off

    pub fn cast(self: Number, typ: NumberType, allocator: Allocator) !Number {
        return switch (self) {
            .bigint => |v| switch (typ) {
                .bigint => .{ .bigint = v },
                inline .i128, .i64, .i32, .i16, .i8 => int: {
                    const max: i129 = std.math.maxInt(numTypScalar(typ));
                    const maxBigInt = try mkBigInt(max, i129, allocator);
                    const reduced = try bigIntMod(v, maxBigInt, allocator);
                    const val = try reduced.to(i128);
                    maxBigInt.deinit();
                    reduced.deinit();
                    
                    break :int switch (typ) {
                        .i128 => .{ .i128 = val },
                        .i64  => .{ .i64  = @intCast(val) },
                        .i32  => .{ .i32  = @intCast(val) },
                        .i16  => .{ .i16  = @intCast(val) },
                        .i8   => .{ .i8   = @intCast(val) },
                    };
                },
                inline .u128, .u64, .u32, .u16, .u8 => uint: {
                    const max: u128 = std.math.maxInt(numTypScalar(typ));
                    const maxBigInt = try mkBigInt(max, u128, allocator);
                    const reduced = try bigIntMod(v, maxBigInt, allocator);
                    maxBigInt.deinit();
                    var val: u128 = 0;
                    
                    if (!v.isPositive()) {
                        // emulate overflow
                        const tmp = reduced.to(i129) + 1;
                        const imax: i129 = @intCast(max);
                        val = @truncate(@as(u129, @intCast(imax + tmp)));
                    } else val = try reduced.to(u128);
                    reduced.deinit();

                    break :uint switch (typ) {
                        .u128 => .{ .u128 = val },
                        .u64  => .{ .u64  = @truncate(val) },
                        .u32  => .{ .u32  = @truncate(val) },
                        .u16  => .{ .u16  = @truncate(val) },
                        .u8   => .{ .u8   = @truncate(val) },
                    };
                },
                inline .f128, .f64, .f32, .f16 => float: {
                    const max: i16385 = @intFromFloat(std.math.floatMax(numTypScalar(typ)));
                    const maxBigInt = try mkBigInt(max, i16385, allocator);
                    const reduced = try bigIntMod(v, maxBigInt, allocator);
                    const val: f128 = @floatFromInt(try reduced.to(i16385));
                    maxBigInt.deinit();
                    reduced.deinit();

                    break :float switch (typ) {
                        .f128 => .{ .f128 = val },
                        .f64  => .{ .f64  = @floatCast(val) },
                        .f32  => .{ .f32  = @floatCast(val) },
                        .f16  => .{ .f16  = @floatCast(val) },
                    };
                }
            },
            inline .i128, .i64, .i32, .i16, .i8,
            .u128, .u64, .u32, .u16, .u8 => |v| switch (typ) {
                .bigint => .{ .bigint = try mkBigInt(v, i129, allocator) },
                .i128 => .{ .i128 = @intCast(v) },
                .i64  => .{ .i64  = @intCast(v) },
                .i32  => .{ .i32  = @intCast(v) },
                .i16  => .{ .i16  = @intCast(v) },
                .i8   => .{ .i8   = @intCast(v) },
                .u128 => .{ .u128 = @intCast(v) },
                .u64  => .{ .u64  = @intCast(v) },
                .u32  => .{ .u32  = @intCast(v) },
                .u16  => .{ .u16  = @intCast(v) },
                .u8   => .{ .u8   = @intCast(v) },
                .f128 => .{ .f128 = @floatFromInt(v) },
                .f64  => .{ .f64  = @floatFromInt(v) },
                .f32  => .{ .f32  = @floatFromInt(v) },
                .f16  => .{ .f16  = @floatFromInt(v) },
            },
            inline .f128, .f64, .f32, .f16 => |v| switch (typ) {
                .bigint => .{ .bigint = try mkBigInt(v, i16385, allocator) },
                .i128 => .{ .i128 = @intFromFloat(v) },
                .i64  => .{ .i64  = @intFromFloat(v) },
                .i32  => .{ .i32  = @intFromFloat(v) },
                .i16  => .{ .i16  = @intFromFloat(v) },
                .i8   => .{ .i8   = @intFromFloat(v) },
                .u128 => .{ .u128 = @intFromFloat(v) },
                .u64  => .{ .u64  = @intFromFloat(v) },
                .u32  => .{ .u32  = @intFromFloat(v) },
                .u16  => .{ .u16  = @intFromFloat(v) },
                .u8   => .{ .u8   = @intFromFloat(v) },
                .f128 => .{ .f128 = @floatCast(v) },
                .f64  => .{ .f64  = @floatCast(v) },
                .f32  => .{ .f32  = @floatCast(v) },
                .f16  => .{ .f16  = @floatCast(v) },
            },
        };
    }

    pub inline fn write(self: Number, tagged: bool, writer: anytype) !void {
        if (tagged) try writer.print("{s}: ", .{@tagName(self)});
        return switch (self) {
            // zig fmt: on
            .bigint => |v| err: {
                try writer.writeAll("bigint: ");
                const str = try v.toString(v.allocator, 10, .upper);
                defer v.allocator.free(str);
                break :err writer.writeAll(str);
            },
            // zig fmt: off
            inline else => |v| writer.print("{}", .{v}),
        };
    }

    // zig fmt: on

    pub inline fn toString(self: Number, tagged: bool, allocator: Allocator) ![]const u8 {
        var arraylist = std.ArrayList(u8).init(allocator);
        try self.write(tagged, arraylist.writer());
        return arraylist.toOwnedSlice();
    }
};
