const std = @import("std");
const big = std.math.big;

const Allocator = std.mem.Allocator;
const allocPrint = std.fmt.allocPrint;

const util = @import("../../util.zig");

const @"type" = @import("../type.zig");
const BuiltinType = @"type".BuiltinType;

pub const NumberType = enum(u8) {
    // zig fmt: off
    
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
    f64    = 0x24,
    f32    = 0x23,
    f16    = 0x22,

    pub const string_map = util.mkStringMap(NumberType);

    // zig fmt: on

    pub fn isBuiltinType(typ: NumberType, btyp: BuiltinType) bool {
        if (btyp == .number) return true;
        if (@intFromEnum(typ) == @intFromEnum(btyp)) return true;
        return switch (typ) {
            .bigint, .i128, .i64, .i32, .i16, .i8 => btyp == .int,
            .u128, .u64, .u32, .u16, .u8 => btyp == .uint,
            .f64, .f32, .f16 => btyp == .float,
        };
    }
};

fn numTypScalar(comptime typ: NumberType) type {
    return switch (typ) {
        // zig fmt: off
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
        .f64    => f64,
        .f32    => f32,
        .f16    => f16,
        // zig fmt: on
    };
}

pub const Number = union(NumberType) {
    // zig fmt: off
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
    f64:    f64,
    f32:    f32,
    f16:    f16,
    // zig fmt: on

    pub fn parse(typ: NumberType, buf: []const u8, base: u8, allocator: Allocator) !Number {
        return switch (typ) {
            .bigint => {
                const limbs_len = big.int.calcSetStringLimbsBufferLen(base, buf.len);
                const limbs = try allocator.alloc(big.Limb, limbs_len);
                var out = big.int.Mutable{ .limbs = limbs, .len = 0, .positive = true };
                try out.setString(base, buf, limbs, allocator);
                return .{ .bigint = out.toManaged(allocator) };
            },
            // zig fmt: off
            .i128 => .{ .i128 = try std.fmt.parseInt(     i128, buf, base) },
            .i64  => .{ .i64  = try std.fmt.parseInt(     i64,  buf, base) },
            .i32  => .{ .i32  = try std.fmt.parseInt(     i32,  buf, base) },
            .i16  => .{ .i16  = try std.fmt.parseInt(     i16,  buf, base) },
            .i8   => .{ .i8   = try std.fmt.parseInt(     i8,   buf, base) },
            .u128 => .{ .u128 = try std.fmt.parseUnsigned(u128, buf, base) },
            .u64  => .{ .u64  = try std.fmt.parseUnsigned(u64,  buf, base) },
            .u32  => .{ .u32  = try std.fmt.parseUnsigned(u32,  buf, base) },
            .u16  => .{ .u16  = try std.fmt.parseUnsigned(u16,  buf, base) },
            .u8   => .{ .u8   = try std.fmt.parseUnsigned(u8,   buf, base) },
            .f64  => .{ .f64  = try std.fmt.parseFloat(   f64,  buf) },
            .f32  => .{ .f32  = try std.fmt.parseFloat(   f32,  buf) },
            .f16  => .{ .f16  = try std.fmt.parseFloat(   f16,  buf) },
            // zig fmt: on
        };
    }

    pub fn deinit(num: *Number) void {
        if (num != .bigint) return;
        num.bigint.deinit();
    }

    pub inline fn isBuiltinType(num: Number, typ: BuiltinType) bool {
        return @as(NumberType, num).isBuiltinType(typ);
    }

    // HASHING

    inline fn hashU128(v: u128, comptime stable: bool) u64 {
        const lo: u64 = @truncate(v);
        const hi: u64 = @truncate(v >> 64);
        if (stable and hi == 0) return lo;
        return std.math.rotr(u64, lo ^ std.math.rotl(u64, hi, 5), 3);
    }

    inline fn hashI128(v: i128, comptime fits_uint: bool) u64 {
        // stability with uint types
        if (fits_uint and v >= 0) {
            if (v <= std.math.maxInt(u64))
                return @truncate(@as(u128, @intCast(v)));
            return hashU128(@bitCast(v), true);
        }
        // stability with smaller int types
        if (v >= std.math.minInt(i64) and v <= std.math.maxInt(i64))
            return @bitCast(@as(i64, @intCast(v)));

        const uv: u128 = @bitCast(v);
        const lo: u64 = @truncate(uv);
        const hi: u64 = @truncate(uv >> 64);
        return std.math.rotl(u64, (lo *% 3) ^ std.math.rotr(u64, hi, 3), 5);
    }

    const hash_seed_str: []align(8) const u8 = @alignCast("numberss");
    const hash_seed = @as(*const u64, @ptrCast(hash_seed_str)).*;

    pub fn hash(num: Number, allocator: Allocator) !u64 {
        return switch (num) {
            .bigint => |v| bigint: {
                if (v.fits(u128)) break :bigint hashU128(v.to(u128) catch unreachable, true);
                if (v.fits(i128)) break :bigint hashI128(v.to(i128) catch unreachable, false);
                const str = try v.toString(allocator, 16, .upper);
                defer allocator.free(str);
                break :bigint std.hash.Wyhash.hash(hash_seed, str);
            },

            inline .i64, .i32, .i16, .i8 => |v| @bitCast(@as(i64, @intCast(v))),
            .i128 => |v| hashI128(v, true),
            inline .u64, .u32, .u16, .u8 => |v| @intCast(v),
            .u128 => |v| hashU128(v, false),

            inline .f64, .f32, .f16 => |v, t| float: {
                // stability with ints
                if (@trunc(v) == v) {
                    const T = numTypScalar(t);

                    if (v >= 0.0) {
                        const upper: T = @floatFromInt(std.math.maxInt(u128));
                        if (v <= upper) break :float hashU128(@intFromFloat(v), true);
                    } else {
                        const lower: T = @floatFromInt(std.math.minInt(i128));
                        if (v >= lower) break :float hashI128(@intFromFloat(v), false);
                    }

                    // stability with bigints (may be slow)
                    const str = try allocPrint(allocator, "{d}", .{v});
                    defer allocator.free(str);
                    break :float std.hash.Wyhash.hash(hash_seed, str);
                }

                const uTyp: NumberType = @enumFromInt(@intFromEnum(t) - 0x10);
                const asU128: u128 = @intCast(@as(numTypScalar(uTyp), @bitCast(v)));
                // this could use a little *magic* ("float :3" + the meaning of life)
                break :float hashU128(asU128 ^ (0xF10A7083 << 42), false);
            },
        };
    }

    // CASTING

    // zig fmt: on
    fn mkBigInt(scalar: anytype, comptime target: type, allocator: Allocator) !big.int.Managed {
        const int: target = switch (@typeInfo(@TypeOf(scalar))) {
            .int, .comptime_int => @intCast(scalar),
            .float, .comptime_float => @intFromFloat(scalar),
            else => @panic("non-number in mkBigInt"),
        };

        const limbs_len = big.int.calcLimbLen(int);
        const limbs = try allocator.alloc(big.Limb, limbs_len);
        const out = big.int.Mutable.init(limbs, int);
        return out.toManaged(allocator);
    }

    inline fn bigIntMod(lhs: big.int.Managed, rhs: big.int.Managed, allocator: Allocator) !big.int.Managed {
        var d = try mkBigInt(0, u1, allocator);
        var o = try mkBigInt(0, u1, allocator);
        try d.divFloor(&o, &lhs, &rhs);
        d.deinit();
        return o;
    }

    pub fn cast(self: Number, typ: NumberType, allocator: Allocator) !Number {
        return switch (self) {
            .bigint => |v| switch (typ) {
                .bigint => .{ .bigint = v },
                inline .i128, .i64, .i32, .i16, .i8 => |t| int: {
                    const max: i129 = std.math.maxInt(numTypScalar(t));
                    var maxBigInt = try mkBigInt(max, i129, allocator);
                    var reduced = try bigIntMod(v, maxBigInt, allocator);
                    const val = try reduced.to(i128);
                    maxBigInt.deinit();
                    reduced.deinit();

                    break :int switch (typ) {
                        // zig fmt: off
                        .i128 => .{ .i128 = val },
                        .i64  => .{ .i64  = @intCast(val) },
                        .i32  => .{ .i32  = @intCast(val) },
                        .i16  => .{ .i16  = @intCast(val) },
                        .i8   => .{ .i8   = @intCast(val) },
                        else  => unreachable,
                        // zig fmt: on
                    };
                },
                inline .u128, .u64, .u32, .u16, .u8 => |t| uint: {
                    const max: u128 = std.math.maxInt(numTypScalar(t));
                    var maxBigInt = try mkBigInt(max, u128, allocator);
                    var reduced = try bigIntMod(v, maxBigInt, allocator);
                    maxBigInt.deinit();
                    var val: u128 = 0;

                    if (!v.isPositive()) {
                        // emulate overflow
                        const tmp = (try reduced.to(i129)) + 1;
                        const imax: i129 = @intCast(max);
                        val = @truncate(@as(u129, @intCast(imax + tmp)));
                    } else val = try reduced.to(u128);
                    reduced.deinit();

                    break :uint switch (typ) {
                        // zig fmt: off
                        .u128 => .{ .u128 = val },
                        .u64  => .{ .u64  = @truncate(val) },
                        .u32  => .{ .u32  = @truncate(val) },
                        .u16  => .{ .u16  = @truncate(val) },
                        .u8   => .{ .u8   = @truncate(val) },
                        else  => unreachable,
                        // zig fmt: on
                    };
                },
                inline .f64, .f32, .f16 => |t| float: {
                    const max: i1025 = @intFromFloat(std.math.floatMax(numTypScalar(t)));
                    var maxBigInt = try mkBigInt(max, i1025, allocator);
                    var reduced = try bigIntMod(v, maxBigInt, allocator);
                    const val: f64 = @floatFromInt(try reduced.to(i1025));
                    maxBigInt.deinit();
                    reduced.deinit();

                    break :float switch (typ) {
                        // zig fmt: off
                        .f64  => .{ .f64  = val },
                        .f32  => .{ .f32  = @floatCast(val) },
                        .f16  => .{ .f16  = @floatCast(val) },
                        else  => unreachable,
                        // zig fmt: on
                    };
                },
            },
            inline .i128, .i64, .i32, .i16, .i8, .u128, .u64, .u32, .u16, .u8 => |v| switch (typ) {
                .bigint => .{ .bigint = try mkBigInt(v, i129, allocator) },
                // zig fmt: off
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
                .f64  => .{ .f64  = @floatFromInt(v) },
                .f32  => .{ .f32  = @floatFromInt(v) },
                .f16  => .{ .f16  = @floatFromInt(v) },
                // zig fmt: on
            },
            inline .f64, .f32, .f16 => |v| switch (typ) {
                .bigint => .{ .bigint = try mkBigInt(v, i1025, allocator) },
                // zig fmt: off
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
                .f64  => .{ .f64  = @floatCast(v) },
                .f32  => .{ .f32  = @floatCast(v) },
                .f16  => .{ .f16  = @floatCast(v) },
                // zig fmt: on
            },
        };
    }

    pub inline fn print(self: Number, comptime tagged: bool, writer: anytype) !void {
        if (tagged) try writer.print("{s}: ", .{@tagName(self)});
        return switch (self) {
            .bigint => |v| err: {
                const str = try v.toString(v.allocator, 10, .upper);
                defer v.allocator.free(str);
                break :err writer.writeAll(str);
            },
            inline else => |v| writer.print("{}", .{v}),
        };
    }

    pub inline fn toString(self: Number, comptime tagged: bool, allocator: Allocator) ![]const u8 {
        var arraylist = std.ArrayList(u8).init(allocator);
        try self.print(tagged, arraylist.writer());
        return arraylist.toOwnedSlice();
    }

    // VALUE OPERATIONS

    const Operands = struct {
        lhs: Number,
        rhs: Number,
    };

    fn castOperands(lhs: Number, rhs: Number, allocator: Allocator) Operands {
        const lhst = @as(NumberType, lhs);
        const rhst = @as(NumberType, rhs);
        const lhsi = @intFromEnum(lhst);
        const rhsi = @intFromEnum(rhst);

        if (lhsi > rhsi) return .{ lhs, rhs.cast(lhst, allocator) };
        if (rhsi > lhsi) return .{ lhs.cast(rhst, allocator), rhs };
        return .{ lhs, rhs };
    }

    pub fn add(l: Number, r: Number, allocator: Allocator) Number {
        const operands = castOperands(l, r, allocator);
        return switch (operands.lhs) {
            .bigint => bigint: {
                var o = try mkBigInt(0, u1, allocator);
                o.add(&operands.lhs.bigint, &operands.rhs.bigint);
                break :bigint .{ .bigint = o };
            },
            // zig fmt: off
            .i128 => .{ .i128 = operands.lhs.i128 + operands.rhs.i128 },
            .i64  => .{ .i64  = operands.lhs.i64  + operands.rhs.i64  },
            .i32  => .{ .i32  = operands.lhs.i32  + operands.rhs.i32  },
            .i16  => .{ .i16  = operands.lhs.i16  + operands.rhs.i16  },
            .i8   => .{ .i8   = operands.lhs.i8   + operands.rhs.i8   },
            .u128 => .{ .u128 = operands.lhs.u128 + operands.rhs.u128 },
            .u64  => .{ .u64  = operands.lhs.u64  + operands.rhs.u64  },
            .u32  => .{ .u32  = operands.lhs.u32  + operands.rhs.u32  },
            .u16  => .{ .u16  = operands.lhs.u16  + operands.rhs.u16  },
            .u8   => .{ .u8   = operands.lhs.u8   + operands.rhs.u8   },
            .f64  => .{ .f64  = operands.lhs.f64  + operands.rhs.f64  },
            .f32  => .{ .f32  = operands.lhs.f32  + operands.rhs.f32  },
            .f16  => .{ .f16  = operands.lhs.f16  + operands.rhs.f16  },
            // zig fmt: on
        };
    }

    pub fn sub(l: Number, r: Number, allocator: Allocator) Number {
        const operands = castOperands(l, r, allocator);
        return switch (operands.lhs) {
            .bigint => bigint: {
                var o = try mkBigInt(0, u1, allocator);
                o.sub(&operands.lhs.bigint, &operands.rhs.bigint);
                break :bigint .{ .bigint = o };
            },
            // zig fmt: off
            .i128 => .{ .i128 = operands.lhs.i128 - operands.rhs.i128 },
            .i64  => .{ .i64  = operands.lhs.i64  - operands.rhs.i64  },
            .i32  => .{ .i32  = operands.lhs.i32  - operands.rhs.i32  },
            .i16  => .{ .i16  = operands.lhs.i16  - operands.rhs.i16  },
            .i8   => .{ .i8   = operands.lhs.i8   - operands.rhs.i8   },
            .u128 => .{ .u128 = operands.lhs.u128 - operands.rhs.u128 },
            .u64  => .{ .u64  = operands.lhs.u64  - operands.rhs.u64  },
            .u32  => .{ .u32  = operands.lhs.u32  - operands.rhs.u32  },
            .u16  => .{ .u16  = operands.lhs.u16  - operands.rhs.u16  },
            .u8   => .{ .u8   = operands.lhs.u8   - operands.rhs.u8   },
            .f64  => .{ .f64  = operands.lhs.f64  - operands.rhs.f64  },
            .f32  => .{ .f32  = operands.lhs.f32  - operands.rhs.f32  },
            .f16  => .{ .f16  = operands.lhs.f16  - operands.rhs.f16  },
            // zig fmt: on
        };
    }

    pub fn mul(l: Number, r: Number, allocator: Allocator) Number {
        const operands = castOperands(l, r, allocator);
        return switch (operands.lhs) {
            .bigint => bigint: {
                var o = try mkBigInt(0, u1, allocator);
                o.mul(&operands.lhs.bigint, &operands.rhs.bigint);
                break :bigint .{ .bigint = o };
            },
            // zig fmt: off
            .i128 => .{ .i128 = operands.lhs.i128 * operands.rhs.i128 },
            .i64  => .{ .i64  = operands.lhs.i64  * operands.rhs.i64  },
            .i32  => .{ .i32  = operands.lhs.i32  * operands.rhs.i32  },
            .i16  => .{ .i16  = operands.lhs.i16  * operands.rhs.i16  },
            .i8   => .{ .i8   = operands.lhs.i8   * operands.rhs.i8   },
            .u128 => .{ .u128 = operands.lhs.u128 * operands.rhs.u128 },
            .u64  => .{ .u64  = operands.lhs.u64  * operands.rhs.u64  },
            .u32  => .{ .u32  = operands.lhs.u32  * operands.rhs.u32  },
            .u16  => .{ .u16  = operands.lhs.u16  * operands.rhs.u16  },
            .u8   => .{ .u8   = operands.lhs.u8   * operands.rhs.u8   },
            .f64  => .{ .f64  = operands.lhs.f64  * operands.rhs.f64  },
            .f32  => .{ .f32  = operands.lhs.f32  * operands.rhs.f32  },
            .f16  => .{ .f16  = operands.lhs.f16  * operands.rhs.f16  },
            // zig fmt: on
        };
    }

    pub fn div(l: Number, r: Number, allocator: Allocator) Number {
        const operands = castOperands(l, r, allocator);
        return switch (operands.lhs) {
            .bigint => bigint: {
                var o = try mkBigInt(0, u1, allocator);
                o.divTrunc(&operands.lhs.bigint, &operands.rhs.bigint);
                break :bigint .{ .bigint = o };
            },
            // zig fmt: off
            .i128 => .{ .i128 = operands.lhs.i128 / operands.rhs.i128 },
            .i64  => .{ .i64  = operands.lhs.i64  / operands.rhs.i64  },
            .i32  => .{ .i32  = operands.lhs.i32  / operands.rhs.i32  },
            .i16  => .{ .i16  = operands.lhs.i16  / operands.rhs.i16  },
            .i8   => .{ .i8   = operands.lhs.i8   / operands.rhs.i8   },
            .u128 => .{ .u128 = operands.lhs.u128 / operands.rhs.u128 },
            .u64  => .{ .u64  = operands.lhs.u64  / operands.rhs.u64  },
            .u32  => .{ .u32  = operands.lhs.u32  / operands.rhs.u32  },
            .u16  => .{ .u16  = operands.lhs.u16  / operands.rhs.u16  },
            .u8   => .{ .u8   = operands.lhs.u8   / operands.rhs.u8   },
            .f64  => .{ .f64  = operands.lhs.f64  / operands.rhs.f64  },
            .f32  => .{ .f32  = operands.lhs.f32  / operands.rhs.f32  },
            .f16  => .{ .f16  = operands.lhs.f16  / operands.rhs.f16  },
            // zig fmt: on
        };
    }

    pub fn mod(l: Number, r: Number, allocator: Allocator) Number {
        const operands = castOperands(l, r, allocator);
        return switch (operands.lhs) {
            .bigint => .{ .bigint = bigIntMod(operands.lhs.bigint, operands.rhs.bigint, allocator) },
            // zig fmt: off
            .i128 => .{ .i128 = operands.lhs.i128 % operands.rhs.i128 },
            .i64  => .{ .i64  = operands.lhs.i64  % operands.rhs.i64  },
            .i32  => .{ .i32  = operands.lhs.i32  % operands.rhs.i32  },
            .i16  => .{ .i16  = operands.lhs.i16  % operands.rhs.i16  },
            .i8   => .{ .i8   = operands.lhs.i8   % operands.rhs.i8   },
            .u128 => .{ .u128 = operands.lhs.u128 % operands.rhs.u128 },
            .u64  => .{ .u64  = operands.lhs.u64  % operands.rhs.u64  },
            .u32  => .{ .u32  = operands.lhs.u32  % operands.rhs.u32  },
            .u16  => .{ .u16  = operands.lhs.u16  % operands.rhs.u16  },
            .u8   => .{ .u8   = operands.lhs.u8   % operands.rhs.u8   },
            .f64  => .{ .f64  = operands.lhs.f64  % operands.rhs.f64  },
            .f32  => .{ .f32  = operands.lhs.f32  % operands.rhs.f32  },
            .f16  => .{ .f16  = operands.lhs.f16  % operands.rhs.f16  },
            // zig fmt: on
        };
    }
};
