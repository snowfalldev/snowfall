const std = @import("std");
const big = std.math.big;

const allocator = @import("gc").allocator();

const ast = @import("../../ast.zig");
const util = @import("../../util.zig");

pub const NumberType = enum(u8) {
    // zig fmt: off
    int     = 0x00,
    float   = 0x10,
    // zig fmt: on

    pub const string_map = util.mkStringMap(NumberType);
};

pub const Number = union(NumberType) {
    // zig fmt: off
    int:    big.int.Const,
    float:  f64,
    // zig fmt: on

    // UTILITIES

    pub fn from(value: anytype) !Number {
        return switch (@typeInfo(@TypeOf(value))) {
            .int, .comptime_int => int: {
                var m = try big.int.Managed.init(allocator);
                try m.set(value);
                break :int .{ .int = m.toConst() };
            },
            .float, .comptime_float => .{ .float = @floatCast(value) },
            else => @compileError("Number.from with non-number value"),
        };
    }

    pub fn deinit(self: Number) void {
        if (self == .float) return;
        allocator.free(self.int.limbs);
    }

    pub fn hash(self: Number) u64 {
        return switch (self) {
            .int => |v| @bitCast(v),
            .float => |v| if (@trunc(v) == v) int: {
                const asInt: i64 = @intFromFloat(v);
                break :int @bitCast(asInt);
            } else @bitCast(v),
        };
    }

    pub fn print(self: Number, writer: anytype) !void {
        return switch (self) {
            .int => |v| writer.print("{}", .{v}),
            .float => |v| writer.print("{d}", .{v}),
        };
    }

    // CASTING

    pub fn cast(self: Number, typ: NumberType) !Number {
        if (@as(NumberType, self) == typ) return self;
        return switch (self) {
            .int => |v| .{ .float = v.toFloat(f64) },
            .float => |v| float: {
                const buf = std.fmt.allocPrint(allocator, "{d}", .{@trunc(v)});
                defer allocator.free(buf);
                var m = try big.int.Managed.init(allocator);
                try m.setString(10, buf);
                break :float .{ .int = m.toConst() };
            },
        };
    }

    // VALUE OPERATIONS

    threadlocal var int_cache_inited = false;
    threadlocal var int_cache: [2]big.int.Managed = undefined;
    inline fn intCache() !*[2]big.int.Managed {
        if (!int_cache_inited) {
            int_cache[0] = try big.int.Managed.init(allocator);
            int_cache[1] = try big.int.Managed.init(allocator);
        }
        return &int_cache;
    }

    inline fn mkConstManaged(c: big.int.Const) big.int.Managed {
        var m = big.int.Managed{
            .allocator = allocator,
            .limbs = c.limbs,
            .metadata = 0,
        };
        m.setMetadata(c.positive, c.limbs.len);
        return m;
    }

    const Operands = struct {
        lhs: Number,
        rhs: Number,
    };

    inline fn castOperands(lhs: Number, rhs: Number) Operands {
        const lhst = @as(NumberType, lhs);
        const rhst = @as(NumberType, rhs);

        if (lhst == rhst) return .{ lhs, rhs };

        const lhsi = @intFromEnum(lhst);
        const rhsi = @intFromEnum(rhst);

        if (lhsi > rhsi) return .{ lhs, rhs.cast(lhst) };
        if (rhsi > lhsi) return .{ lhs.cast(rhst), rhs };
    }

    pub inline fn boolify(self: Number) bool {
        return switch (self) {
            .int => |v| v.orderAgainstScalar(0) == .gt,
            .float => |v| v > 0,
        };
    }

    pub fn chain(l: Number, r: Number, op: ast.Chainer) bool {
        const operands = castOperands(l, r);
        const lhs_val = operands.lhs.boolify();
        const rhs_val = operands.rhs.boolify();

        return switch (op) {
            .@"and" => lhs_val and rhs_val,
            .@"or" => lhs_val or rhs_val,
        };
    }

    pub fn cmp(l: Number, r: Number, op: ast.Comparator) bool {
        const operands = castOperands(l, r);
        return switch (operands.lhs) {
            .int => |lhs| int: {
                const rhs = operands.rhs.int;
                const order = lhs.order(rhs);

                break :int switch (op) {
                    .eq => order == .eq,
                    .ne => order != .eq,
                    .gt => order == .gt,
                    .lt => order == .lt,
                    .gt_eq => order != .lt,
                    .lt_eq => order != .gt,
                };
            },
            .float => |lhs| float: {
                const rhs = operands.rhs.float;

                break :float switch (op) {
                    .eq => lhs == rhs,
                    .ne => lhs != rhs,
                    .gt => lhs > rhs,
                    .lt => lhs < rhs,
                    .gt_eq => lhs >= rhs,
                    .lt_eq => lhs <= rhs,
                };
            },
        };
    }

    pub fn math(l: Number, r: Number, op: ast.MathOp) !Number {
        const operands = castOperands(l, r);
        return switch (operands.lhs) {
            .int => |lhs| int: {
                const rhs = operands.rhs.int;
                const tmp = try intCache();

                const a = mkConstManaged(lhs);
                const b = mkConstManaged(rhs);

                switch (op) {
                    .add => try tmp[0].add(&a, &b),
                    .sub => try tmp[0].sub(&a, &b),
                    .mul => try tmp[0].mul(&a, &b),
                    .div => try tmp[0].divFloor(&tmp[1], &a, &b),
                    .mod => try tmp[1].divFloor(&tmp[0], &a, &b),
                }

                const limbs = try allocator.dupe(big.Limb, tmp[0].limbs[0..tmp[0].len()]);
                break :int .{ .int = .{ .limbs = limbs, .positive = tmp[0].isPositive() } };
            },
            .float => |lhs| float: {
                const rhs = operands.rhs.float;

                const value = switch (op) {
                    .add => lhs + rhs,
                    .sub => lhs - rhs,
                    .mul => lhs * rhs,
                    .div => lhs / rhs,
                    .mod => lhs % rhs,
                };

                break :float .{ .float = value };
            },
        };
    }

    pub fn bitwise(l: Number, r: Number, op: ast.BitwiseOp) !Number {
        if (l == .float or r == .float) return error.BitwiseWithFloat;

        const lhs = mkConstManaged(l.int);
        const rhs = mkConstManaged(r.int);

        if ((op == .ls or op == .rs) and !rhs.fits(usize))
            return error.BadBitwiseRHS;

        var tmp = try big.int.Managed.init(allocator);

        switch (op) {
            .ls => tmp.shiftLeft(&lhs, rhs.toInt(usize)),
            .rs => tmp.shiftRight(&lhs, rhs.toInt(usize)),
            .@"and" => tmp.bitAnd(&lhs, &rhs),
            .@"or" => tmp.bitOr(&lhs, &rhs),
            .xor => tmp.bitXor(&lhs, &rhs),
        }

        return .{ .int = tmp.toConst() };
    }
};
