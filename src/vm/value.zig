const std = @import("std");

const Allocator = std.mem.Allocator;

const @"type" = @import("type.zig");
const Type = @"type".Type;
const StructType = @"type".StructType;
const EnumType = @"type".EnumType;
const UnionType = @"type".UnionType;

// ALL VALUES

pub const Value = union(enum) {
    builtin: BuiltinValue,
    structured: StructuredValue,
    collection: CollectionValue,
    reference: *Value,

    pub inline fn hash(self: Value, allocator: Allocator) u64 {
        return switch (self) {
            .builtin => |v| v.hash(allocator),
            .reference => |v| @intCast(@intFromPtr(v)),
            else => @panic("unimplemented"),
        };
    }

    pub inline fn print(self: Value, comptime tagged: bool, writer: anytype) !void {
        return switch (self) {
            .builtin => |v| v.print(tagged, writer),
            .reference => |v| {
                if (tagged) writer.writeAll("(ref) ");
                v.print(tagged, writer);
            },
            else => @panic("unimplemented"),
        };
    }
};

// BUILT-IN VALUES

const number = @import("value/number.zig");
pub const NumberType = number.NumberType;
pub const Number = number.Number;

pub const String = @import("value/String.zig");

pub const BuiltinValue = union(enum) {
    number: Number,
    string: String,
    optional: ?*const Value,

    pub inline fn hash(self: BuiltinValue, allocator: Allocator) u64 {
        return switch (self) {
            .number => |v| v.hash(allocator),
            .string => |v| v.hash(),
            .optional => |o| if (o) |v| v.hash(allocator) else 0,
        };
    }

    pub inline fn print(self: BuiltinValue, comptime tagged: bool, writer: anytype) !void {
        return switch (self) {
            .number => |v| v.print(tagged, writer),
            .string => |v| v.print(tagged, writer),
            .optional => |o| optional: {
                if (tagged) writer.writeAll("optional: ");
                if (o) |v| break :optional v.print(tagged, writer);
                break :optional writer.writeAll("null");
            },
        };
    }
};

// STRUCTURED VALUES

pub const StructuredValue = union(enum) {
    @"struct": StructValue,
    @"enum": EnumValue,
    @"union": UnionValue,
};

pub const StructValue = struct {
    typ: *const StructType,
    values: std.StringArrayHashMap(*Value),
};

pub const EnumValue = struct {
    typ: *const EnumType,
    selected: []const u8,
};

pub const UnionValue = struct {
    typ: *const UnionType,
    selected: []const u8,
    value: *Value,
};

// COLLECTION VALUES

pub const CollectionValue = union(enum) {
    map: Map,
};

pub const List = struct {
    inner: std.ArrayList(Value),
    typ: *const Type,

    pub inline fn append(self: *List, v: Value) !void {
        try self.inner.append(v);
    }
};

pub const Set = struct {
    inner: std.AutoHashMap(u64, void),
    typ: *const Type,

    pub inline fn put(self: *Map, v: Value) !void {
        try self.inner.put(v.hash(self.inner.allocator));
    }
};

pub const Map = struct {
    inner: std.AutoHashMap(u64, Value),
    typ: *const Type,

    pub inline fn put(self: *Map, k: Value, v: Value) !void {
        try self.inner.put(k.hash(self.inner.allocator), v);
    }
};
