const std = @import("std");

const Allocator = std.mem.Allocator;

const @"type" = @import("type.zig");
const Types = @"type".Type;
const Type = @"type".Type;

const BuiltinType = @"type".BuiltinType;

const StructuredTypes = @"type".StructuredTypes;
const StructuredType = @"type".StructuredType;
const StructType = @"type".StructType;
const EnumType = @"type".EnumType;
const UnionType = @"type".UnionType;

const CollectionTypes = @"type".CollectionTypes;
const CollectionType = @"type".CollectionType;
const ListType = @"type".ListType;
const SetType = @"type".SetType;
const MapType = @"type".MapType;

const ReferenceTypes = @"type".ReferenceTypes;
const ReferenceType = @"type".ReferenceType;

// ALL VALUES

pub const Value = union(Types) {
    builtin: BuiltinValue,
    structured: StructuredValue,
    collection: CollectionValue,
    reference: *Value,

    pub inline fn isType(self: Value, typ: *const Type) bool {
        if (@as(Types, self) != @as(Types, typ.*)) return false;
        return switch (self) {
            .builtin => |v| v.isBuiltinType(typ.builtin),
            .structured => |v| v.isType(typ),
        };
    }

    pub inline fn isBuiltinType(self: Value, typ: BuiltinType) bool {
        return self == .builtin and self.builtin.isBuiltinType(typ);
    }

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
    maybe: ?*const Value,

    pub inline fn isType(self: BuiltinValue, typ: *const Type) bool {
        if (typ.* != .builtin) return false;
        return self.isBuiltinType(typ.*.builtin);
    }

    pub inline fn isBuiltinType(self: BuiltinValue, typ: BuiltinType) bool {
        return switch (self) {
            .number => |v| v.isBuiltinType(typ),
            .string => typ == .string,
            .maybe => typ == .maybe,
        };
    }

    pub inline fn hash(self: BuiltinValue, allocator: Allocator) u64 {
        return switch (self) {
            .number => |v| v.hash(allocator),
            .string => |v| v.hash(),
            .maybe => |o| if (o) |v| v.hash(allocator) else 0,
        };
    }

    pub inline fn print(self: BuiltinValue, comptime tagged: bool, writer: anytype) !void {
        return switch (self) {
            .number => |v| v.print(tagged, writer),
            .string => |v| v.print(tagged, writer),
            .maybe => |o| maybe: {
                if (tagged) writer.writeAll("maybe: ");
                if (o) |v| break :maybe v.print(tagged, writer);
                break :maybe writer.writeAll("null");
            },
        };
    }
};

// STRUCTURED VALUES

pub const StructuredValue = union(StructuredTypes) {
    @"struct": StructValue,
    @"enum": EnumValue,
    @"union": UnionValue,

    pub inline fn isType(self: StructuredValue, typ: *const Type) bool {
        if (typ.* != .structured) return false;
        return self.isStructuredType(&typ.structured);
    }

    pub inline fn isStructuredType(self: StructuredValue, typ: *const StructuredType) bool {
        if (@as(StructuredTypes, typ.*) != @as(StructuredTypes, self)) return false;
        return switch (self) {
            .@"struct" => |v| @intFromPtr(&typ.@"struct") == @intFromPtr(v.typ),
            .@"enum" => |v| @intFromPtr(&typ.@"enum") == @intFromPtr(v.typ),
            .@"union" => |v| @intFromPtr(&typ.@"union") == @intFromPtr(v.typ),
        };
    }
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

pub const CollectionValue = union(CollectionTypes) {
    list: List,
    set: Set,
    map: Map,

    pub inline fn isType(self: CollectionValue, typ: *const Type) bool {
        if (typ.* != .collection) return false;
        return self.isCollectionType(&typ.collection);
    }

    pub inline fn isCollectionType(self: CollectionValue, typ: *const CollectionType) bool {
        if (@as(CollectionTypes, typ.*) != @as(CollectionTypes, self)) return false;
        return switch (self) {
            .list => |v| @intFromPtr(&typ.list) == @intFromPtr(v.typ),
            .set => |v| @intFromPtr(&typ.set) == @intFromPtr(v.typ),
            .map => |v| @intFromPtr(&typ.map) == @intFromPtr(v.typ),
        };
    }
};

pub const List = struct {
    typ: *const ListType,
    inner: std.ArrayList(Value),

    pub inline fn append(self: *List, v: Value) !void {
        try self.inner.append(v);
    }
};

pub const Set = struct {
    typ: *const SetType,
    inner: std.AutoHashMap(u64, void),

    pub inline fn put(self: *Map, v: Value) !void {
        try self.inner.put(v.hash(self.inner.allocator), void{});
    }

    pub inline fn has(self: *Map, v: Value) bool {
        return self.inner.contains(v.hash(self.inner.allocator));
    }
};

pub const Map = struct {
    typ: *const MapType,
    inner: std.AutoHashMap(u64, Value),

    pub inline fn put(self: *Map, k: Value, v: Value) !void {
        try self.inner.put(k.hash(self.inner.allocator), v);
    }

    pub inline fn has(self: *Map, v: Value) bool {
        return self.inner.contains(v.hash(self.inner.allocator));
    }
};

// REFERENCES

pub const ReferenceValue = union(ReferenceTypes) {
    constant: *const Value,
    mutable: *Value,
};
