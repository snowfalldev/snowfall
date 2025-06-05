const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("types.zig");
const Types = types.Type;
const Type = types.Type;

const BuiltinType = types.BuiltinType;

const StructuredTypes = types.StructuredTypes;
const StructuredType = types.StructuredType;
const StructType = types.StructType;
const EnumType = types.EnumType;
const UnionType = types.UnionType;

const CollectionTypes = types.CollectionTypes;
const CollectionType = types.CollectionType;
const ListType = types.ListType;
const SetType = types.SetType;
const MapType = types.MapType;

const Scope = @import("Scope.zig");
const ast = @import("../ast.zig");

// ALL VALUES

pub const Value = union(Types) {
    builtin: BuiltinValue,
    structured: StructuredValue,
    collection: CollectionValue,
    reference: *Scope.Variable,

    pub inline fn isType(self: Value, typ: *const Type) bool {
        if (@as(Types, self) != @as(Types, typ.*)) return false;
        return switch (self) {
            .builtin => |v| v.isType(typ),
            .structured => |v| v.isType(typ),
        };
    }

    pub inline fn isBuiltinType(self: Value, typ: BuiltinType) bool {
        return self == .builtin and self.builtin.isBuiltinType(typ);
    }

    pub inline fn hash(self: Value) u64 {
        return switch (self) {
            .builtin => |v| v.hash(),
            .reference => |v| @intCast(@intFromPtr(v)),
            else => @panic("unimplemented"),
        };
    }

    pub inline fn print(self: Value, writer: anytype) !void {
        return switch (self) {
            .builtin => |v| v.print(writer),
            .reference => |v| {
                v.lock.lockShared();
                defer v.lock.unlockShared();
                v.value.print(writer);
            },
            else => @panic("unimplemented"),
        };
    }
};

// BUILT-IN VALUES

pub const Number = @import("value/number.zig").Number;
pub const String = @import("value/String.zig");
const unicode = @import("../unicode.zig");

pub const BuiltinValue = union(enum) {
    number: Number,
    string: String,
    char: u21,
    bool: bool,
    void,
    //maybe: ?*const Value,

    // UTILITIES

    pub fn hash(self: BuiltinValue) u64 {
        return switch (self) {
            .number => |v| v.hash(),
            .string => |v| v.hash(),
            .char => |v| v,
            .bool => |v| @intFromBool(v),
            .void => 0,
            //.maybe => |o| if (o) |v| v.hash() else 0,
        };
    }

    pub fn print(self: BuiltinValue, writer: anytype) !void {
        return switch (self) {
            .number => |v| v.print(writer),
            .string => |v| v.print(writer),
            .char => |v| unicode.writeCodepointToUtf8(v, writer),
            .bool => |v| writer.print("{}", .{v}),
            .void => writer.writeAll("void"),
            //.maybe => |o| maybe: {
            //    if (o) |v| break :maybe v.print(writer);
            //    break :maybe writer.writeAll("null");
            //},
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
    selected: usize,
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
        if (!v.isType(self.typ.item)) return error.BadType;
        try self.inner.append(v);
    }
};

pub const Set = struct {
    typ: *const SetType,
    inner: std.AutoHashMap(u64, void),

    pub inline fn put(self: *Set, v: Value) !void {
        if (!v.isType(self.typ.item)) return error.BadType;
        try self.inner.put(v.hash(self.inner.allocator), void{});
    }

    pub inline fn has(self: *Set, v: Value) bool {
        if (!v.isType(self.typ.item)) return false;
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
