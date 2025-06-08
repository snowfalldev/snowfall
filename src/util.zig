// STRING -> ENUM MAP GENERATOR

const StaticStringMap = @import("static-map").StaticStringMap;

fn EnumStringMapT(comptime T: type) type {
    const type_info = @typeInfo(T);
    if (type_info != .@"enum") @compileError("enumStringMap with non-enum type");
    return StaticStringMap(T, type_info.@"enum".fields.len);
}

pub inline fn enumStringMap(comptime T: type) EnumStringMapT(T) {
    const MapT = EnumStringMapT(T);

    const fields = @typeInfo(T).@"enum".fields;
    comptime var out: [fields.len]struct { []const u8, T } = undefined;

    inline for (fields, 0..) |field, i| {
        out[i] = .{ field.name, @field(T, field.name) };
    }

    return MapT.initComptime(out, .{ .eval_branch_quota = fields.len * 50 });
}

// FAST STRING HASHMAP

const std = @import("std");

pub fn StringHashMap(comptime V: type) type {
    return std.HashMap([]const u8, V, StringContext, std.hash_map.default_max_load_percentage);
}

pub fn StringHashMapUnmanaged(comptime V: type) type {
    return std.HashMapUnmanaged([]const u8, V, StringContext, std.hash_map.default_max_load_percentage);
}

pub fn StringArrayHashMap(comptime V: type) type {
    return std.ArrayHashMap([]const u8, V, StringArrayContext, true);
}

pub fn StringArrayHashMapUnmanaged(comptime V: type) type {
    return std.ArrayHashMapUnmanaged([]const u8, V, StringArrayContext, true);
}

pub const StringContext = struct {
    pub fn hash(_: @This(), s: []const u8) u64 {
        return hashString(s);
    }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return eqlString(a, b);
    }
};

pub const StringArrayContext = struct {
    pub fn hash(_: @This(), s: []const u8) u32 {
        return @truncate(hashString(s));
    }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return eqlString(a, b);
    }
};

pub fn eqlString(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn hashString(s: []const u8) u64 {
    return std.hash.RapidHash.hash(0, s);
}
