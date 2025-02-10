const std = @import("std");

pub inline fn mkStringMap(comptime T: type) std.StaticStringMap(T) {
    const type_info = @typeInfo(T);
    if (type_info != .@"enum") @compileError("mkStringMap with non-enum type");

    const fields = type_info.@"enum".fields;
    comptime var out: [fields.len]struct { []const u8, T } = undefined;

    inline for (fields, 0..) |field, i| {
        out[i] = .{ field.name, @field(T, field.name) };
    }

    return std.StaticStringMap(T).initComptime(out);
}
