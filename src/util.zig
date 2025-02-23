const StaticStringMap = @import("static-map").StaticStringMap;

fn MkStringMapRetT(comptime T: type) type {
    const type_info = @typeInfo(T);
    if (type_info != .@"enum") @compileError("mkStringMap with non-enum type");
    return StaticStringMap(T, type_info.@"enum".fields.len);
}

pub inline fn mkStringMap(comptime T: type) MkStringMapRetT(T) {
    const type_info = @typeInfo(T);
    if (type_info != .@"enum") @compileError("mkStringMap with non-enum type");

    const fields = type_info.@"enum".fields;
    comptime var out: [fields.len]struct { []const u8, T } = undefined;

    inline for (fields, 0..) |field, i| {
        out[i] = .{ field.name, @field(T, field.name) };
    }

    return StaticStringMap(T, fields.len).initComptime(out);
}
