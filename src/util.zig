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
