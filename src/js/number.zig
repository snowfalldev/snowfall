const std = @import("std");

pub const epsilon = std.math.floatEps(f64);
pub const max_safe_integer: f64 = @floatFromInt(@exp2(53) - 1);
pub const max_value = std.math.floatMax(f64);
pub const min_safe_integer: f64 = -max_safe_integer;
pub const min_value = std.math.floatMin(f64);
pub const nan = std.math.nan(f64);
pub const positive_infinity = std.math.inf(f64);
pub const negative_infinity: f64 = -positive_infinity;
