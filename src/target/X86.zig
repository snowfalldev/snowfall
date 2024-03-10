const std = @import("std");

mode: enum { x86, amd64, ia64 } = .amd64,
features: std.EnumSet(Feature) = std.EnumSet(Feature).initEmpty(),
era: Era,

pub const Feature = enum {
    mmx,
    fxsr,

    sse,
    sse2,
    sse3,
    ssse3,
};

// Era from which the CPU originates.
// Helpful for tuning, determining feature sets, Mach-O, etc.
pub const Era = enum {
    i386,
    i486,
    i586,
    i686,
};

const Self = @This();
