const X86 = @import("target/X86.zig");
const RISCV = @import("target/RISCV.zig");

pub const Target = enum {
    x86,
    riscv,
};

pub const TargetInfo = union(Target) {
    x86: X86,
    riscv: RISCV,

    const Self = @This();

    pub inline fn bits(self: *const Self) u8 {
        return switch (self) {
            .x86 => |v| if (v.mode == .x86) 32 else 64,
            .riscv => |v| switch (v) {
                .rv32 => 32,
                .rv64 => 64,
                .rv128 => 128,
            },
        };
    }
};
