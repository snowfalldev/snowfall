const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) anyerror!void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = null,
    });

    var arch = builtin.cpu.arch;
    if (target.cpu_arch != null) {
        arch = target.cpu_arch.?;
    }

    var os = builtin.os.tag;
    if (target.os_tag != null) {
        os = target.os_tag.?;
    }

    var config_h = try std.fs.cwd().createFile("deps/qbe/config.h", .{});

    if (os.isDarwin()) {
        switch (arch) {
            std.Target.Cpu.Arch.aarch64 => try config_h.writeAll("#define Deftgt T_arm64_apple"),
            std.Target.Cpu.Arch.x86_64 => try config_h.writeAll("#define Deftgt T_amd64_apple"),
            else => @panic("architecture not supported"),
        }
    } else {
        switch (arch) {
            std.Target.Cpu.Arch.aarch64 => try config_h.writeAll("#define Deftgt T_arm64"),
            std.Target.Cpu.Arch.riscv64 => try config_h.writeAll("#define Deftgt T_rv64"),
            std.Target.Cpu.Arch.x86_64 => try config_h.writeAll("#define Deftgt T_amd64_sysv"),
            else => @panic("architecture not supported"),
        }
    }

    const qbe = b.addExecutable(.{
        .name = "qbe",
    });
    qbe.linkLibC();
    qbe.addCSourceFiles(.{
        .files = &[_][]const u8{
            "deps/qbe/main.c",
            "deps/qbe/util.c",
            "deps/qbe/parse.c",
            "deps/qbe/abi.c",
            "deps/qbe/cfg.c",
            "deps/qbe/mem.c",
            "deps/qbe/ssa.c",
            "deps/qbe/alias.c",
            "deps/qbe/load.c",
            "deps/qbe/copy.c",
            "deps/qbe/fold.c",
            "deps/qbe/simpl.c",
            "deps/qbe/live.c",
            "deps/qbe/spill.c",
            "deps/qbe/rega.c",
            "deps/qbe/emit.c",

            "deps/qbe/amd64/emit.c",
            "deps/qbe/amd64/isel.c",
            "deps/qbe/amd64/sysv.c",
            "deps/qbe/amd64/targ.c",

            "deps/qbe/arm64/abi.c",
            "deps/qbe/arm64/emit.c",
            "deps/qbe/arm64/isel.c",
            "deps/qbe/arm64/targ.c",

            "deps/qbe/rv64/abi.c",
            "deps/qbe/rv64/emit.c",
            "deps/qbe/rv64/isel.c",
            "deps/qbe/rv64/targ.c",
        },
        .flags = &[_][]const u8{
            "-std=c99",
            "-g",
            "-Wall",
            "-Wextra",
            "-Wpedantic",
        },
    });

    b.installArtifact(qbe);

    try std.fs.cwd().deleteFile("deps/qbe/config.h");

    const lib = b.addStaticLibrary(.{
        .name = "yttrium",
        .root_source_file = std.Build.FileSource.relative("src/lib.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "yttrium",
        .root_source_file = std.Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
