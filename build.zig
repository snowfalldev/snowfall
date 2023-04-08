const std = @import("std");

pub fn build(b: *std.Build) anyerror!void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = null,
    });

    const lib = b.addStaticLibrary(.{
        .name = "yttrium",
        .root_source_file = std.Build.FileSource.relative("src/lib.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.builtin.Version.parse("0.0.1"),
    });

    lib.install();

    const exe = b.addExecutable(.{
        .name = "yttrium",
        .root_source_file = std.Build.FileSource.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.builtin.Version.parse("0.0.1"),
    });

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
