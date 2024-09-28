const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) anyerror!void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // DEPENDENCIES

    const zg = b.dependency("zg", .{});
    const code_point_mod = zg.module("code_point");
    const gencatdata_mod = zg.module("GenCatData");
    const utftools = b.dependency("utftools", .{});
    const utftools_mod = utftools.module("utftools");

    // LIBRARY

    const lib = b.addStaticLibrary(.{
        .name = "tungsten",
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    lib.root_module.addImport("code_point", code_point_mod);
    lib.root_module.addImport("GenCatData", gencatdata_mod);
    lib.root_module.addImport("utftools", utftools_mod);
    b.installArtifact(lib);

    // RUNTIME

    const exe = b.addExecutable(.{
        .name = "tungsten",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    exe.root_module.addImport("code_point", code_point_mod);
    exe.root_module.addImport("GenCatData", gencatdata_mod);
    exe.root_module.addImport("utftools", utftools_mod);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // TESTS

    const tests = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    tests.root_module.addImport("code_point", code_point_mod);
    tests.root_module.addImport("GenCatData", gencatdata_mod);
    tests.root_module.addImport("utftools", utftools_mod);
    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(&b.addRunArtifact(tests).step);
}
