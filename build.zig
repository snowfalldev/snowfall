const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) anyerror!void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // DEPENDENCIES

    //const unicode = b.dependency("unicode", .{});
    //const unicode_mod = unicode.module("unicode");

    // LIBRARY

    const lib = b.addStaticLibrary(.{
        .name = "tungsten",
        .root_source_file = std.Build.LazyPath.relative("src/lib.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    //lib.root_module.addImport("unicode", unicode_mod);
    b.installArtifact(lib);

    // RUNTIME

    const exe = b.addExecutable(.{
        .name = "tungsten",
        .root_source_file = std.Build.LazyPath.relative("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    //exe.root_module.addImport("unicode", unicode_mod);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // TESTS

    const tests = b.addTest(.{
        .name = "test",
        .root_source_file = std.Build.LazyPath.relative("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(&b.addRunArtifact(tests).step);
}
