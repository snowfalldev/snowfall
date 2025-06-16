const builtin = @import("builtin");
const std = @import("std");

pub fn build(b: *std.Build) anyerror!void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // DEPENDENCIES

    const runerip = b.dependency("runerip", .{});
    const runerip_mod = runerip.module("runerip");

    const zig_gc = b.dependency("zig_gc", .{});
    const zig_gc_mod = zig_gc.module("gc");

    const utils = b.dependency("utils", .{});
    const utils_mod = utils.module("utils");

    // MODULE

    const mod = b.addModule("snowfall", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "runerip", .module = runerip_mod },
            .{ .name = "gc", .module = zig_gc_mod },
            .{ .name = "utils", .module = utils_mod },
        },
    });

    // RUNTIME

    const exe = b.addExecutable(.{
        .name = "snowfall",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .version = try std.SemanticVersion.parse("0.0.1"),
    });

    exe.root_module.addImport("snowfall", mod);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // TESTS

    //const tests = b.addTest(.{
    //    .name = "test",
    //    .root_source_file = b.path("src/tests.zig"),
    //    .target = target,
    //    .optimize = optimize,
    //});

    //tests.root_module.addImport("snowfall", mod);

    //const tests_step = b.step("test", "Run all tests");
    //tests_step.dependOn(&b.addRunArtifact(tests).step);
}
