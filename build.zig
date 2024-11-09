const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "emu",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    _ = b.addModule("emu", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "example",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // CHIP-8 test ROMs
    lib_unit_tests.root_module.addAnonymousImport("ibm.ch8", .{
        .root_source_file = b.path("rom/ibm.ch8"),
    });

    const chip8_test_suite = b.dependency("chip8_test_suite", .{});
    const chip8_tests_dir = chip8_test_suite.path("bin").getPath(b);
    var chip8_tests = try std.fs.openDirAbsolute(chip8_tests_dir, .{ .iterate = true });
    defer chip8_tests.close();
    var iter = chip8_tests.iterate();

    while (try iter.next()) |file| {
        if (std.mem.endsWith(u8, file.name, ".ch8")) {
            const name = try std.fmt.allocPrint(b.allocator, "bin/{s}", .{file.name});
            lib_unit_tests.root_module.addAnonymousImport(file.name, .{
                .root_source_file = chip8_test_suite.path(name),
            });
        }
    }

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
