const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const jpeg_module = b.addModule("jpeg", .{
        .root_source_file = b.path("src/jpeg/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const png_module = b.addModule("png", .{
        .root_source_file = b.path("src/png/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const image_module = b.addModule("image", .{
        .root_source_file = b.path("src/image/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const root_module = b.createModule(.{
        .root_source_file = b.path("src/zjpeg.zig"),
        .target = target,
        .optimize = optimize,
    });

    jpeg_module.addImport("image", image_module);
    png_module.addImport("image", image_module);
    root_module.addImport("jpeg", jpeg_module);
    root_module.addImport("image", image_module);
    root_module.addImport("png", png_module);

    const exe = b.addExecutable(.{
        .name = "zjpeg",
        .root_module = root_module,
    });

    // Dependencies
    const sdl_dep = b.lazyDependency("SDL", .{
        .optimize = optimize,
        .target = target,
    });
    const sdl_artifact = sdl_dep.?.artifact("SDL2");
    for (sdl_artifact.root_module.include_dirs.items) |include_dir| {
        try exe.root_module.include_dirs.append(b.allocator, include_dir);
    }
    exe.linkLibrary(sdl_artifact);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const jpeg_tests = b.addTest(.{ .root_module = jpeg_module });
    const png_tests = b.addTest(.{ .root_module = png_module });

    const run_jpeg_tests = b.addRunArtifact(jpeg_tests);
    const run_png_tests = b.addRunArtifact(png_tests);

    const test_step_jpeg = b.step("test-jpeg", "Run unit tests");
    test_step_jpeg.dependOn(&run_jpeg_tests.step);

    const test_step_png = b.step("test-png", "Run unit tests");
    test_step_png.dependOn(&run_png_tests.step);

    const install_docs = b.addInstallDirectory(.{
        .source_dir = exe.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Copy documentation artifacts to prefix path");
    docs_step.dependOn(&install_docs.step);

    const serve_step = b.step("serve", "Serve documentation");
    var a3: [3][]const u8 = .{ "zig", "run", "serveDocs.zig" };
    const serve_run = b.addSystemCommand(&a3);
    serve_step.dependOn(&serve_run.step);
}
