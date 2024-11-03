const std = @import("std");
const http = std.http;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const address = "127.0.0.1";
const port = 8192;

pub fn main() !void {
    defer if (gpa.deinit() == .leak) {
        std.process.exit(1);
    };

    const addr = std.net.Address.parseIp(address, port) catch unreachable;
    var net_server = try addr.listen(.{ .reuse_address = true });
    std.log.info("Serving HTTP on {s} port {d} (http://{s}:{d}/)", .{ address, port, address, port });
    const conn = try net_server.accept();
    defer conn.stream.close();

    var client_header_buffer: [1024]u8 = undefined;
    var http_server = http.Server.init(conn, &client_header_buffer);

    while (http_server.state == .ready) {
        var request = http_server.receiveHead() catch |err| switch (err) {
            error.HttpConnectionClosing => continue,
            else => |e| return e,
        };

        try handleRequest(&request);
    }
}

fn handleRequest(request: *http.Server.Request) !void {
    const body = try (try request.reader()).readAllAlloc(alloc, 8192);
    defer alloc.free(body);

    var send_buffer: [1000]u8 = undefined;

    const file_map = [_]FileServeInfo{
        FileServeInfo{
            .path = "/",
            .file = "zig-out/docs/index.html",
            .content_type = "text/html",
        },
        FileServeInfo{
            .path = ".js",
            .file = "zig-out/docs/main.js",
            .content_type = "application/javascript",
        },
        FileServeInfo{
            .path = ".wasm",
            .file = "zig-out/docs/main.wasm",
            .content_type = "application/wasm",
        },
        FileServeInfo{
            .path = ".tar",
            .file = "zig-out/docs/sources.tar",
            .content_type = "application/x-tar",
        },
    };

    var matched = false;
    for (file_map) |file_info| {
        if (std.mem.endsWith(u8, request.head.target, file_info.path) or std.mem.eql(u8, file_info.path, "/") and std.mem.eql(u8, request.head.target, "/")) {
            matched = true;
            try serveFile(request, file_info, &send_buffer);
            break;
        }
    }

    if (!matched) {
        try request.respond("", .{ .status = .not_found });
    }
}

fn serveFile(request: *http.Server.Request, file_info: FileServeInfo, send_buffer: *[1000]u8) !void {
    var file = try std.fs.cwd().openFile(file_info.file, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    var response = request.respondStreaming(.{
        .send_buffer = send_buffer,
        .content_length = file_size,
        .respond_options = .{
            .extra_headers = &.{
                .{
                    .name = "content-type",
                    .value = file_info.content_type,
                },
                .{
                    .name = "cache-control",
                    .value = "no-store",
                },
            },
        },
    });

    const w = response.writer();
    var buffer: [1024]u8 = undefined;
    while (true) {
        const read_bytes = try file.read(&buffer);
        if (read_bytes == 0) break;
        try w.writeAll(buffer[0..read_bytes]);
    }
    try response.end();
    std.log.info("{s} - '{s} {s} {s}' {d}", .{
        address,
        @tagName(request.head.method),
        request.head.target,
        @tagName(request.head.version),
        @intFromEnum(http.Status.ok),
    });
}

const FileServeInfo = struct {
    path: []const u8,
    file: []const u8,
    content_type: []const u8,
};
