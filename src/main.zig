const std = @import("std");
const lex = @import("./lexer.zig").lex;

const max_file_size = std.math.maxInt(usize);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var it = try std.process.argsWithAllocator(allocator);
    defer it.deinit();

    const exe = it.next() orelse unreachable;

    const input_file_name = it.next() orelse {
        std.debug.print("usage: {s} input_file\n", .{exe});
        std.process.exit(1);
    };

    var input_file = try std.fs.cwd().openFile(input_file_name, .{ .mode = .read_only });
    defer input_file.close();

    const input = try input_file.reader().readAllAlloc(allocator, max_file_size);
    defer allocator.free(input);

    const tokens = try lex(allocator, input);
    defer tokens.deinit();

    std.debug.print("{}\n", .{tokens});

    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}
