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

    const tokens_json = try std.json.stringifyAlloc(allocator, tokens.items, .{ .whitespace = .{ .indent = .{ .Space = 2 } }});
    defer allocator.free(tokens_json);

    std.debug.print("{s}\n", .{tokens_json});
}
