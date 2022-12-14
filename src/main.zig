const std = @import("std");
const compile = @import("./compiler.zig").compile;
const lex = @import("./lexer.zig").lex;
const parse = @import("./parser.zig").parse;

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

    // const tokens_json = try std.json.stringifyAlloc(allocator, tokens.items, .{ .whitespace = .{ .indent = .{ .Space = 2 } } });
    // defer allocator.free(tokens_json);

    // std.debug.print("{s}\n", .{tokens_json});

    const ast = try parse(allocator, tokens.items);

    // const ast_json = try std.json.stringifyAlloc(allocator, ast, .{ .whitespace = .{ .indent = .{ .Space = 2 } } });
    // defer allocator.free(ast_json);

    // std.debug.print("{s}\n", .{ast_json});

    const mod = compile(allocator, ast.block) catch {
        std.debug.print("Compiler error, exiting\n", .{});
        std.process.exit(1);
    };

    mod.print();

    const binary = try mod.writeAlloc(allocator, null);

    std.debug.print("{any}\n", .{binary});

    // const compiled = new WebAssembly.Module(binary);

    // const instance = new WebAssembly.Instance(compiled, {});

    // console.log((instance.exports.main as () => number)());

}
