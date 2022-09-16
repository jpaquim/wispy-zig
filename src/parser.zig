const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("./types/ast.zig");
const AstNode = ast.AstNode;
const BlockNode = AstNode.BlockNode;
const TokenTree = ast.TokenTree;
const TokenTreeNode = ast.TokenTreeNode;
const Token = @import("./types/token.zig").Token;

pub fn parse(allocator: Allocator, tokens: []const Token) !AstNode {
    var blocks = std.ArrayList(AstNode).init(allocator);

    var i: usize = 0;
    while (i < tokens.len) {
        const tree = try consumeTokenTree(allocator, tokens[i..], &i);

        try blocks.append(.{ .block = try parseBlock(allocator, tree) });
    }

    return AstNode{ .block = .{ .expressions = blocks.items } };
}

fn parseBlock(allocator: Allocator, block: TokenTree) !BlockNode {
    var expressions = try std.ArrayList(AstNode).initCapacity(allocator, block.len);
    // for (block) |node, i| {
    // expressions.items[i] = try parseExpression(allocator, node);
    for (block) |node| {
        expressions.appendAssumeCapacity(try parseExpression(allocator, node));
    }
    return BlockNode{ .expressions = expressions.items };
}

fn parseExpression(allocator: Allocator, expression: TokenTreeNode) Allocator.Error!AstNode {
    switch (expression) {
        .token_tree => |token_tree| return AstNode{ .block = try parseBlock(allocator, token_tree) },
        .non_bracket_token => |token| switch (token) {
            .identifier => |identifier| return AstNode{ .identifier = identifier },
            .typed_identifier => |typed_identifier| {
                const separator = std.mem.indexOfScalar(u8, typed_identifier, ':') orelse unreachable;
                return AstNode{ .typed_identifier = .{
                    .identifier = typed_identifier[0..separator],
                    .type_identifier = typed_identifier[separator + 1 ..],
                } };
            },
            .float => |float| return AstNode{ .float = float },
            .int => |int| return AstNode{ .int = int },
            // else => {
            //     std.debug.print("Unrecognized expression {}\n", .{expression});
            //     return error.UnrecognizedExpression;
            // },
        },
    }
}

const Error = error{ ExpectedBracket, ExpectedLeftBracket } || Allocator.Error;

fn consumeTokenTree(allocator: Allocator, tokens: []const Token, next_index: *usize) Error!TokenTree {
    var tree = std.ArrayList(TokenTreeNode).init(allocator);

    var i: usize = 0;

    try consumeLeftBracket(tokens, &i);

    while (i < tokens.len) {
        const token = tokens[i];

        if (token == .bracket) {
            if (getBracketDirection(token) catch unreachable == .left) {
                try tree.append(.{ .token_tree = try consumeTokenTree(allocator, tokens[i..], &i) });
                continue;
            }
            i += 1;
            break;
        }

        try tree.append(.{ .non_bracket_token = switch (token) {
            .int => |int| .{ .int = int },
            .float => |float| .{ .float = float },
            .identifier => |identifier| .{ .identifier = identifier },
            .typed_identifier => |typed_identifier| .{ .typed_identifier = typed_identifier },
            .bracket => unreachable,
        } });

        i += 1;
    }

    next_index.* += i;

    return tree.items;
}

fn consumeLeftBracket(tokens: []const Token, next_index: *usize) !void {
    const bracketDirection = try getBracketDirection(tokens[0]);

    if (bracketDirection != .left) {
        std.debug.print("Expected left bracket\n", .{});
        return error.ExpectedLeftBracket;
    }

    next_index.* += 1;
}

const BracketDirection = enum {
    left,
    right,
};

fn getBracketDirection(token: Token) !BracketDirection {
    if (token != .bracket) {
        std.debug.print("Expected bracket, got {s}\n", .{@tagName(token)});
        return error.ExpectedBracket;
    }
    return if (token.bracket == '(' or token.bracket == '[') .left else .right;
}
