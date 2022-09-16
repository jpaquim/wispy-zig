const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("./types/token.zig").Token;

pub fn lex(allocator: Allocator, input: []const u8) !std.ArrayList(Token) {
    const chars = std.mem.trim(u8, input, &std.ascii.spaces);

    var tokens = std.ArrayList(Token).init(allocator);

    var i: usize = 0;
    while (i < chars.len) {
        const word = consumeNextWord(chars[i..], &i) orelse break;

        const token = try identifyToken(word);

        try tokens.append(token);
    }

    return tokens;
}

fn consumeNextWord(chars: []const u8, next_index: *usize) ?[]const u8 {
    var token: ?[]const u8 = null;

    var i: usize = 0;
    while (i < chars.len) {
        const char = chars[i];

        if (std.ascii.isSpace(char)) {
            i += 1;
            if (token != null)
                break
            else
                continue;
        }

        if (isTerminatorToken(char) and token != null) break;

        if (token) |*tok|
            tok.*.len += 1
        else
            token = chars[i .. i + 1];
        i += 1;

        if (isTerminatorToken(char)) break;
    }

    next_index.* += i;

    return token;
}

fn identifyToken(word: []const u8) !Token {
    if (std.fmt.parseInt(i32, word, 10)) |n| return Token{ .int = n } else |_| {}
    if (std.fmt.parseFloat(f64, word)) |f| return Token{ .float = f } else |_| {}
    if (isIdentifier(word)) return Token{ .identifier = word };
    if (isBracket(word)) return Token{ .bracket = word[0] };
    if (isTypedIdentifier(word)) return Token{ .typed_identifier = word };

    std.debug.print("Unknown token: {s}\n", .{word});
    return error.UnknownToken;
}

fn isIdentifier(word: []const u8) bool {
    const first_char = word[0];
    if (!std.ascii.isAlpha(first_char) and first_char != '_') {
        return false;
    }

    for (word[1..]) |char| {
        if (!std.ascii.isAlNum(char) and char != '_' and char != '-') {
            return false;
        }
    }
    return true;
}

fn isTypedIdentifier(word: []const u8) bool {
    const separator = std.mem.indexOfScalar(u8, word, ':') orelse return false;
    return isIdentifier(word[0..separator]) and isIdentifier(word[separator + 1 ..]);
}

fn isBracket(word: []const u8) bool {
    return word.len == 1 and isBracketChar(word[0]);
}

fn isTerminatorToken(char: u8) bool {
    return isBracketChar(char);
}

fn isBracketChar(char: u8) bool {
    return switch (char) {
        '(', ')', '[', ']' => true,
        else => false,
    };
}
