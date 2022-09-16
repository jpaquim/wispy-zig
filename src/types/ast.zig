const std = @import("std");

pub const AstNode = union(enum) {
    pub const BlockNode = struct { expressions: []AstNode };

    int: i32,
    float: f64,
    identifier: []const u8,
    typed_identifier: struct {
        identifier: []const u8,
        type_identifier: []const u8,
    },
    block: BlockNode,

    pub fn jsonStringify(
        self: AstNode,
        opt: std.json.StringifyOptions,
        w: anytype,
    ) !void {
        var jsw = std.json.writeStream(w, 3);
        if (opt.whitespace) |whitespace| jsw.whitespace = whitespace;

        try jsw.beginObject();

        try jsw.objectField("type");
        try jsw.emitString(@tagName(self));

        switch (self) {
            .int => |int| {
                try jsw.objectField("value");
                try jsw.emitNumber(int);
            },
            .float => |float| {
                try jsw.objectField("value");
                try jsw.emitNumber(float);
            },
            .identifier => |identifier| {
                try jsw.objectField("identifier");
                try jsw.emitString(identifier);
            },
            .typed_identifier => |typed_identifier| {
                try jsw.objectField("identifier");
                try jsw.emitString(typed_identifier.identifier);
                try jsw.objectField("type_identifier");
                try jsw.emitString(typed_identifier.type_identifier);
            },
            .block => |block| {
                try jsw.objectField("expressions");
                try std.json.stringify(block.expressions, .{ .whitespace = jsw.whitespace }, w);
                jsw.state_index -= 1;
            }
        }

        try jsw.endObject();
    }
};

pub const NonBracketToken = union(enum) {
    int: i32,
    float: f64,
    identifier: []const u8,
    typed_identifier: []const u8,
};

pub const TokenTreeNode = union(enum) {
    non_bracket_token: NonBracketToken,
    token_tree: TokenTree,
};

pub const TokenTree = []TokenTreeNode;
