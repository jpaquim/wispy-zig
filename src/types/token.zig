const std = @import("std");

pub const Token = union(enum) {
    int: i32,
    float: f64,
    identifier: []const u8,
    typed_identifier: []const u8,
    bracket: u8, // '(' | ')' | '[' | ']';

    pub fn jsonStringify(
        self: Token,
        opt: std.json.StringifyOptions,
        w: anytype,
    ) !void {
        var jsw = std.json.writeStream(w, 3);
        if (opt.whitespace) |whitespace| jsw.whitespace = whitespace;

        try jsw.beginObject();

        try jsw.objectField("type");
        try jsw.emitString(@tagName(self));

        try jsw.objectField("value");
        switch (self) {
            .int => |int| try jsw.emitNumber(int),
            .float => |float| try jsw.emitNumber(float),
            .identifier, .typed_identifier => |identifier| try jsw.emitString(identifier),
            .bracket => |bracket| try jsw.emitString(&.{bracket}),
        }

        try jsw.endObject();
    }
};
