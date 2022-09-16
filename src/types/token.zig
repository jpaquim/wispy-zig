pub const Token = union(enum) {
    int: i32,
    float: f64,
    identifier: []const u8,
    typed_identifier: []const u8,
    bracket: u8, // '(' | ')' | '[' | ']';
};
