const std = @import("std");
const Allocator = std.mem.Allocator;
const binaryen = @import("binaryen");
const ast = @import("./types/ast.zig");
const AstNode = ast.AstNode;
const BlockNode = AstNode.BlockNode;
const TokenTree = ast.TokenTree;
const TokenTreeNode = ast.TokenTreeNode;
const Token = @import("./types/token.zig").Token;

pub fn compile(allocator: Allocator, block: BlockNode) !*binaryen.Module {
    const mod = binaryen.Module.init();
    defer mod.deinit();

    var function_map = try generateFunctionMap(allocator, block);

    try registerStandardFunctions(mod, &function_map);

    _ = try compileExpression(allocator, .{
        .expression = .{ .block = block },
        .mod = mod,
        .function_map = function_map,
        .parameters = ParameterMap.init(allocator),
    });

    return mod;
}

const Error = error{
    ExpectedFunctionDefinition,
    ExpectedIdentifier,
    ExpectedParameters,
    ExpectedTypedFunctionName,
    FunctionNotFound,
    UnrecognizedExpression,
    UnrecognizedIdentifier,
    UnsupportedType,
    UntypedFunctionParameter,
} || Allocator.Error;

const CompileExpressionOpts = struct {
    expression: AstNode,
    mod: *binaryen.Module,
    parameters: ParameterMap,
    function_map: FunctionMap,
};

fn compileExpression(allocator: Allocator, opts: CompileExpressionOpts) Error!*binaryen.Expression {
    const expression = opts.expression;
    const mod = opts.mod;

    switch (expression) {
        .block => |block| return compileBlock(allocator, .{
            .expression = block,
            .mod = opts.mod,
            .parameters = opts.parameters,
            .function_map = opts.function_map,
        }),
        .int => |int| return mod.makeConst(binaryen.Literal.int32(int)),
        .float => |float| return mod.makeConst(binaryen.Literal.float32(@floatCast(f32, float))),
        .identifier => |identifier| return compileIdentifier(.{
            .expression = identifier,
            .mod = opts.mod,
            .parameters = opts.parameters,
            .function_map = opts.function_map,
        }),
        else => {
            std.debug.print("Unrecognized expression {s}\n", .{@tagName(expression)});
            return error.UnrecognizedExpression;
        },
    }
}

const CompileBlockOpts = struct {
    expression: BlockNode,
    mod: *binaryen.Module,
    parameters: ParameterMap,
    function_map: FunctionMap,
};

fn compileBlock(allocator: Allocator, opts: CompileBlockOpts) Error!*binaryen.Expression {
    const block = opts.expression;
    const mod = opts.mod;

    if (block.expressions[0] == .identifier and block.expressions.len > 1) {
        return compileFunctionCall(allocator, opts);
    }

    var expressions = try std.ArrayList(*binaryen.Expression).initCapacity(allocator, block.expressions.len);
    for (block.expressions) |expression| {
        expressions.appendAssumeCapacity(try compileExpression(allocator, .{
            .expression = expression,
            .mod = opts.mod,
            .parameters = opts.parameters,
            .function_map = opts.function_map,
        }));
    }

    return mod.makeBlock(null, expressions.items, binaryen.Type.auto());
}

fn compileFunctionCall(allocator: Allocator, opts: CompileBlockOpts) !*binaryen.Expression {
    const expression = opts.expression;
    const function_map = opts.function_map;
    const mod = opts.mod;
    const identifier_node = expression.expressions[0];

    if (identifier_node != .identifier) {
        std.debug.print("Expected identifier when compiling function call\n", .{});
        return error.ExpectedIdentifier;
    }
    const identifier = identifier_node.identifier;

    if (std.mem.eql(u8, identifier, "fn")) return compileFunction(allocator, opts);

    if (std.mem.eql(u8, identifier, "if")) return compileIf(allocator, opts);

    const function_info = function_map.get(identifier) orelse {
        std.debug.print("Function {s} not found\n", .{identifier});
        return error.FunctionNotFound;
    };

    const arg_exprs = expression.expressions[1..];
    var args = try std.ArrayList(*binaryen.Expression).initCapacity(allocator, arg_exprs.len);
    for (arg_exprs) |expr| {
        args.appendAssumeCapacity(try compileExpression(allocator, .{
            .expression = expr,
            .mod = opts.mod,
            .parameters = opts.parameters,
            .function_map = opts.function_map,
        }));
    }

    return mod.makeCall(identifier, args.items, function_info.return_type);
}

fn compileFunction(allocator: Allocator, opts: CompileBlockOpts) !*binaryen.Expression {
    const block = opts.expression;
    const mod = opts.mod;

    try assertFn(block);

    const function_identifier = try getFunctionIdentifier(block);
    const identifier = function_identifier.identifier;
    const return_type = function_identifier.return_type;

    const function_parameters = try getFunctionParameters(allocator, block);
    const parameters = function_parameters.parameters;
    const parameter_types = function_parameters.parameter_types;

    const body = try compileBlock(allocator, .{
        .expression = .{
            .expressions = block.expressions[3..],
        },
        .mod = opts.mod,
        .function_map = opts.function_map,
        .parameters = parameters,
    });

    _ = mod.addFunction(identifier, parameter_types, return_type, null, body);

    _ = mod.addFunctionExport(identifier, identifier);

    return mod.makeNop();
}

const CompileIdentifierOpts = struct {
    expression: []const u8,
    mod: *binaryen.Module,
    parameters: ParameterMap,
    function_map: FunctionMap,
};

fn compileIdentifier(opts: CompileIdentifierOpts) !*binaryen.Expression {
    const identifier = opts.expression;
    const parameters = opts.parameters;
    const mod = opts.mod;

    const info = parameters.get(identifier) orelse {
        std.debug.print("Unrecognized identifier {s}\n", .{identifier});
        return error.UnrecognizedIdentifier;
    };

    return mod.makeLocalGet(info.index, info.type);
}

const ParameterMap = std.StringHashMap(struct { index: u32, type: binaryen.Type });

const FunctionParameters = struct {
    parameters: ParameterMap,
    parameter_types: binaryen.Type,
};
fn getFunctionParameters(allocator: Allocator, block: BlockNode) !FunctionParameters {
    const node = block.expressions[2];

    if (node != .block) {
        std.debug.print("Expected function parameters\n", .{});
        return error.ExpectedParameters;
    }

    const exprs = node.block.expressions;
    var parameters = ParameterMap.init(allocator);
    try parameters.ensureTotalCapacity(@intCast(u32, exprs.len));
    var types = try std.ArrayList(binaryen.Type).initCapacity(allocator, exprs.len);
    for (exprs) |n, i| {
        if (n != .typed_identifier) {
            std.debug.print("All parameters must be typed\n", .{});
            return error.UntypedFunctionParameter;
        }
        const type_ = try mapBinaryenType(n.typed_identifier.type_identifier);

        types.appendAssumeCapacity(type_);

        parameters.putAssumeCapacity(n.typed_identifier.identifier, .{ .index = @intCast(u32, i), .type = type_ });
    }

    std.mem.reverse(binaryen.Type, types.items);

    return FunctionParameters{
        .parameters = parameters,
        .parameter_types = binaryen.Type.create(types.items),
    };
}

const FunctionIdentifier = struct {
    identifier: []const u8,
    return_type: binaryen.Type,
};

const GetFunctionIdentifierError = error{ExpectedTypedFunctionName} || MapBinaryenTypeError;
fn getFunctionIdentifier(block: BlockNode) GetFunctionIdentifierError!FunctionIdentifier {
    const node = block.expressions[1];

    if (node != .typed_identifier) {
        std.debug.print("Expected typed function name\n", .{});
        return error.ExpectedTypedFunctionName;
    }

    return FunctionIdentifier{
        .identifier = node.typed_identifier.identifier,
        .return_type = try mapBinaryenType(node.typed_identifier.type_identifier),
    };
}

fn compileIf(allocator: Allocator, opts: CompileBlockOpts) !*binaryen.Expression {
    const expression = opts.expression;
    const mod = opts.mod;

    const condition_node = expression.expressions[1];

    const if_true_node = expression.expressions[2];

    const if_false_node = if (expression.expressions.len > 3) expression.expressions[3] else null;

    const condition = try compileExpression(allocator, .{
        .expression = condition_node,
        .mod = opts.mod,
        .parameters = opts.parameters,
        .function_map = opts.function_map,
    });

    const if_true = try compileExpression(allocator, .{
        .expression = if_true_node,
        .mod = opts.mod,
        .parameters = opts.parameters,
        .function_map = opts.function_map,
    });

    const if_false = if (if_false_node) |n| try compileExpression(allocator, .{
        .expression = n,
        .mod = opts.mod,
        .parameters = opts.parameters,
        .function_map = opts.function_map,
    }) else null;

    return mod.makeIf(condition, if_true, if_false);
}

fn registerStandardFunctions(mod: *binaryen.Module, map: *FunctionMap) !void {
    const int32 = binaryen.Type.int32();
    const float32 = binaryen.Type.float32();
    try registerLogicFunction(.{
        .name = "lt_i32",
        .type_ = int32,
        .operator = binaryen.ltSInt32(),
        .mod = mod,
        .map = map,
    });
    try registerLogicFunction(.{
        .name = "gt_i32",
        .type_ = int32,
        .operator = binaryen.gtSInt32(),
        .mod = mod,
        .map = map,
    });
    try registerLogicFunction(.{
        .name = "eq_i32",
        .type_ = int32,
        .operator = binaryen.eqInt32(),
        .mod = mod,
        .map = map,
    });
    try registerLogicFunction(.{
        .name = "lt_f32",
        .type_ = float32,
        .operator = binaryen.ltFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerLogicFunction(.{
        .name = "gt_f32",
        .type_ = float32,
        .operator = binaryen.gtFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerLogicFunction(.{
        .name = "eq_f32",
        .type_ = float32,
        .operator = binaryen.eqFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "add_i32",
        .type_ = int32,
        .operator = binaryen.addInt32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "sub_i32",
        .type_ = int32,
        .operator = binaryen.subInt32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "mul_i32",
        .type_ = int32,
        .operator = binaryen.mulInt32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "add_f32",
        .type_ = float32,
        .operator = binaryen.addFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "sub_f32",
        .type_ = float32,
        .operator = binaryen.subFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "mul_f32",
        .type_ = float32,
        .operator = binaryen.mulFloat32(),
        .mod = mod,
        .map = map,
    });
    try registerMathFunction(.{
        .name = "div_f32",
        .type_ = float32,
        .operator = binaryen.divFloat32(),
        .mod = mod,
        .map = map,
    });
}

const RegisterFunctionOpts = struct {
    mod: *binaryen.Module,
    name: []const u8,
    type_: binaryen.Type,
    operator: binaryen.Op,
    map: *FunctionMap,
};

fn registerMathFunction(opts: RegisterFunctionOpts) !void {
    try registerBinaryFunction(.{
        .mod = opts.mod,
        .name = opts.name,
        .param_type = opts.type_,
        .return_type = opts.type_,
        .operator = opts.operator,
        .map = opts.map,
    });
}

fn registerLogicFunction(opts: RegisterFunctionOpts) !void {
    try registerBinaryFunction(.{
        .mod = opts.mod,
        .name = opts.name,
        .param_type = opts.type_,
        .return_type = binaryen.Type.int32(),
        .operator = opts.operator,
        .map = opts.map,
    });
}

const RegisterBinaryFunctionOpts = struct {
    mod: *binaryen.Module,
    name: []const u8,
    param_type: binaryen.Type,
    return_type: binaryen.Type,
    operator: binaryen.Op,
    map: *FunctionMap,
};

fn registerBinaryFunction(opts: RegisterBinaryFunctionOpts) !void {
    const mod = opts.mod;
    const name = opts.name;
    const param_type = opts.param_type;
    const return_type = opts.return_type;
    const operator = opts.operator;
    const map = opts.map;
    _ = mod.addFunction(
        name,
        binaryen.Type.create(&.{ param_type, param_type }),
        return_type,
        null,
        mod.makeBlock(
            null,
            &.{mod.makeBinary(operator, mod.makeLocalGet(0, param_type), mod.makeLocalGet(1, param_type))},
            binaryen.Type.auto(),
        ),
    );
    try map.put(name, .{ .return_type = return_type });
}

const FunctionMap = std.StringHashMap(struct { return_type: binaryen.Type });

const GenerateFunctionMapError = GetFunctionIdentifierError || Allocator.Error;
fn generateFunctionMap(allocator: Allocator, block: BlockNode) GenerateFunctionMapError!FunctionMap {
    const first_node = block.expressions[0];

    if (first_node == .identifier and std.mem.eql(u8, first_node.identifier, "fn")) {
        const function_identifier = try getFunctionIdentifier(block);
        const identifier = function_identifier.identifier;
        const return_type = function_identifier.return_type;

        var inner_function_map = try generateFunctionMap(allocator, .{ .expressions = block.expressions[3..] });
        try inner_function_map.put(identifier, .{ .return_type = return_type });
        return inner_function_map;
    }

    var map = FunctionMap.init(allocator);
    for (block.expressions) |expr| {
        if (expr == .block) {
            const inner_function_map = try generateFunctionMap(allocator, expr.block);
            try map.ensureUnusedCapacity(inner_function_map.count());
            var it = inner_function_map.iterator();
            while (it.next()) |kv| {
                map.putAssumeCapacity(kv.key_ptr.*, kv.value_ptr.*);
            }
        }
    }
    return map;
}

const MapBinaryenTypeError = error{UnsupportedType};
fn mapBinaryenType(type_identifier: []const u8) MapBinaryenTypeError!binaryen.Type {
    if (std.mem.eql(u8, type_identifier, "i32")) return binaryen.Type.fromEnum(.i32);
    if (std.mem.eql(u8, type_identifier, "f32")) return binaryen.Type.fromEnum(.f32);
    std.debug.print("Unsupported type {s}\n", .{type_identifier});
    return error.UnsupportedType;
}

fn assertFn(block: BlockNode) !void {
    const node = block.expressions[0];
    if (node == .identifier and std.mem.eql(u8, node.identifier, "fn")) return;
    std.debug.print("Expected function definition expression\n", .{});
    return error.ExpectedFunctionDefinition;
}
