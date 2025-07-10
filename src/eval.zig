const std = @import("std");

const cm = @import("main.zig");
const parse = @import("parse.zig");
const Tokenizer = @import("Tokenizer.zig");

const util = @import("util");

pub const compiled = @import("compiled.zig");

pub inline fn eval(
    comptime expr_src: []const u8,
    ctx: anytype,
    inputs: anytype,
) !Eval(expr_src, @TypeOf(ctx), @TypeOf(inputs)) {
    const compiled_expr = compiled.expr(ctx, expr_src);
    return compiled_expr.eval(inputs);
}

pub inline fn evalWithUnused(
    comptime expr_src: []const u8,
    ctx: anytype,
    inputs: anytype,
) !Eval(expr_src, @TypeOf(ctx), @TypeOf(inputs)) {
    const compiled_expr = compiled.expr(ctx, expr_src);
    return compiled_expr.evalWithUnused(inputs);
}

/// Returns the type used to represent the result of evaluating the `expr` with the given context and inputs.
pub fn Eval(
    comptime expr_src: []const u8,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    return compiled.Expr(Ctx, expr_src).Eval(Inputs);
}

test eval {
    const BasicCtx = struct {
        pub const init: @This() = .{};

        const UnOp = enum { @"-" };
        pub inline fn matchUnOp(comptime str: []const u8) bool {
            return @hasField(UnOp, str);
        }

        const BinOp = enum { @"+", @"-", @"*", @"/" };
        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(BinOp, str);
        }

        const relations = .{
            .@"+" = cm.relation(.left, 0),
            .@"-" = cm.relation(.left, 0),
            .@"*" = cm.relation(.left, 1),
            .@"/" = cm.relation(.left, 1),
        };
        pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) ?cm.Order {
            return @field(relations, lhs).order(@field(relations, rhs));
        }

        pub const EvalNumberLiteral = cm.ctx.DefaultEvalNumberLiteral;
        pub const evalNumberLiteral = cm.ctx.defaultEvalNumberLiteral;

        pub fn EvalIdent(comptime ident: []const u8) type {
            _ = ident;
            return noreturn;
        }
        pub fn evalIdent(ctx: @This(), comptime ident: []const u8) EvalIdent(ident) {
            _ = ctx;
            @compileError("Should not be referenced");
        }

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            return @FieldType(Lhs, field);
        }
        pub fn evalProperty(_: @This(), lhs: anytype, comptime field: []const u8) EvalProperty(@TypeOf(lhs), field) {
            return @field(lhs, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            return switch (@typeInfo(Rhs).@"struct".fields.len) {
                0 => Lhs,
                1 => std.meta.Elem(Lhs),
                else => |n| [n]std.meta.Elem(Lhs),
            };
        }
        pub fn evalIndexAccess(_: @This(), lhs: anytype, rhs: anytype) EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            return switch (@typeInfo(@TypeOf(rhs)).@"struct".fields.len) {
                0 => lhs,
                1 => lhs[rhs[0]],
                else => @shuffle(
                    std.meta.Elem(@TypeOf(lhs)),
                    lhs[0..].*,
                    undefined,
                    @as([rhs.len]comptime_int, rhs),
                ),
            };
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            _ = Args;
            return @typeInfo(util.ImplicitDeref(Callee)).@"fn".return_type.?;
        }
        pub fn evalFuncCall(_: @This(), callee: anytype, args: anytype) EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            return @call(.auto, callee, args);
        }

        pub fn EvalMethodCall(comptime SelfParam: type, comptime method: []const u8, comptime Args: type) type {
            const SelfNs = util.ImplicitDeref(SelfParam);
            _ = Args;
            if (@TypeOf(@field(SelfNs, method)) == void) return noreturn;
            const MethodType = @TypeOf(@field(SelfNs, method));

            if (@typeInfo(MethodType) != .@"fn") return noreturn;
            const params = @typeInfo(MethodType).@"fn".params;

            if (params.len == 0) return noreturn;
            const Expected = util.ImplicitDeref(params[0].type orelse *const SelfParam);

            if (SelfNs != Expected) return noreturn;
            return @typeInfo(MethodType).@"fn".return_type.?;
        }
        pub fn evalMethodCall(ctx: @This(), self_param: anytype, comptime method: []const u8, args: anytype) EvalMethodCall(@TypeOf(self_param), method, @TypeOf(args)) {
            const func = @field(util.ImplicitDeref(@TypeOf(self_param)), method);
            return ctx.evalFuncCall(func, .{self_param} ++ args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            _ = op;
            return T;
        }
        pub fn evalUnOp(_: @This(), comptime op: []const u8, val: anytype) EvalUnOp(op, @TypeOf(val)) {
            return switch (@field(UnOp, op)) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(
                @as(Lhs, undefined),
                @as(Rhs, undefined),
            );
        }
        pub fn evalBinOp(_: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return switch (@field(BinOp, op)) {
                .@"+" => lhs + rhs,
                .@"-" => lhs - rhs,
                .@"*" => lhs * rhs,
                .@"/" => lhs / rhs,
            };
        }
    };
    const basic_ctx: BasicCtx = .init;

    try std.testing.expectEqual(3, eval("x[y]", basic_ctx, .{
        .x = [3]u16{ 0, 3, 7 },
        .y = 1,
    }));
    try std.testing.expectEqual(.{ 1, 2, 3 }, eval("x + y", basic_ctx, .{
        .x = std.simd.iota(u8, 3),
        .y = @as(@Vector(3, u8), @splat(1)),
    }));
    try std.testing.expectEqual(-4, eval("-4", basic_ctx, .{}));
    try std.testing.expectEqual(7, eval("a + 3", basic_ctx, .{ .a = 4 }));
    try std.testing.expectEqual(2, eval("a / 2", basic_ctx, .{ .a = 4 }));
    try std.testing.expectEqual(12, eval("(y + 2) * x", basic_ctx, .{ .y = 2, .x = 3 }));
    try std.testing.expectEqual(8, eval("y + 2 * x", basic_ctx, .{ .y = 2, .x = 3 }));
    try std.testing.expectEqual(3, eval("a.b", basic_ctx, .{ .a = .{ .b = 3 } }));
    try std.testing.expectEqual(6, eval("a / b / c", basic_ctx, .{ .a = 24, .b = 2, .c = 2 }));

    const test_fns = struct {
        // zig fmt: off
        inline fn get15() comptime_int { return 15; }
        inline fn addOne(comptime value: comptime_int) i32 { return value + 1; }
        inline fn sub(a: i32, b: i32) u32 { return a - b; }
        // zig fmt: on
    };
    try std.testing.expectEqual(15, eval("get15()", basic_ctx, .{ .get15 = test_fns.get15 }));
    try std.testing.expectEqual(16, eval("addOne(15)", basic_ctx, .{ .addOne = test_fns.addOne }));
    try std.testing.expectEqual(17, eval("sub(19, 2)", basic_ctx, .{ .sub = test_fns.sub }));

    try std.testing.expectEqual(30, eval("2 * get15()", basic_ctx, .{ .get15 = test_fns.get15 }));
    try std.testing.expectEqual(-45, eval("(3 * -get15())", basic_ctx, .{ .get15 = test_fns.get15 }));

    try std.testing.expectEqual([_]u8{ 'a', 'b', 'c' }, eval("a[0, 2, 4]", basic_ctx, .{ .a = "a b c" }));
    try std.testing.expectEqual(31, eval("a.b()", basic_ctx, .{ .a = struct {
        fn b(_: @This()) u32 {
            return 31;
        }
    }{} }));

    const PowCtx = struct {
        pub const init: @This() = .{};

        pub const matchUnOp = {};

        const BinOp = enum { @"^" };

        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(BinOp, str);
        }

        pub const orderBinOp = {};

        const relations = .{
            .@"^" = .{ .prec = 0, .assoc = .right },
        };

        pub const EvalNumberLiteral = BasicCtx.EvalNumberLiteral;
        pub const evalNumberLiteral = BasicCtx.evalNumberLiteral;

        pub fn EvalIdent(comptime ident: []const u8) type {
            _ = ident;
            return noreturn;
        }
        pub fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
            _ = ctx;
            comptime unreachable;
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(@as(Lhs, 0), @as(Rhs, 0));
        }
        pub inline fn evalBinOp(_: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return std.math.pow(@TypeOf(lhs, rhs), lhs, rhs);
        }
    };
    try std.testing.expectEqual(64, eval("a ^ 3", PowCtx.init, .{ .a = @as(u64, 4) }));
}
