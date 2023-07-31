const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");
const Tokenizer = @import("Tokenizer.zig");
const parse = @import("parse.zig");

pub const Char = parse.Char;
pub const Number = parse.Number;
pub const operator = @import("operator.zig");

comptime {
    _ = Tokenizer;
    _ = parse;
}

/// Evaluates `expr` as an expression, wherein the operations are defined
/// by `ctx`, and the values of variables may be set via `inputs`.
pub inline fn eval(
    comptime expr: []const u8,
    ctx: anytype,
    inputs: anytype,
) !Eval(expr, @TypeOf(ctx), @TypeOf(inputs)) {
    const Ctx = @TypeOf(ctx);
    const Ns = util.ImplicitDeref(Ctx);
    const root = comptime parse.parseExpr(expr, Ns.UnOp, Ns.BinOp, Ns.relations);
    return evalImpl(root, ctx, inputs);
}

pub fn Eval(
    comptime expr: []const u8,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const Ns = util.ImplicitDeref(Ctx);
    const root = parse.parseExpr(expr, Ns.UnOp, Ns.BinOp, Ns.relations);
    return EvalImpl(root, Ctx, Inputs);
}

fn EvalImpl(
    comptime expr: parse.ExprNode,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const UnOp = Ctx.UnOp;
    const BinOp = Ctx.BinOp;

    const EvalProperty = Ctx.EvalProperty;
    const EvalIndexAccess = Ctx.EvalIndexAccess;
    const EvalUnOp = Ctx.EvalUnOp;
    const EvalBinOp = Ctx.EvalBinOp;

    return switch (expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .ident => |ident| std.meta.FieldType(Inputs, @field(std.meta.FieldEnum(Inputs), ident)),
        .integer => comptime_int,
        .char => Char,
        .float => Number,
        .group => |group| EvalImpl(group.*, Ctx, Inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed.*, Ctx, Inputs);
            break :blk EvalProperty(Lhs, util.dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            break :blk EvalIndexAccess(Lhs, Rhs);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val.*, Ctx, Inputs);
            break :blk EvalUnOp(@field(UnOp, un.op), Val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            break :blk EvalBinOp(Lhs, @field(BinOp, bin.op), Rhs);
        },
    };
}

inline fn evalImpl(
    comptime expr: parse.ExprNode,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(expr, @TypeOf(ctx), @TypeOf(inputs)) {
    const Ctx = @TypeOf(ctx);
    const UnOp = Ctx.UnOp;
    const BinOp = Ctx.BinOp;

    const Inputs = @TypeOf(inputs);
    return switch (comptime expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .ident => |ident| @field(inputs, ident),
        .integer => |int| int,
        .char => |char| char,
        .float => |num| num,
        .group => |group| evalImpl(group.*, ctx, inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(fa.accessed.*, ctx, inputs);
            break :blk ctx.evalProperty(lhs, util.dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(ia.accessed, ctx, inputs);

            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(ia.accessor, ctx, inputs);

            break :blk ctx.evalIndexAccess(lhs, rhs);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val.*, Ctx, Inputs);
            const val: Val = try evalImpl(un.val.*, ctx, inputs);
            break :blk ctx.evalUnOp(@field(UnOp, un.op), val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs.*, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(bin.lhs.*, ctx, inputs);

            const Rhs = EvalImpl(bin.rhs.*, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(bin.rhs.*, ctx, inputs);

            break :blk ctx.evalBinOp(lhs, @field(BinOp, bin.op), rhs);
        },
    };
}

test eval {
    const SimpleCtx = struct {
        pub const UnOp = enum { @"-" };
        pub const BinOp = enum { @"+", @"-", @"*", @"/" };
        pub const relations: operator.RelationMap(BinOp) = .{
            .@"+" = operator.relation(.left, 0),
            .@"-" = operator.relation(.left, 1),
            .@"*" = operator.relation(.left, 1),
            .@"/" = operator.relation(.left, 1),
        };

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            return std.meta.FieldType(Lhs, @field(std.meta.FieldEnum(Lhs), field));
        }
        pub fn evalProperty(_: @This(), lhs: anytype, comptime field: []const u8) EvalProperty(@TypeOf(lhs), field) {
            return @field(lhs, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            _ = Rhs;
            return std.meta.Elem(Lhs);
        }
        pub fn evalIndexAccess(_: @This(), lhs: anytype, rhs: anytype) EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            return lhs[rhs];
        }

        pub fn EvalUnOp(comptime op: UnOp, comptime T: type) type {
            _ = op;
            return T;
        }
        pub fn evalUnOp(_: @This(), comptime op: UnOp, val: anytype) EvalUnOp(op, @TypeOf(val)) {
            return switch (op) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(
                @as(Lhs, undefined),
                @as(Rhs, undefined),
            );
        }
        pub fn evalBinOp(_: @This(), lhs: anytype, comptime op: BinOp, rhs: anytype) EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return switch (op) {
                .@"+" => lhs + rhs,
                .@"-" => lhs - rhs,
                .@"*" => lhs * rhs,
                .@"/" => lhs / rhs,
            };
        }
    };
    try util.testing.expectEqual(3, eval("x[y]", SimpleCtx{}, .{
        .x = [3]u16{ 0, 3, 7 },
        .y = 1,
    }));
    try util.testing.expectEqual(.{ 1, 2, 3 }, eval("x + y", SimpleCtx{}, .{
        .x = std.simd.iota(u8, 3),
        .y = @as(@Vector(3, u8), @splat(1)),
    }));
    try util.testing.expectEqual(-4, eval("-4", SimpleCtx{}, .{}));
    try util.testing.expectEqual(7, eval("a + 3", SimpleCtx{}, .{ .a = 4 }));
    try util.testing.expectEqual(2, eval("a / 2", SimpleCtx{}, .{ .a = 4 }));
    try util.testing.expectEqual(12, eval("(y + 2) * x", SimpleCtx{}, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(8, eval("y + 2 * x", SimpleCtx{}, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(3, eval("a.b", SimpleCtx{}, .{ .a = .{ .b = 3 } }));

    const PowCtx = struct {
        pub const UnOp = enum {};
        pub const BinOp = enum { @"^" };
        pub const relations: operator.RelationMap(BinOp) = .{
            .@"^" = .{ .prec = 0, .assoc = .right },
        };

        pub const EvalProperty = opaque {};
        pub const EvalIndexAccess = opaque {};
        pub const EvalUnOp = opaque {};

        pub fn EvalBinOp(comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(@as(Lhs, 0), @as(Rhs, 0));
        }
        pub inline fn evalBinOp(_: @This(), lhs: anytype, comptime op: BinOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return std.math.pow(@TypeOf(lhs, rhs), lhs, rhs);
        }
    };
    try util.testing.expectEqual(64, eval("a ^ 3", PowCtx{}, .{ .a = @as(u64, 4) }));
}
