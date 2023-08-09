const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");
const Tokenizer = @import("Tokenizer.zig");
const parse = @import("parse.zig");

const operator = @import("operator.zig");
const Char = parse.Char;
const Number = parse.Number;

/// Evaluates `expr` as an expression, wherein the operations are defined
/// by `ctx`, and the values of variables may be set via `inputs`.
///
/// * `expr`:
/// Source code representing an expression generally comparable in structure to
/// mathematical and programming notation in most C-like programming languages.
/// A simple representation of the accepted grammar is:
/// ```
///   Expr = $Ident | $Integer | $Char | $Float | $Group | $FieldAccess | $IndexAccess | $FuncCall | $UnOp | $BinOp
///
///   Ident = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9']*
///   Integer # A zig integer literal
///   Char    # A zig char literal
///   Float   # A zig float literal
///   Group = '(' $Expr ')'
///   FieldAccess = $Expr '.' ($Ident|$OperatorSymbol)+
///   IndexAccess = $Expr '[' $Expr ']'
///   FuncCall = $Expr '(' ($Expr ','?)* ')'
///   UnOp = $Operator $Expr
///   BinOp = $Expr $Operator $Expr
///
///   Operator = $OperatorSymbol+
///   OperatorSymbol = ['!' '#' '$' '%' '&' '*' '+' '-' '/' '<' '=' '>' '?' '@' '~' '^' '|' ':']
/// ```
///
/// * `ctx`:
///     Should be a value of a type with a namespace containing:
///     + `UnOp: type`
///         Enum type containing tags whose names correspond to operators, which
///         must only be comprised of symbols contained in `operator.symbols`
///
///     + `BinOp: type`
///         Same constraints as `UnOp`
///
///     + `relations: operator.RelationMap(BinOp) | @TypeOf(.{...})`
///         Struct value whose fields all correspond to the binary operators defined by `BinOp`,
///         each with a value of type `operator.Relation` describing the binary operator's precedence
///         level and associativity.
///
///     + `EvalProperty: fn (comptime Lhs: type, comptime field: []const u8) type`
///     + `evalProperty: fn (ctx: @This(), lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field)`
///         Returns the value that should result from an expression `lhs.field`
///
///     + `EvalIndexAccess: fn (comptime Lhs: type, comptime Rhs: type) type`
///     + `evalIndexAccess: fn (ctx: @This(), lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs))`
///         Returns the value that should result from an expression `lhs[rhs]`
///
///     + `EvalFuncCall: fn (comptime Callee: type, comptime Args: type) type`
///     + `evalFuncCall: fn (ctx: @This(), callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args))`
///         Returns the value that should result from an expression `callee(args...)`, where each of the elements of the tuple `args`
///         are the arguments that should be passed to `callee`.
///
///     + `EvalUnOp: fn (comptime op: UnOp, comptime T: type) type`
///     + `evalUnOp: fn (ctx: @This(), comptime op: UnOp, value: anytype) !EvalUnOp(op, @TypeOf(value))`
///         Returns the value that should result from an expression `op value`
///
///     + `EvalBinOp: fn (comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type`
///     + `evalBinOp: fn (ctx: @This(), lhs: anytype, comptime op: BinOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs))`
///         Returns the value that should result from an expression `lhs op rhs`
///
/// Important to note that in order to work with character and float literals, the implementations of the
/// functions listed above must account for `comath.Char` and `comath.Number`, respectively.
///
/// * `inputs`:
/// a non-tuple struct literal whose field names correspond to identifiers in the `expr` source code
///
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
    const EvalFuncCall = Ctx.EvalFuncCall;
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
            const Lhs = EvalImpl(fa.accessed, Ctx, Inputs);
            break :blk EvalProperty(Lhs, util.dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            break :blk EvalIndexAccess(Lhs, Rhs);
        },
        .func_call => |fc| blk: {
            const Callee = EvalImpl(fc.callee, Ctx, Inputs);
            const Args = EvalExprTupleImpl(fc.args, Ctx, Inputs);
            break :blk EvalFuncCall(Callee, Args);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val, Ctx, Inputs);
            break :blk EvalUnOp(@field(UnOp, un.op), Val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs, Ctx, Inputs);
            const Rhs = EvalImpl(bin.rhs, Ctx, Inputs);
            break :blk EvalBinOp(Lhs, @field(BinOp, bin.op), Rhs);
        },
    };
}
fn EvalExprTupleImpl(
    comptime list: []const parse.ExprNode,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    var fields: [list.len]std.builtin.Type.StructField = undefined;
    for (&fields, list, 0..) |*field, arg, i| {
        const T = EvalImpl(arg, Ctx, Inputs);
        const comptime_only = util.typeIsComptimeOnly(T) orelse @compileError("Cannot have an argument of type " ++ @typeName(T));

        field.* = .{
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .type = T,
            .default_value = if (!comptime_only) null else blk: {
                const val: T = evalImpl(arg, @as(Ctx, undefined), @as(Inputs, undefined)) catch |err| @compileError(@errorName(err));
                break :blk &val;
            },
            .is_comptime = comptime_only,
            .alignment = 0,
        };
        if (@sizeOf(T) == 0) {
            field.is_comptime = true;
            field.default_value = &(evalImpl(arg, @as(Ctx, undefined), @as(Inputs, undefined)) catch |err| @compileError(@errorName(err)));
        }
    }
    return @Type(.{ .Struct = .{
        .layout = .Auto,
        .decls = &.{},
        .is_tuple = true,
        .fields = &fields,
    } });
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
            const Lhs = EvalImpl(fa.accessed, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(fa.accessed, ctx, inputs);
            break :blk ctx.evalProperty(lhs, util.dedupeSlice(u8, fa.accessor));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(ia.accessed, ctx, inputs);

            const Rhs = EvalImpl(ia.accessor, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(ia.accessor, ctx, inputs);

            break :blk ctx.evalIndexAccess(lhs, rhs);
        },
        .func_call => |fc| blk: {
            const Callee = EvalImpl(fc.callee, Ctx, Inputs);
            const callee: Callee = try evalImpl(fc.callee, ctx, inputs);

            const Args = EvalExprTupleImpl(fc.args, Ctx, Inputs);
            const args: Args = args: {
                if (util.typeIsComptimeOnly(Args).?) {
                    var args: Args = undefined;
                    inline for (fc.args, 0..) |arg, i| {
                        if (@typeInfo(Args).Struct.fields[i].is_comptime) continue;
                        args[i] = try evalImpl(arg, ctx, inputs);
                    }
                    break :args args;
                } else comptime {
                    var args: Args = undefined;
                    inline for (fc.args, 0..) |arg, i| {
                        if (@typeInfo(Args).Struct.fields[i].is_comptime) continue;
                        args[i] = try evalImpl(arg, ctx, inputs);
                    }
                    break :args args;
                }
            };

            break :blk ctx.evalFuncCall(callee, args);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val, Ctx, Inputs);
            const val: Val = try evalImpl(un.val, ctx, inputs);
            break :blk ctx.evalUnOp(@field(UnOp, un.op), val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs, Ctx, Inputs);
            const lhs: Lhs = try evalImpl(bin.lhs, ctx, inputs);

            const Rhs = EvalImpl(bin.rhs, Ctx, Inputs);
            const rhs: Rhs = try evalImpl(bin.rhs, ctx, inputs);

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

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            _ = Args;
            return @typeInfo(util.ImplicitDeref(Callee)).Fn.return_type.?;
        }
        pub fn evalFuncCall(_: @This(), callee: anytype, args: anytype) EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            return @call(.auto, callee, args);
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

    const test_fns = struct {
        // zig fmt: off
        inline fn get15() comptime_int { return 15; }
        inline fn addOne(comptime value: comptime_int) i32 { return value + 1; }
        inline fn sub(a: i32, b: i32) u32 { return a - b; }
        // zig fmt: on
    };
    try util.testing.expectEqual(15, eval("get15()", SimpleCtx{}, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(16, eval("addOne(15)", SimpleCtx{}, .{ .addOne = test_fns.addOne }));
    try util.testing.expectEqual(17, eval("sub(19, 2)", SimpleCtx{}, .{ .sub = test_fns.sub }));

    try util.testing.expectEqual(30, eval("2 * get15()", SimpleCtx{}, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(-45, eval("(3 * -get15())", SimpleCtx{}, .{ .get15 = test_fns.get15 }));

    const PowCtx = struct {
        pub const UnOp = enum {};
        pub const BinOp = enum { @"^" };
        pub const relations: operator.RelationMap(BinOp) = .{
            .@"^" = .{ .prec = 0, .assoc = .right },
        };

        pub const EvalProperty = opaque {};
        pub const EvalIndexAccess = opaque {};
        pub const EvalUnOp = opaque {};
        pub const EvalFuncCall = opaque {};

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
