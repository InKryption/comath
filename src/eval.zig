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
///   IndexAccess = $Expr '[' $ExprList ']'
///   FuncCall    = $Expr '(' $ExprList ')'
///   UnOp = $Operator $Expr
///   BinOp = $Expr $Operator $Expr
///
///   ExprList = ($Expr ','?)*
///   Operator = $OperatorSymbol+
///   OperatorSymbol = ['!' '#' '$' '%' '&' '*' '+' '-' '/' '<' '=' '>' '?' '@' '~' '^' '|' ':']
/// ```
///
/// * `ctx`:
///     Should be a value of a type with a namespace containing:
///     + `allow_unused_inputs: bool = false`
///         Boolean constant which with a value of `true` disables checking for unused inputs,
///         and with a value of `false` causes a compile error to be issued for unused inputs.
///         This declaration can be omitted, and defaults to `false`.
///
///     + `UnOp: type`
///         Enum type containing tags whose names correspond to operators, which
///         must only be comprised of symbols contained in `operator.symbols`
///
///     + `BinOp: type`
///         Same constraints as `UnOp`
///
///     + `relations: operator.RelationMap(BinOp) | @TypeOf(.{...})`
///         Struct constant whose fields all correspond to the binary operators defined by `BinOp`,
///         each with a value of type `operator.Relation` describing the binary operator's precedence
///         level and associativity.
///
///     + `EvalProperty: fn (comptime Lhs: type, comptime field: []const u8) type`
///     + `evalProperty: fn (ctx: @This(), lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field)`
///         Returns the value that should result from an expression `lhs.field`.
///
///     + `EvalIndexAccess: fn (comptime Lhs: type, comptime Rhs: type) type`
///     + `evalIndexAccess: fn (ctx: @This(), lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs))`
///         Returns the value that should result from an expression `lhs[rhs...]`, where each of the elements of the tuple `rhs`
///         are the indices that should be used to access `lhs`.
///
///     + `EvalFuncCall: fn (comptime Callee: type, comptime Args: type) type`
///     + `evalFuncCall: fn (ctx: @This(), callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args))`
///         Returns the value that should result from an expression `callee(args...)`, where each of the elements of the tuple `args`
///         are the arguments that should be passed to `callee`.
///
///     + `EvalUnOp: fn (comptime op: UnOp, comptime T: type) type`
///     + `evalUnOp: fn (ctx: @This(), comptime op: UnOp, value: anytype) !EvalUnOp(op, @TypeOf(value))`
///         Returns the value that should result from an expression `op value`.
///
///     + `EvalBinOp: fn (comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type`
///     + `evalBinOp: fn (ctx: @This(), lhs: anytype, comptime op: BinOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs))`
///         Returns the value that should result from an expression `lhs op rhs`.
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
    const Inputs = @TypeOf(inputs);
    const allow_unused_inputs: bool = @hasDecl(Ns, "allow_unused_inputs") and Ns.allow_unused_inputs;
    comptime if (!allow_unused_inputs and @typeInfo(Inputs).Struct.fields.len != 0) {
        const InputTag = util.dedupe.Enum(std.meta.FieldEnum(Inputs));
        const deduped_expr = util.dedupe.scalarSlice(u8, expr[0..].*);
        analyzeUnusedInputs(deduped_expr, InputTag) catch |err| @compileError(@errorName(err));
    };
    const root = comptime parse.parseExpr(
        expr,
        util.namespaceDecl(Ns, "UnOp") orelse null,
        util.namespaceDecl(Ns, "BinOp") orelse null,
        util.namespaceDecl(Ns, "relations") orelse null,
    );
    return evalImpl(root, ctx, inputs);
}
inline fn analyzeUnusedInputs(
    comptime expr: []const u8,
    comptime InputTag: type,
) !void {
    comptime {
        var unused_set = std.EnumSet(InputTag).initFull();
        var tokenizer = Tokenizer{};
        @setEvalBranchQuota(expr.len * 100);
        while (true) switch (tokenizer.next(expr)) {
            .eof => break,
            .ident => |ident| unused_set.remove(@field(InputTag, ident)),
            else => {},
        };
        var err_str: []const u8 = "";
        var iter = unused_set.iterator();
        @setEvalBranchQuota(unused_set.count() + 1);
        while (iter.next()) |unused| {
            const comma = if (err_str.len == 0) "" else ", ";
            err_str = comma ++ @tagName(unused);
        }

        const err_msg_prefix = if (unused_set.count() > 1)
            "Unused inputs: "
        else
            "Unused input: ";
        if (err_str.len != 0)
            return @field(anyerror, err_msg_prefix ++ err_str);
    }
}

pub fn Eval(
    comptime expr: []const u8,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const Ns = util.ImplicitDeref(Ctx);
    const root = parse.parseExpr(
        expr,
        util.namespaceDecl(Ns, "UnOp") orelse null,
        util.namespaceDecl(Ns, "BinOp") orelse null,
        util.namespaceDecl(Ns, "relations") orelse null,
    );
    return EvalImpl(root, Ctx, Inputs);
}

fn EvalImpl(
    comptime expr: parse.ExprNode,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const Ns = switch (@typeInfo(Ctx)) {
        .Pointer => |pointer| switch (pointer.size) {
            .One => pointer.child,
            else => Ctx,
        },
        else => Ctx,
    };
    return switch (expr) {
        .null => noreturn,
        .err => noreturn,
        .ident => |ident| std.meta.FieldType(Inputs, @field(std.meta.FieldEnum(Inputs), ident)),
        .integer => comptime_int,
        .char => Char,
        .float => Number,
        .group => |group| EvalImpl(group.*, Ctx, Inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed, Ctx, Inputs);
            break :blk Ns.EvalProperty(Lhs, util.dedupe.scalarSlice(u8, fa.accessor[0..].*));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ctx, Inputs);
            const Rhs = EvalExprTupleImpl(ia.accessor, Ctx, Inputs);
            break :blk Ns.EvalIndexAccess(Lhs, Rhs);
        },
        .func_call => |fc| blk: {
            const Callee = EvalImpl(fc.callee, Ctx, Inputs);
            const Args = EvalExprTupleImpl(fc.args, Ctx, Inputs);
            break :blk Ns.EvalFuncCall(Callee, Args);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val, Ctx, Inputs);
            break :blk Ns.EvalUnOp(un.op, Val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs, Ctx, Inputs);
            const Rhs = EvalImpl(bin.rhs, Ctx, Inputs);
            break :blk Ns.EvalBinOp(Lhs, bin.op, Rhs);
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
fn evalExprTupleImpl(
    comptime list: []const parse.ExprNode,
    ctx: anytype,
    inputs: anytype,
) !EvalExprTupleImpl(list, @TypeOf(ctx), @TypeOf(inputs)) {
    const Tuple = EvalExprTupleImpl(list, @TypeOf(ctx), @TypeOf(inputs));
    var args: Tuple = undefined;
    @setEvalBranchQuota(args.len * 2);
    inline for (list, 0..) |arg, i| {
        if (@typeInfo(Tuple).Struct.fields[i].is_comptime) continue;
        args[i] = try evalImpl(arg, ctx, inputs);
    }
    return args;
}

inline fn evalImpl(
    comptime expr: parse.ExprNode,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(expr, @TypeOf(ctx), @TypeOf(inputs)) {
    const Ctx = @TypeOf(ctx);
    const Ns = switch (@typeInfo(Ctx)) {
        .Pointer => |pointer| switch (pointer.size) {
            .One => pointer.child,
            else => Ctx,
        },
        else => Ctx,
    };

    const Inputs = @TypeOf(inputs);
    return switch (comptime expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .err => |err| @compileError(err),

        .ident => |ident| @field(inputs, ident),
        .integer => |int| int,
        .char => |char| char,
        .float => |num| num,
        .group => |group| evalImpl(group.*, ctx, inputs),
        .field_access => |fa| blk: {
            const Lhs = EvalImpl(fa.accessed, Ns, Inputs);
            const lhs: Lhs = try evalImpl(fa.accessed, ctx, inputs);
            break :blk ctx.evalProperty(lhs, util.dedupe.scalarSlice(u8, fa.accessor[0..].*));
        },
        .index_access => |ia| blk: {
            const Lhs = EvalImpl(ia.accessed, Ns, Inputs);
            const lhs: Lhs = try evalImpl(ia.accessed, ctx, inputs);

            const Rhs = EvalExprTupleImpl(ia.accessor, Ns, Inputs);
            const rhs: Rhs = rhs: {
                if (comptime util.typeIsComptimeOnly(Rhs).?) {
                    break :rhs try comptime evalExprTupleImpl(ia.accessor, ctx, inputs);
                } else {
                    break :rhs try evalExprTupleImpl(ia.accessor, ctx, inputs);
                }
            };

            break :blk ctx.evalIndexAccess(lhs, rhs);
        },
        .func_call => |fc| blk: {
            const Callee = EvalImpl(fc.callee, Ns, Inputs);
            const callee: Callee = try evalImpl(fc.callee, ctx, inputs);

            const Args = EvalExprTupleImpl(fc.args, Ns, Inputs);
            const args: Args = args: {
                if (comptime util.typeIsComptimeOnly(Args).?) {
                    break :args try comptime evalExprTupleImpl(fc.args, ctx, inputs);
                } else {
                    break :args try evalExprTupleImpl(fc.args, ctx, inputs);
                }
            };

            break :blk ctx.evalFuncCall(callee, args);
        },
        .un_op => |un| blk: {
            const Val = EvalImpl(un.val, Ns, Inputs);
            const val: Val = try evalImpl(un.val, ctx, inputs);
            break :blk ctx.evalUnOp(un.op, val);
        },
        .bin_op => |bin| blk: {
            const Lhs = EvalImpl(bin.lhs, Ns, Inputs);
            const lhs: Lhs = try evalImpl(bin.lhs, ctx, inputs);

            const Rhs = EvalImpl(bin.rhs, Ns, Inputs);
            const rhs: Rhs = try evalImpl(bin.rhs, ctx, inputs);

            break :blk ctx.evalBinOp(lhs, bin.op, rhs);
        },
    };
}

test eval {
    const BasicCtx = struct {
        pub const UnOp = enum { @"-" };
        pub const BinOp = enum { @"+", @"-", @"*", @"/" };
        pub const relations = .{
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
            return switch (@typeInfo(Rhs).Struct.fields.len) {
                0 => Lhs,
                1 => std.meta.Elem(Lhs),
                else => |n| [n]std.meta.Elem(Lhs),
            };
        }
        pub fn evalIndexAccess(_: @This(), lhs: anytype, rhs: anytype) EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            return switch (@typeInfo(@TypeOf(rhs)).Struct.fields.len) {
                0 => lhs,
                1 => lhs[rhs[0]],
                else => @shuffle(
                    std.meta.Elem(@TypeOf(lhs)),
                    util.implicitDeref(lhs[0..]),
                    undefined,
                    @as([rhs.len]comptime_int, rhs),
                ),
            };
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            _ = Args;
            return @typeInfo(util.ImplicitDeref(Callee)).Fn.return_type.?;
        }
        pub fn evalFuncCall(_: @This(), callee: anytype, args: anytype) EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            return @call(.auto, callee, args);
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
    try util.testing.expectEqual(3, eval("x[y]", BasicCtx{}, .{
        .x = [3]u16{ 0, 3, 7 },
        .y = 1,
    }));
    try util.testing.expectEqual(.{ 1, 2, 3 }, eval("x + y", BasicCtx{}, .{
        .x = std.simd.iota(u8, 3),
        .y = @as(@Vector(3, u8), @splat(1)),
    }));
    try util.testing.expectEqual(-4, eval("-4", BasicCtx{}, .{}));
    try util.testing.expectEqual(7, eval("a + 3", BasicCtx{}, .{ .a = 4 }));
    try util.testing.expectEqual(2, eval("a / 2", BasicCtx{}, .{ .a = 4 }));
    try util.testing.expectEqual(12, eval("(y + 2) * x", BasicCtx{}, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(8, eval("y + 2 * x", BasicCtx{}, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(3, eval("a.b", BasicCtx{}, .{ .a = .{ .b = 3 } }));
    try util.testing.expectEqual(6, eval("a / b / c", BasicCtx{}, .{ .a = 24, .b = 2, .c = 2 }));

    const test_fns = struct {
        // zig fmt: off
        inline fn get15() comptime_int { return 15; }
        inline fn addOne(comptime value: comptime_int) i32 { return value + 1; }
        inline fn sub(a: i32, b: i32) u32 { return a - b; }
        // zig fmt: on
    };
    try util.testing.expectEqual(15, eval("get15()", BasicCtx{}, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(16, eval("addOne(15)", BasicCtx{}, .{ .addOne = test_fns.addOne }));
    try util.testing.expectEqual(17, eval("sub(19, 2)", BasicCtx{}, .{ .sub = test_fns.sub }));

    try util.testing.expectEqual(30, eval("2 * get15()", BasicCtx{}, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(-45, eval("(3 * -get15())", BasicCtx{}, .{ .get15 = test_fns.get15 }));

    try util.testing.expectEqual([_]u8{ 'a', 'b', 'c' }, eval("a[0, 2, 4]", BasicCtx{}, .{ .a = "a b c" }));

    const PowCtx = struct {
        pub const BinOp = enum { @"^" };
        pub const relations: operator.RelationMap(BinOp) = .{
            .@"^" = .{ .prec = 0, .assoc = .right },
        };

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            _ = op;
            return @TypeOf(@as(Lhs, 0), @as(Rhs, 0));
        }
        pub inline fn evalBinOp(_: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return std.math.pow(@TypeOf(lhs, rhs), lhs, rhs);
        }
    };
    try util.testing.expectEqual(64, eval("a ^ 3", PowCtx{}, .{ .a = @as(u64, 4) }));
}
