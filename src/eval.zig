const std = @import("std");
const assert = std.debug.assert;

const comath = @import("main.zig");
const util = @import("util");
const Tokenizer = @import("Tokenizer.zig");
const parse = @import("parse.zig");

const operator = @import("operator.zig");
const Number = @import("main.zig").Number;

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
///   Ident = $IdentStartSymbol $IdentSymbol*
///   Integer # String of characters tokenized as a zig integer literal
///   Float   # String of characters tokenized as a zig float literal
///
///   FieldAccess = $Expr '.' ($IdentSymbol|$OperatorSymbol)+
///   Group = '(' $Expr ')'
///   UnOp = $Operator $Expr
///   BinOp = $Expr $Operator $Expr
///   IndexAccess = $Expr '[' $ExprList ']'
///   FuncCall    = $Expr '(' $ExprList ')'
///
///   ExprList = ($Expr ','?)*
///
///   Operator = $OperatorSymbol+
///   OperatorSymbol = ['!' '#' '$' '%' '&' '*' '+' '-' '/' '<' '=' '>' '?' '@' '~' '^' '|' ':']
///
///   IdentStartSymbol = ['A'-'Z' 'a'-'z' '_']
///   IdentSymbol = $IdentStartSymbol | ['0'-'9']
/// ```
///
/// * `ctx`:
///     Should be a value of a type with a namespace containing:
///     + `allow_unused_inputs: bool = false`
///         Boolean constant which with a value of `true` disables checking for unused inputs,
///         and with a value of `false` causes a compile error to be issued for unused inputs.
///         This declaration can be omitted, and defaults to `false`.
///
///     + `matchUnOp: fn (comptime str: []const u8) callconv(.Inline) bool`
///         Function receiving a string of symbols, which should return true for any string of
///         symbols matching a recognized unary operator.
///
///     + `matchBinOp: fn (comptime str: []const u8) callconv(.Inline) bool`
///         Function receiving a string of symbols, which should return true for any string of
///         symbols matching a recognized binary operator.
///
///     + `relations: operator.RelationMap(BinOp) | @TypeOf(.{...})`
///         Struct constant whose fields all correspond to the binary operators recognized by `matchBinOp`,
///         each with a value of type `operator.Relation` describing the binary operator's precedence
///         level and associativity.
///
///     + `EvalNumberLiteral: fn (comptime src: []const u8) type`
///     + `evalNumberLiteral: fn (comptime src: []const u8) EvalNumberLiteral(src)`
///         Returns the value that should be used to represent a number literal.
///
///     + `EvalIdent: fn (comptime ident: []const u8) type`
///     + `evalIdent: fn (ctx: @This(), comptime ident: []const u8) !EvalIdent(ident)`
///         Returns the value of the identifier. `EvalIdent(ident) = noreturn` will
///         make the function get the value from the `inputs` struct.
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
/// Important to note that in order to work with number literals, the implementations of the
/// functions listed above must account for `comath.Number`.
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
    const Ns = switch (@typeInfo(Ctx)) {
        .Pointer => @TypeOf(ctx.*),
        else => Ctx,
    };
    const allow_unused_inputs: bool =
        @hasDecl(Ns, "allow_unused_inputs") and
        Ns.allow_unused_inputs;
    const Inputs = @TypeOf(inputs);

    const deduped_expr = util.dedupe.scalarSlice(u8, expr[0..].*);
    comptime if (!allow_unused_inputs and @typeInfo(Inputs).Struct.fields.len != 0) {
        const InputTag = util.dedupe.Enum(std.meta.FieldEnum(Inputs));
        const EvalIdent = if (@hasDecl(Ns, "EvalIdent")) Ns.EvalIdent else struct {
            fn DummyEvalIdent(comptime _: []const u8) type {
                return noreturn;
            }
        }.DummyEvalIdent;
        analyzeInputs(deduped_expr, InputTag, EvalIdent) catch |err|
            @compileError(@errorName(err));
    };
    const root = comptime parse.parseExpr(
        deduped_expr,
        if (@hasDecl(Ns, "matchUnOp")) Ns.matchUnOp else null,
        if (@hasDecl(Ns, "matchBinOp")) Ns.matchBinOp else null,
        if (@hasDecl(Ns, "relations")) Ns.relations else null,
    );
    return evalImpl(root, ctx, inputs);
}

pub fn Eval(
    comptime expr: []const u8,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const Ns = switch (@typeInfo(Ctx)) {
        .Struct, .Union, .Enum => Ctx,
        .Pointer => |pointer| if (pointer.size != .One)
            struct {}
        else switch (@typeInfo(pointer.child)) {
            .Struct, .Union, .Enum, .Opaque => pointer.child,
            else => struct {},
        },
        else => struct {},
    };
    const root = parse.parseExpr(
        expr,
        if (@hasDecl(Ns, "matchUnOp")) Ns.matchUnOp else null,
        if (@hasDecl(Ns, "matchBinOp")) Ns.matchBinOp else null,
        if (@hasDecl(Ns, "relations")) Ns.relations else null,
    );
    return EvalImpl(root, Ctx, Inputs);
}

inline fn analyzeInputs(
    comptime expr: []const u8,
    comptime InputTag: type,
    comptime EvalIdent: fn (comptime []const u8) type,
) !void {
    comptime {
        var unused_set = std.EnumSet(InputTag).initFull();
        var shadow_set = std.EnumSet(InputTag).initEmpty();
        var tokenizer = Tokenizer{};
        @setEvalBranchQuota(expr.len * 100);
        while (true) switch (tokenizer.next(expr)) {
            .eof => break,
            .ident => |ident| {
                const global_ident = EvalIdent(ident) != noreturn;
                const input_ident = @hasField(InputTag, ident);

                if (input_ident) {
                    const tag = @field(InputTag, ident);
                    if (global_ident) shadow_set.insert(tag);
                    unused_set.remove(tag);
                }
            },
            else => {},
        };

        var err_str: []const u8 = "";

        for (.{ unused_set, shadow_set }, .{
            if (unused_set.count() > 1) "Unused inputs: " else "Unused input: ",
            if (shadow_set.count() > 1) "Inputs shadowing context identifiers: " else "Input shadowing context identifier: ",
        }) |set, err_msg_prefix| {
            if (set.count() == 0) continue;
            err_str = err_str ++ err_msg_prefix;

            var iter = set.iterator();
            @setEvalBranchQuota(set.count() + 1);
            var need_comma = false;
            while (iter.next()) |unused| {
                const comma = if (need_comma) ", " else "";
                need_comma = true;
                err_str = err_str ++ comma ++ @tagName(unused);
            }

            err_str = err_str ++ "\n";
        }

        if (err_str.len != 0)
            return @field(anyerror, err_str);
    }
}

fn EvalImpl(
    comptime expr: parse.ExprNode,
    comptime Ctx: type,
    comptime Inputs: type,
) type {
    const Ns = switch (@typeInfo(Ctx)) {
        .Struct, .Union, .Enum => Ctx,
        .Pointer => |pointer| Ns: {
            if (pointer.size != .One) break :Ns struct {};
            if (pointer.child == anyopaque) break :Ns struct {};
            break :Ns switch (@typeInfo(pointer.child)) {
                .Struct, .Union, .Enum, .Opaque => pointer.child,
                else => struct {},
            };
        },
        else => struct {},
    };
    return switch (expr) {
        .null => noreturn,
        .err => noreturn,
        .ident => |ident| blk: {
            const IdentType = Ns.EvalIdent(ident);
            if (IdentType != noreturn) break :blk IdentType;
            const field_tag = @field(std.meta.FieldEnum(Inputs), ident);
            break :blk std.meta.FieldType(Inputs, field_tag);
        },
        .number => |number| Ns.EvalNumberLiteral(number),
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

inline fn evalImpl(
    comptime expr: parse.ExprNode,
    ctx: anytype,
    inputs: anytype,
) !EvalImpl(expr, @TypeOf(ctx), @TypeOf(inputs)) {
    const Ctx = @TypeOf(ctx);
    const Ns = switch (@typeInfo(Ctx)) {
        .Struct, .Union, .Enum => Ctx,
        .Pointer => |pointer| Ns: {
            if (pointer.size != .One) break :Ns struct {};
            if (pointer.child == anyopaque) break :Ns struct {};
            break :Ns switch (@typeInfo(pointer.child)) {
                .Struct, .Union, .Enum, .Opaque => pointer.child,
                else => struct {},
            };
        },
        else => struct {},
    };

    const Inputs = @TypeOf(inputs);
    return switch (comptime expr) {
        .null => @compileError("Incomplete AST (encountered null expression)"),
        .err => |err| @compileError(err),

        .ident => |ident| blk: {
            const IdentType = Ns.EvalIdent(ident);
            if (IdentType != noreturn) break :blk ctx.evalIdent(ident);
            break :blk @field(inputs, ident);
        },
        .number => |number| Ns.evalNumberLiteral(number),
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

test eval {
    const BasicCtx = struct {
        const UnOp = enum { @"-" };
        pub inline fn matchUnOp(comptime str: []const u8) bool {
            return @hasField(UnOp, str);
        }

        const BinOp = enum { @"+", @"-", @"*", @"/" };
        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(BinOp, str);
        }

        pub const relations = .{
            .@"+" = operator.relation(.left, 0),
            .@"-" = operator.relation(.left, 0),
            .@"*" = operator.relation(.left, 1),
            .@"/" = operator.relation(.left, 1),
        };

        pub const EvalNumberLiteral = comath.contexts.DefaultEvalNumberLiteral;
        pub const evalNumberLiteral = comath.contexts.defaultEvalNumberLiteral;

        pub fn EvalIdent(comptime ident: []const u8) type {
            _ = ident;
            return noreturn;
        }
        pub fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
            _ = ctx;
            @compileError("Should not be referenced");
        }

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
    const basic_ctx = BasicCtx{};

    try util.testing.expectEqual(3, eval("x[y]", basic_ctx, .{
        .x = [3]u16{ 0, 3, 7 },
        .y = 1,
    }));
    try util.testing.expectEqual(.{ 1, 2, 3 }, eval("x + y", basic_ctx, .{
        .x = std.simd.iota(u8, 3),
        .y = @as(@Vector(3, u8), @splat(1)),
    }));
    try util.testing.expectEqual(-4, eval("-4", basic_ctx, .{}));
    try util.testing.expectEqual(7, eval("a + 3", basic_ctx, .{ .a = 4 }));
    try util.testing.expectEqual(2, eval("a / 2", basic_ctx, .{ .a = 4 }));
    try util.testing.expectEqual(12, eval("(y + 2) * x", basic_ctx, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(8, eval("y + 2 * x", basic_ctx, .{ .y = 2, .x = 3 }));
    try util.testing.expectEqual(3, eval("a.b", basic_ctx, .{ .a = .{ .b = 3 } }));
    try util.testing.expectEqual(6, eval("a / b / c", basic_ctx, .{ .a = 24, .b = 2, .c = 2 }));

    const test_fns = struct {
        // zig fmt: off
        inline fn get15() comptime_int { return 15; }
        inline fn addOne(comptime value: comptime_int) i32 { return value + 1; }
        inline fn sub(a: i32, b: i32) u32 { return a - b; }
        // zig fmt: on
    };
    try util.testing.expectEqual(15, eval("get15()", basic_ctx, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(16, eval("addOne(15)", basic_ctx, .{ .addOne = test_fns.addOne }));
    try util.testing.expectEqual(17, eval("sub(19, 2)", basic_ctx, .{ .sub = test_fns.sub }));

    try util.testing.expectEqual(30, eval("2 * get15()", basic_ctx, .{ .get15 = test_fns.get15 }));
    try util.testing.expectEqual(-45, eval("(3 * -get15())", basic_ctx, .{ .get15 = test_fns.get15 }));

    try util.testing.expectEqual([_]u8{ 'a', 'b', 'c' }, eval("a[0, 2, 4]", basic_ctx, .{ .a = "a b c" }));

    const PowCtx = struct {
        const BinOp = enum { @"^" };
        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(BinOp, str);
        }
        pub const relations = .{
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
    try util.testing.expectEqual(64, eval("a ^ 3", PowCtx{}, .{ .a = @as(u64, 4) }));
}
