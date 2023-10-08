const comath = @import("../main.zig");
const std = @import("std");
const util = @import("util");

const UnOp = enum { @"-" };
const BinOp = std.meta.FieldEnum(@TypeOf(relations));
const relations = .{
    .@"+" = comath.relation(.left, 10),
    .@"+|" = comath.relation(.left, 10),
    .@"+%" = comath.relation(.left, 10),

    .@"-" = comath.relation(.left, 10),
    .@"-|" = comath.relation(.left, 10),
    .@"-%" = comath.relation(.left, 10),

    .@"*" = comath.relation(.left, 20),
    .@"*|" = comath.relation(.left, 20),
    .@"*%" = comath.relation(.left, 20),

    .@"/" = comath.relation(.left, 20),
    .@"%" = comath.relation(.left, 20),

    .@"^" = comath.relation(.right, 30),
};

pub inline fn simpleCtx(sub_ctx: anytype) SimpleCtx(@TypeOf(sub_ctx)) {
    const SubCtx = @TypeOf(sub_ctx);
    if (util.NamespaceOf(SubCtx) == null and SubCtx != void) @compileError(
        "Expected struct/union/enum, or pointer to struct/union/enum/opaque, got '" ++ @typeName(SubCtx) ++ "'" ++
            if (SubCtx != type) "" else " (" ++ @typeName(sub_ctx) ++ ")",
    );
    return .{ .sub_ctx = sub_ctx };
}
pub fn SimpleCtx(comptime SubCtx: type) type {
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();
        const Ns = util.NamespaceOf(SubCtx) orelse struct {};

        pub const allow_unused_inputs = @hasDecl(Ns, "allow_unused_inputs") and Ns.allow_unused_inputs;

        pub inline fn matchUnOp(comptime str: []const u8) bool {
            const sub_match = @hasDecl(Ns, "matchUnOp") and Ns.matchUnOp(str);
            return sub_match or @hasField(UnOp, str);
        }

        pub inline fn matchBinOp(comptime str: []const u8) bool {
            const sub_match = @hasDecl(Ns, "matchBinOp") and Ns.matchBinOp(str);
            return sub_match or @hasField(BinOp, str);
        }

        pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) ?comath.Order {
            if (@hasDecl(Ns, "orderBinOp")) blk: {
                const order = Ns.orderBinOp(lhs, rhs) orelse break :blk;
                return order;
            }
            if (!@hasField(@TypeOf(relations), lhs)) return null;
            if (!@hasField(@TypeOf(relations), rhs)) return null;
            return @field(relations, lhs).order(@field(relations, rhs));
        }

        pub const EvalNumberLiteral = comath.ctx.DefaultEvalNumberLiteral;
        pub const evalNumberLiteral = comath.ctx.defaultEvalNumberLiteral;

        pub fn EvalIdent(comptime ident: []const u8) type {
            if (@hasDecl(Ns, "EvalIdent")) {
                if (Ns.EvalIdent(ident) != noreturn) {
                    return Ns.EvalIdent(ident);
                }
            }
            return noreturn;
        }
        pub inline fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
            if (@hasDecl(Ns, "EvalIdent")) {
                if (Ns.EvalIdent(ident) != noreturn) {
                    return ctx.sub_ctx.evalIdent(ident);
                }
            }
            comptime unreachable;
        }

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return Ns.EvalProperty(Lhs, field);
                }
            }
            return std.meta.FieldType(Lhs, field);
        }
        pub inline fn evalProperty(ctx: Self, lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {
            const Lhs = @TypeOf(lhs);
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return ctx.sub_ctx.evalProperty(lhs, field);
                }
            }
            return @field(lhs, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            if (@hasDecl(Ns, "EvalIndexAccess")) {
                if (Ns.EvalIndexAccess(Lhs, Rhs) != noreturn) {
                    return Ns.EvalIndexAccess(Lhs, Rhs);
                }
            }
            return std.meta.Elem(Lhs);
        }
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (!@typeInfo(Rhs).Struct.is_tuple) comptime unreachable;
            if (@hasDecl(Ns, "EvalIndexAccess")) {
                if (Ns.EvalIndexAccess(Lhs, Rhs) != noreturn) {
                    return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
                }
            }
            if (rhs.len != 1) @compileError("Expected single `usize` to index value of type" ++ @typeName(Lhs));
            return lhs[rhs[0]];
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return Ns.EvalFuncCall(Callee, Args);
                }
            }
            const Ret = @typeInfo(util.ImplicitDeref(Callee)).Fn.return_type orelse blk: {
                const callee: Callee = undefined;
                const args: Args = undefined;
                break :blk @TypeOf(@call(.auto, callee, args));
            };
            return util.GetPayloadIfErrorUnion(Ret);
        }
        pub inline fn evalFuncCall(ctx: Self, callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);

            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return ctx.sub_ctx.evalFuncCall(callee, args);
                }
            }
            return @call(.auto, callee, args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, T) != noreturn) {
                    return Ns.EvalUnOp(op, T);
                }
            }

            return switch (@field(UnOp, op)) {
                .@"-" => T,
            };
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, @TypeOf(val)) != noreturn) {
                    return ctx.sub_ctx.evalUnOp(op, val);
                }
            }
            return switch (@field(UnOp, op)) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return Ns.EvalBinOp(Lhs, op, Rhs);
                }
            }

            const lhs: Lhs = switch (@typeInfo(Lhs)) {
                .Int, .ComptimeInt, .Float, .ComptimeFloat => 0,
                else => return noreturn,
            };
            const rhs: Rhs = switch (@typeInfo(Rhs)) {
                .Int, .ComptimeInt, .Float, .ComptimeFloat => 0,
                else => return noreturn,
            };
            return switch (@field(BinOp, op)) {
                .@"+" => @TypeOf(lhs + rhs),
                .@"+%" => @TypeOf(lhs +% rhs),
                .@"+|" => @TypeOf(lhs +| rhs),

                .@"-" => @TypeOf(lhs - rhs),
                .@"-%" => @TypeOf(lhs -% rhs),
                .@"-|" => @TypeOf(lhs -| rhs),

                .@"*" => @TypeOf(lhs * rhs),
                .@"*%" => @TypeOf(lhs *% rhs),
                .@"*|" => @TypeOf(lhs *| rhs),

                .@"/" => @TypeOf(lhs / rhs),
                .@"%" => @TypeOf(lhs % rhs),

                .@"^" => @TypeOf(lhs ^ rhs),
            };
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
                }
            }
            return switch (@field(BinOp, op)) {
                .@"+" => lhs + rhs,
                .@"+%" => lhs +% rhs,
                .@"+|" => lhs +% rhs,

                .@"-" => lhs - rhs,
                .@"-%" => lhs -% rhs,
                .@"-|" => lhs -% rhs,

                .@"*" => lhs * rhs,
                .@"*%" => lhs *% rhs,
                .@"*|" => lhs *% rhs,

                .@"/" => lhs / rhs,
                .@"%" => lhs % rhs,

                .@"^" => blk: {
                    const T = @TypeOf(lhs, rhs);
                    comptime if (T == comptime_int or
                        T == comptime_float)
                    {
                        @setEvalBranchQuota(@min(std.math.maxInt(u32), rhs * 10));
                        var x: T = 1;
                        for (0..rhs) |_| x *= lhs;
                        break :blk x;
                    };

                    break :blk std.math.pow(T, lhs, rhs);
                },
            };
        }
    };
}

test simpleCtx {
    try util.testing.expectEqual(5, comath.eval("a + b", simpleCtx({}), .{ .a = 2, .b = 3 }));
    try util.testing.expectEqual(1, comath.eval("a +% b", simpleCtx({}), .{ .a = @as(u8, std.math.maxInt(u8)), .b = 2 }));
    try util.testing.expectEqual(59049, comath.eval("3^(2 * a + -b)", simpleCtx({}), .{ .a = 7, .b = 4 }));

    // complex precedence interactions
    try util.testing.expectEqual(6 - 3 + 4 + 2, comath.eval("6*1-3*1+4*1+2", simpleCtx({}), .{}));

    const op_override_ctx = simpleCtx(struct {
        const OverrideUnOp = enum { @"++" };
        pub inline fn matchUnOp(comptime str: []const u8) bool {
            return @hasField(OverrideUnOp, str);
        }

        const OverrideBinOp = enum { @"^", @"$" };
        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(OverrideBinOp, str);
        }

        pub const relations = .{
            .@"$" = .{ .prec = 2, .assoc = .right },
        };

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            return switch (@field(OverrideUnOp, op)) {
                .@"++" => T,
            };
        }
        pub fn evalUnOp(_: @This(), comptime op: []const u8, val: anytype) EvalUnOp(op, @TypeOf(val)) {
            return switch (@field(OverrideUnOp, op)) {
                .@"++" => val + 1,
            };
        }
        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            return switch (@field(OverrideBinOp, op)) {
                .@"^", .@"$" => @TypeOf(
                    @as(Lhs, undefined),
                    @as(Rhs, undefined),
                ),
            };
        }
        pub fn evalBinOp(_: @This(), lhs: anytype, comptime op: []const u8, rhs: anytype) EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return switch (@field(OverrideBinOp, op)) {
                .@"^" => lhs ^ rhs,
                .@"$" => std.math.log(@TypeOf(lhs, rhs), lhs, rhs),
            };
        }
    }{});
    try util.testing.expectEqual(2, comath.eval("(++2 ^ 5) $ 36", op_override_ctx, .{}));
}
