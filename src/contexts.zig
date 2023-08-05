const std = @import("std");
const util = @import("util");
const eval = @import("eval");

pub inline fn simpleCtx(sub_ctx: anytype) SimpleCtx(@TypeOf(sub_ctx)) {
    return .{ .sub_ctx = sub_ctx };
}
pub fn SimpleCtx(comptime SubCtx: type) type {
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();

        const significant_ctx = switch (@typeInfo(util.ImplicitDeref(SubCtx))) {
            .Struct, .Union, .Enum, .Opaque => true,
            else => false,
        };
        const Ns = if (significant_ctx) util.ImplicitDeref(SubCtx) else struct {};

        pub const UnOp = enum {
            @"-",
        };
        pub const BinOp = enum {
            @"+",
            @"+|",
            @"+%",

            @"-",
            @"-|",
            @"-%",

            @"*",
            @"*|",
            @"*%",

            @"/",
            @"%",

            @"^",
            @"@",
        };
        pub const relations: eval.operator.RelationMap(BinOp) = .{
            .@"+" = .{ .prec = 1, .assoc = .left },
            .@"+|" = .{ .prec = 1, .assoc = .left },
            .@"+%" = .{ .prec = 1, .assoc = .left },

            .@"-" = .{ .prec = 1, .assoc = .left },
            .@"-|" = .{ .prec = 1, .assoc = .left },
            .@"-%" = .{ .prec = 1, .assoc = .left },

            .@"*" = .{ .prec = 2, .assoc = .left },
            .@"*|" = .{ .prec = 2, .assoc = .left },
            .@"*%" = .{ .prec = 2, .assoc = .left },

            .@"/" = .{ .prec = 2, .assoc = .left },
            .@"%" = .{ .prec = 2, .assoc = .left },

            .@"^" = .{ .prec = 3, .assoc = .right },
            .@"@" = .{ .prec = 0, .assoc = .left },
        };

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return Ns.EvalProperty(Lhs, field);
                }
            }
            return std.meta.FieldType(Lhs, field);
        }
        pub inline fn evalProperty(ctx: Self, lhs: anytype, comptime field: []const u8) EvalProperty(@TypeOf(lhs), field) {
            const Lhs = @TypeOf(lhs);
            if (@hasDecl(Ns, "EvalProperty")) {
                if (Ns.EvalProperty(Lhs, field) != noreturn) {
                    return ctx.evalProperty(lhs, field);
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
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@hasDecl(Ns, "EvalIndexAccess")) {
                if (Ns.EvalIndexAccess(Lhs, Rhs) != noreturn) {
                    return ctx.evalIndexAccess(lhs, rhs);
                }
            }
            return lhs[rhs];
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return Ns.EvalFuncCall(Callee, Args);
                }
            }
            return @typeInfo(util.ImplicitDeref(Callee)).Fn.return_type.?;
        }
        pub inline fn evalFuncCall(ctx: Self, callee: anytype, args: anytype) EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);

            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return ctx.evalFuncCall(callee, args);
                }
            }
        }

        pub fn EvalUnOp(comptime op: UnOp, comptime T: type) type {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, T) != noreturn) {
                    return Ns.EvalUnOp(op, T);
                }
            }
            return switch (op) {
                .@"-" => T,
            };
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: UnOp, val: anytype) EvalUnOp(op, @TypeOf(val)) {
            if (@hasDecl(Ns, "EvalUnOp")) {
                if (Ns.EvalUnOp(op, @TypeOf(val)) != noreturn) {
                    return ctx.evalUnOp(op, val);
                }
            }
            return switch (op) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type {
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return Ns.EvalBinOp(Lhs, op, Rhs);
                }
            }
            const lhs: Lhs = 0;
            const rhs: Rhs = 0;
            return switch (op) {
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
                .@"@" => @compileError("No default implementation for '@'"),
            };
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: BinOp, rhs: anytype) EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@hasDecl(Ns, "EvalBinOp")) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return ctx.evalBinOp(lhs, op, rhs);
                }
            }
            return switch (op) {
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
                .@"@" => comptime unreachable,
            };
        }
    };
}

test simpleCtx {
    try util.testing.expectEqual(5, eval.eval("a + b", simpleCtx({}), .{ .a = 2, .b = 3 }));
    try util.testing.expectEqual(1, eval.eval("a +% b", simpleCtx({}), .{ .a = @as(u8, std.math.maxInt(u8)), .b = 2 }));
    try util.testing.expectEqual(387420489, eval.eval("3^(2 * a + b)", simpleCtx({}), .{ .a = 7, .b = 4 }));
}
