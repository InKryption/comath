const std = @import("std");
const cm = @import("../comath.zig");
const util = @import("util");

const UnOp = enum { @"-" };
const BinOp = std.meta.FieldEnum(@TypeOf(relations));
const relations = .{
    .@"+" = cm.relation(.left, 10),
    .@"+|" = cm.relation(.left, 10),
    .@"+%" = cm.relation(.left, 10),

    .@"-" = cm.relation(.left, 10),
    .@"-|" = cm.relation(.left, 10),
    .@"-%" = cm.relation(.left, 10),

    .@"*" = cm.relation(.left, 20),
    .@"*|" = cm.relation(.left, 20),
    .@"*%" = cm.relation(.left, 20),

    .@"/" = cm.relation(.left, 20),
    .@"%" = cm.relation(.left, 20),

    .@"^" = cm.relation(.right, 30),
};

pub inline fn context(sub_ctx: anytype) Context(@TypeOf(sub_ctx)) {
    const SubCtx = @TypeOf(sub_ctx);
    if (util.NamespaceOf(SubCtx) == null and SubCtx != void) @compileError(
        "Expected struct/union/enum, or pointer to struct/union/enum/opaque, got '" ++ @typeName(SubCtx) ++ "'" ++
            if (SubCtx != type) "" else " (" ++ @typeName(sub_ctx) ++ ")",
    );
    return .{ .sub_ctx = sub_ctx };
}

pub fn Context(comptime SubCtx: type) type {
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();
        const Ns = util.NamespaceOf(SubCtx) orelse cm.ctx.Null;

        pub inline fn matchUnOp(comptime str: []const u8) bool {
            const sub_match = @TypeOf(Ns.matchUnOp) != void and Ns.matchUnOp(str);
            return sub_match or @hasField(UnOp, str);
        }

        pub inline fn matchBinOp(comptime str: []const u8) bool {
            const sub_match = @TypeOf(Ns.matchBinOp) != void and Ns.matchBinOp(str);
            return sub_match or @hasField(BinOp, str);
        }

        pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) ?cm.Order {
            if (@TypeOf(Ns.orderBinOp) != void) blk: {
                const order = Ns.orderBinOp(lhs, rhs) orelse break :blk;
                return order;
            }
            if (!@hasField(@TypeOf(relations), lhs)) return null;
            if (!@hasField(@TypeOf(relations), rhs)) return null;
            return @field(relations, lhs).order(@field(relations, rhs));
        }

        pub fn EvalNumberLiteral(comptime src: []const u8) type {
            if (@TypeOf(Ns.EvalNumberLiteral) != void) blk: {
                if (@TypeOf(Ns.evalNumberLiteral) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalNumberLiteral(src);
                if (T == noreturn) break :blk;
                return T;
            }
            return cm.ctx.DefaultEvalNumberLiteral(src);
        }
        pub inline fn evalNumberLiteral(comptime src: []const u8) EvalNumberLiteral(src) {
            if (@TypeOf(Ns.evalNumberLiteral) != void) blk: {
                const T = Ns.EvalNumberLiteral(src);
                if (T == noreturn) break :blk;
                return Ns.evalNumberLiteral(src);
            }
            return cm.ctx.defaultEvalNumberLiteral(src);
        }

        pub fn EvalIdent(comptime ident: []const u8) type {
            if (@TypeOf(Ns.EvalIdent) != void) blk: {
                if (@TypeOf(Ns.evalIdent) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalIdent(ident);
                if (T == noreturn) break :blk;
                return T;
            }
            return noreturn;
        }
        pub fn evalIdent(ctx: Self, comptime ident: []const u8) !EvalIdent(ident) {
            if (@TypeOf(Ns.evalIdent) != void) blk: {
                const T = Ns.EvalIdent(ident);
                if (T == noreturn) break :blk;
                return ctx.sub_ctx.evalIdent(ident);
            }
            comptime unreachable;
        }

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            if (@TypeOf(Ns.EvalProperty) != void) blk: {
                if (@TypeOf(Ns.evalProperty) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalProperty(Lhs, field);
                if (T == noreturn) break :blk;
                return T;
            }
            return @FieldType(Lhs, field);
        }
        pub fn evalProperty(ctx: Self, lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {
            const Lhs = @TypeOf(lhs);
            if (@TypeOf(Ns.evalProperty) != void) blk: {
                const T = Ns.EvalProperty(Lhs, field);
                if (T == noreturn) break :blk;
                return ctx.sub_ctx.evalProperty(lhs, field);
            }
            return @field(lhs, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            if (@TypeOf(Ns.EvalIndexAccess) != void) blk: {
                if (@TypeOf(Ns.evalIndexAccess) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalIndexAccess(Lhs, Rhs);
                if (T == noreturn) break :blk;
                return T;
            }
            return switch (@typeInfo(Rhs).@"struct".fields.len) {
                0 => Lhs,
                1 => std.meta.Elem(Lhs),
                else => |n| [n]std.meta.Elem(Lhs),
            };
        }
        pub fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            const Rhs = @TypeOf(rhs);
            if (@TypeOf(Ns.evalIndexAccess) != void) blk: {
                const T = Ns.EvalIndexAccess(Lhs, Rhs);
                if (T == noreturn) break :blk;
                return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
            }
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
            if (@TypeOf(Ns.EvalFuncCall) != void) blk: {
                if (@TypeOf(Ns.evalFuncCall) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalFuncCall(Callee, Args);
                if (T == noreturn) break :blk;
                return T;
            }
            return @typeInfo(Callee).@"fn".return_type.?;
        }
        pub fn evalFuncCall(
            ctx: Self,
            callee: anytype,
            args: anytype,
        ) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);
            if (@TypeOf(Ns.evalFuncCall) != void) blk: {
                const T = Ns.EvalFuncCall(Callee, Args);
                if (T == noreturn) break :blk;
                return ctx.sub_ctx.evalFuncCall(callee, args);
            }
            return @call(.auto, callee, args);
        }

        pub fn EvalMethodCall(comptime SelfParam: type, comptime method: []const u8, comptime Args: type) type {
            if (@TypeOf(Ns.EvalMethodCall) != void) blk: {
                if (@TypeOf(Ns.evalMethodCall) == void) @compileError("Can't have a type & value method pair be mismatched");
                const T = Ns.EvalMethodCall(SelfParam, method, Args);
                if (T == noreturn) break :blk;
                return T;
            }

            const SelfNs = util.ImplicitDeref(SelfParam);
            if (!@hasDecl(SelfNs, method)) return noreturn;
            const MethodType = @TypeOf(@field(SelfNs, method));

            if (@typeInfo(MethodType) != .@"fn") return noreturn;
            const params = @typeInfo(MethodType).@"fn".params;

            if (params.len == 0) return noreturn;
            const Expected = util.ImplicitDeref(params[0].type orelse *const SelfParam);

            if (SelfNs != Expected) return noreturn;
            return @typeInfo(MethodType).@"fn".return_type.?;
        }
        pub fn evalMethodCall(
            ctx: Self,
            self_param: anytype,
            comptime method: []const u8,
            args: anytype,
        ) !EvalMethodCall(@TypeOf(self_param), method, @TypeOf(args)) {
            if (@TypeOf(Ns.evalMethodCall) != void) blk: {
                const T = Ns.EvalMethodCall(@TypeOf(self_param), method, @TypeOf(args));
                if (T == noreturn) break :blk;
                return ctx.sub_ctx.evalMethodCall(self_param, method, args);
            }
            const func = @field(util.ImplicitDeref(@TypeOf(self_param)), method);
            return ctx.evalFuncCall(func, .{self_param} ++ args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            if (@TypeOf(Ns.EvalUnOp) != void) {
                if (Ns.EvalUnOp(op, T) != noreturn) {
                    return Ns.EvalUnOp(op, T);
                }
            }

            return switch (@field(UnOp, op)) {
                .@"-" => T,
            };
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            if (@TypeOf(Ns.EvalUnOp) != void) {
                if (Ns.EvalUnOp(op, @TypeOf(val)) != noreturn) {
                    return ctx.sub_ctx.evalUnOp(op, val);
                }
            }
            return switch (@field(UnOp, op)) {
                .@"-" => -val,
            };
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            if (@TypeOf(Ns.EvalBinOp) != void) {
                if (Ns.EvalBinOp(Lhs, op, Rhs) != noreturn) {
                    return Ns.EvalBinOp(Lhs, op, Rhs);
                }
            }

            const lhs: Lhs = switch (@typeInfo(Lhs)) {
                .int, .comptime_int, .float, .comptime_float => 0,
                else => return noreturn,
            };
            const rhs: Rhs = switch (@typeInfo(Rhs)) {
                .int, .comptime_int, .float, .comptime_float => 0,
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
            if (@TypeOf(Ns.EvalBinOp) != void) {
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

test context {
    try std.testing.expectEqual(5, cm.eval("a + b", context({}), .{ .a = 2, .b = 3 }));
    try std.testing.expectEqual(1, cm.eval("a +% b", context({}), .{ .a = @as(u8, std.math.maxInt(u8)), .b = 2 }));
    try std.testing.expectEqual(59049, cm.eval("3^(2 * a + -b)", context({}), .{ .a = 7, .b = 4 }));

    // complex precedence interactions
    try std.testing.expectEqual(6 - 3 + 4 + 2, cm.eval("6*1-3*1+4*1+2", context({}), .{}));

    const op_override_ctx = context(struct {
        const OverrideUnOp = enum { @"++" };
        pub inline fn matchUnOp(comptime str: []const u8) bool {
            return @hasField(OverrideUnOp, str);
        }

        const OverrideBinOp = enum { @"^", @"$" };
        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasField(OverrideBinOp, str);
        }

        pub const orderBinOp = {};

        pub const EvalNumberLiteral = {};
        pub const evalNumberLiteral = {};

        pub const EvalIdent = {};
        pub const evalIdent = {};

        pub const EvalProperty = {};
        pub const evalProperty = {};

        pub const EvalIndexAccess = {};
        pub const evalIndexAccess = {};

        pub const EvalFuncCall = {};
        pub const evalFuncCall = {};

        pub const EvalMethodCall = {};
        pub const evalMethodCall = {};

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            return switch (@field(OverrideUnOp, op)) {
                .@"++" => T,
            };
        }
        pub fn evalUnOp(_: @This(), comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            return switch (@field(OverrideUnOp, op)) {
                .@"++" => val + 1,
            };
        }

        const relations = .{
            .@"$" = .{ .prec = 2, .assoc = .right },
        };

        pub fn EvalBinOp(
            comptime Lhs: type,
            comptime op: []const u8,
            comptime Rhs: type,
        ) type {
            return switch (@field(OverrideBinOp, op)) {
                .@"^", .@"$" => @TypeOf(
                    @as(Lhs, undefined),
                    @as(Rhs, undefined),
                ),
            };
        }
        pub fn evalBinOp(
            _: @This(),
            lhs: anytype,
            comptime op: []const u8,
            rhs: anytype,
        ) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            return switch (@field(OverrideBinOp, op)) {
                .@"^" => lhs ^ rhs,
                .@"$" => std.math.log(@TypeOf(lhs, rhs), lhs, rhs),
            };
        }
    }{});
    try std.testing.expectEqual(2, cm.eval("(++2 ^ 5) $ 36", op_override_ctx, .{}));
}

test "field access" {
    const Vec = struct {
        x: f32,
        y: f32,
    };

    const v: Vec = .{ .x = 1, .y = 2 };

    try std.testing.expectEqual(
        1,
        cm.eval("v.x", context({}), .{ .v = v }),
    );
    try std.testing.expectEqual(
        2,
        cm.eval("v.y", context({}), .{ .v = v }),
    );
}
