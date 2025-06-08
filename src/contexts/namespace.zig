const comath = @import("../main.zig");
const std = @import("std");
const util = @import("util");

pub inline fn namespace(ns: anytype) blk: {
    const Ns = @TypeOf(ns);
    const is_value = Ns != type;
    break :blk Namespace(if (is_value) Ns else ns, is_value);
} {
    return .{ .ns = if (@TypeOf(ns) != type) ns };
}

pub fn Namespace(comptime Ns: type, comptime ns_is_value: bool) type {
    return struct {
        ns: if (ns_is_value) Ns else void,
        const Self = @This();

        pub inline fn matchUnOp(comptime str: []const u8) bool {
            _ = str;
            return false;
        }

        pub inline fn matchBinOp(comptime str: []const u8) bool {
            _ = str;
            return false;
        }

        pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) ?comath.Order {
            _ = lhs;
            _ = rhs;
            return null;
        }

        pub fn DefaultEvalNumberLiteral(comptime src: []const u8) type {
            _ = src;
            return noreturn;
        }
        pub fn defaultEvalNumberLiteral(comptime src: []const u8) DefaultEvalNumberLiteral(src) {
            comptime unreachable;
        }

        pub fn EvalIdent(comptime ident: []const u8) type {
            if (ns_is_value) {
                if (!@hasField(Ns, ident)) return noreturn;
                return @FieldType(Ns, ident);
            } else {
                if (!@hasDecl(Ns, ident)) return noreturn;
                return @TypeOf(@field(Ns, ident));
            }
        }
        pub inline fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
            if (ns_is_value) {
                return @field(ctx.ns, ident);
            } else {
                return @field(Ns, ident);
            }
        }

        pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {
            _ = Lhs;
            _ = field;
            return noreturn;
        }
        pub inline fn evalProperty(_: Self, lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {
            comptime unreachable;
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            _ = Lhs;
            _ = Rhs;
            return noreturn;
        }
        pub inline fn evalIndexAccess(_: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            comptime unreachable;
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            if (@hasDecl(Ns, "EvalFuncCall")) {
                if (Ns.EvalFuncCall(Callee, Args) != noreturn) {
                    return Ns.EvalFuncCall(Callee, Args);
                }
            }
            const Ret = @typeInfo(util.ImplicitDeref(Callee)).@"fn".return_type orelse blk: {
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
            _ = op;
            _ = T;
            return noreturn;
        }
        pub inline fn evalUnOp(_: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            comptime unreachable;
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            _ = Lhs;
            _ = op;
            _ = Rhs;
            return noreturn;
        }
        pub inline fn evalBinOp(_: Self, lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            comptime unreachable;
        }
    };
}

fn lerp2(u: f32, first: f32, second: f32) f32 {
    return u * first + (1.0 - u) * second;
}

test namespace {
    const simple_ctx1 = comath.ctx.simple(namespace(struct {
        pub fn lerp(u: f32, first: f32, second: f32) f32 {
            return u * first + (1.0 - u) * second;
        }
    }));
    try std.testing.expectEqual(0.75, comath.eval("lerp(0.5, 0.25, 1.25)", simple_ctx1, .{}));

    const simple_ctx2 = comath.ctx.simple(namespace(.{ .lerp = lerp2 }));
    try std.testing.expectEqual(0.75, comath.eval("lerp(0.5, 0.25, 1.25)", simple_ctx2, .{}));

    try std.testing.expectEqual(
        comath.eval("lerp(0.5, 0.25, 1.25)", simple_ctx1, .{}),
        comath.eval("lerp(0.5, 0.25, 1.25)", simple_ctx2, .{}),
    );
}
