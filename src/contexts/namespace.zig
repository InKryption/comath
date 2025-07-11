const std = @import("std");
const cm = @import("../comath.zig");

pub const Access = enum {
    as_type,
    as_value,
    as_value_then_type,
    as_type_then_value,
};

pub inline fn context(ns: anytype) blk: {
    const Ns = @TypeOf(ns);
    const is_value = Ns != type;
    break :blk Context(
        if (is_value) .as_value else .as_type,
        if (is_value) Ns else ns,
    );
} {
    return .{ .ns = if (@TypeOf(ns) != type) ns };
}

pub fn Context(
    comptime access: Access,
    comptime Ns: type,
) type {
    return struct {
        ns: switch (access) {
            .as_value, .as_value_then_type => Ns,
            .as_type, .as_type_then_value => void,
        },
        const Self = @This();

        pub const matchUnOp = {};
        pub const matchBinOp = {};
        pub const orderBinOp = {};

        pub const EvalNumberLiteral = {};
        pub const evalNumberLiteral = {};

        pub fn EvalIdent(comptime ident: []const u8) type {
            return switch (classifyIdent(ident)) {
                .null => noreturn,
                .as_type => @TypeOf(@field(Ns, ident)),
                .as_value => @FieldType(Ns, ident),
            };
        }

        pub inline fn evalIdent(ctx: @This(), comptime ident: []const u8) !EvalIdent(ident) {
            return switch (classifyIdent(ident)) {
                .null => comptime unreachable,
                .as_type => @field(Ns, ident),
                .as_value => @field(ctx.ns, ident),
            };
        }

        inline fn classifyIdent(comptime ident: []const u8) enum { null, as_type, as_value } {
            comptime {
                switch (access) {
                    .as_type => if (@hasDecl(Ns, ident)) return .as_type,
                    .as_value => if (@hasField(Ns, ident)) return .as_value,
                    .as_value_then_type => {
                        if (@hasField(Ns, ident)) return .as_value;
                        if (@hasDecl(Ns, ident)) return .as_type;
                    },
                    .as_type_then_value => {
                        if (@hasDecl(Ns, ident)) return .as_type;
                        if (@hasField(Ns, ident)) return .as_value;
                    },
                }
                return .null;
            }
        }

        pub const EvalProperty = {};
        pub const evalProperty = {};

        pub const EvalIndexAccess = {};
        pub const evalIndexAccess = {};

        pub const EvalFuncCall = {};
        pub const evalFuncCall = {};

        pub const EvalMethodCall = {};
        pub const evalMethodCall = {};

        pub const EvalUnOp = {};
        pub const evalUnOp = {};

        pub const EvalBinOp = {};
        pub const evalBinOp = {};
    };
}

fn lerp2(u: f32, first: f32, second: f32) f32 {
    return u * first + (1.0 - u) * second;
}

test context {
    const simple_ctx1 = cm.ctx.simple.context(context(struct {
        pub fn lerp(u: f32, first: f32, second: f32) f32 {
            return u * first + (1.0 - u) * second;
        }
    }));
    try std.testing.expectEqual(0.75, cm.eval("lerp(0.5, 0.25, 1.25)", simple_ctx1, .{}));

    const simple_ctx2 = cm.ctx.simple.context(context(.{ .lerp = lerp2 }));
    try std.testing.expectEqual(0.75, cm.eval("lerp(0.5, 0.25, 1.25)", simple_ctx2, .{}));

    try std.testing.expectEqual(
        cm.eval("lerp(0.5, 0.25, 1.25)", simple_ctx1, .{}),
        cm.eval("lerp(0.5, 0.25, 1.25)", simple_ctx2, .{}),
    );
}
