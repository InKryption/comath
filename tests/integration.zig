const std = @import("std");
const comath = @import("comath");

fn BigNumCtx(comptime SubCtx: type) type {
    return struct {
        sub_ctx: SubCtx,

        pub const allow_unused_inputs = false;

        pub const UnOp = enum {};
        pub const BinOp = enum {};
        pub const relations: comath.RelationMap(BinOp) = .{};
    };
}
