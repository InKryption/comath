const std = @import("std");
const util = @import("util");

pub const symbols = @import("Tokenizer.zig").operator_characters;

pub const Order = enum(comptime_int) {
    incompatible,
    lt,
    gt,
};

pub const Associativity = enum(comptime_int) {
    none,
    left,
    right,
};

pub inline fn relation(
    comptime assoc: Associativity,
    comptime prec: comptime_int,
) Relation {
    return .{
        .assoc = assoc,
        .prec = prec,
    };
}
pub const Relation = struct {
    assoc: Associativity,
    prec: comptime_int,

    pub inline fn order(comptime lhs: Relation, comptime rhs: Relation) Order {
        return switch (std.math.order(lhs.prec, rhs.prec)) {
            .lt => .lt,
            .gt => .gt,
            .eq => if (lhs.assoc != rhs.assoc) .incompatible else switch (lhs.assoc) {
                .none => .incompatible,
                .right => .lt,
                .left => .gt,
            },
        };
    }
};
