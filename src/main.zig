const std = @import("std");
const util = @import("util");

const comath = @This();

pub const Eval = @import("eval.zig").Eval;
pub const eval = @import("eval.zig").eval;

pub const contexts = @import("contexts.zig");

comptime {
    _ = @import("eval.zig");
    _ = contexts;
}

/// Relative order of chained operators
pub const Order = enum(comptime_int) {
    /// The chained operators are incompatible
    incompatible,
    /// The chained operators are evaluated left to right
    lt,
    /// The chained operators are evaluated right to left
    gt,
};

/// Construct a relation struct, useful for defining a table of relationships between operators.
pub inline fn relation(
    comptime assoc: Relation.Associativity,
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

    pub const Associativity = enum(comptime_int) {
        none,
        left,
        right,
    };

    pub inline fn order(comptime lhs: Relation, comptime rhs: Relation) comath.Order {
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
