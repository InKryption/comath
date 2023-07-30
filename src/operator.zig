const std = @import("std");

pub const Associativity = enum(comptime_int) {
    none,
    left,
    right,
};
pub const Relation = struct {
    assoc: Associativity,
    prec: comptime_int,
};

pub fn RelationMap(comptime BinOpEnum: type) type {
    return std.enums.EnumFieldStruct(BinOpEnum, Relation, null);
}

pub inline fn relation(
    comptime assoc: Associativity,
    comptime prec: comptime_int,
) Relation {
    return .{
        .assoc = assoc,
        .prec = prec,
    };
}
