const std = @import("std");
const util = @import("util");

pub const symbols = @import("Tokenizer.zig").operator_characters;

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
    return std.enums.EnumFieldStruct(util.DedupedEnum(BinOpEnum), ?Relation, @as(?Relation, null));
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

pub fn OpEnumUnion(comptime A: type, comptime B: type) type {
    const a_info = @typeInfo(A).Enum;
    const b_info = @typeInfo(B).Enum;
    var fields: []const std.builtin.Type.EnumField = &.{};
    for (a_info.fields, 0..) |a_field, i| {
        fields = fields ++ &[_]std.builtin.Type.EnumField{.{
            .name = a_field.name,
            .value = i,
        }};
    }
    var value = fields.len;
    for (b_info.fields) |b_field| {
        if (@hasField(A, b_field.name)) continue;
        fields = fields ++ &[_]std.builtin.Type.EnumField{.{
            .name = b_field.name,
            .value = value,
        }};
        value += 1;
    }
    return @Type(.{ .Enum = .{
        .tag_type = std.math.IntFittingRange(0, fields.len - 1),
        .is_exhaustive = true,
        .decls = &.{},
        .fields = fields,
    } });
}
