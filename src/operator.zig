const std = @import("std");
const util = @import("util");

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
    return std.enums.EnumFieldStruct(Enum(BinOpEnum), Relation, null);
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

pub fn Enum(comptime E: type) type {
    const info = @typeInfo(E).Enum;
    comptime var fields: [info.fields.len]std.builtin.Type.EnumField = info.fields[0..].*;
    for (&fields, 0..) |*field, i| field.* = .{
        .name = util.dedupeSlice(u8, field.name),
        .value = i,
    };
    std.sort.insertionContext(0, fields.len, struct {
        pub fn swap(a: usize, b: usize) void {
            std.mem.swap([]const u8, &fields[a].name, &fields[b].name);
        }
        pub fn lessThan(a: usize, b: usize) bool {
            return util.orderComptime(u8, fields[a].name, fields[b].name) == .lt;
        }
    });
    return EnumImpl(fields.len, fields);
}
fn EnumImpl(
    comptime field_count: comptime_int,
    comptime fields: [field_count]std.builtin.Type.EnumField,
) type {
    return @Type(.{ .Enum = .{
        .tag_type = std.math.IntFittingRange(0, fields.len -| 1),
        .is_exhaustive = true,
        .decls = &.{},
        .fields = &fields,
    } });
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
