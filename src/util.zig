const std = @import("std");
const assert = std.debug.assert;

const util = @This();

pub const testing = struct {
    pub inline fn expectEqual(a: anytype, b: anytype) !void {
        const T = @TypeOf(a, b);
        return std.testing.expectEqual(@as(T, a), @as(T, b));
    }
    pub inline fn expectEqualDeep(a: anytype, b: anytype) !void {
        const T = @TypeOf(a, b);
        return std.testing.expectEqualDeep(@as(T, a), @as(T, b));
    }
};

pub const dedupe = struct {
    pub inline fn scalarValue(comptime value: anytype) *const @TypeOf(value) {
        return &value;
    }

    pub inline fn scalarSlice(
        comptime T: type,
        comptime array: anytype,
    ) *const [array.len]T {
        return &array;
    }

    pub inline fn sliceSlice(
        comptime T: type,
        comptime slices: []const []const T,
    ) *const [slices.len][]const T {
        comptime {
            var deduped = slices[0..].*;
            for (&deduped) |*slice| slice.* = scalarSlice(T, slice.*);
            return scalarSlice([]const T, &deduped);
        }
    }

    pub inline fn enumValue(value: anytype) Enum(@TypeOf(value)) {
        const T = @TypeOf(value);
        const Deduped = Enum(T);
        if (@inComptime()) return @field(Deduped, @tagName(value));
        if (@typeInfo(T).Enum.is_exhaustive) return switch (value) {
            inline else => |tag| comptime @field(Deduped, @tagName(tag)),
        };
        inline for (@typeInfo(T).Enum.fields) |field| {
            const tag = @field(T, field.name);
            if (tag == value) return @field(Deduped, field.name);
        }
        unreachable;
    }
    pub const Enum = struct {
        fn Enum(comptime E: type) type {
            const info = @typeInfo(E).Enum;
            comptime var fields: [info.fields.len]std.builtin.Type.EnumField = info.fields[0..].*;
            for (&fields, 0..) |*field, i| field.* = .{
                .name = util.dedupe.scalarSlice(u8, field.name[0..].*),
                .value = i,
            };
            if (fields.len == 0) return EnumImpl(fields.len, fields);
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
    }.Enum;
};

pub inline fn eqlComptime(comptime T: type, comptime a: []const T, comptime b: []const T) bool {
    if (a.len != b.len) return false;
    const len = a.len;
    const V = @Vector(len, T);
    comptime return @reduce(.And, @as(V, a[0..].*) == @as(V, b[0..].*));
}

pub inline fn indexOfDiffComptime(comptime T: type, comptime a: []const T, comptime b: []const T) ?comptime_int {
    const shortest = @min(a.len, b.len);
    const V = @Vector(shortest, T);
    const neqls_vec = @as(V, a[0..shortest].*) != @as(V, b[0..shortest].*);
    const mask: std.meta.Int(.unsigned, shortest) = @bitCast(neqls_vec);
    const idx = @clz(mask);
    if (idx == shortest) {
        if (a.len == b.len) return null;
        return shortest;
    }
    return idx;
}

pub inline fn orderComptime(comptime T: type, comptime a: []const T, comptime b: []const T) std.math.Order {
    const diff = comptime indexOfDiffComptime(T, a, b) orelse return .eq;
    comptime if (a.len != b.len) switch (diff) {
        a.len => return .lt,
        b.len => return .gt,
        else => {},
    };
    return if (a[diff] < b[diff]) .lt else .gt;
}

pub fn indexOfNonePosComptime(
    comptime T: type,
    comptime haystack: anytype,
    comptime start: comptime_int,
    comptime excluded: anytype,
) ?comptime_int {
    if (@TypeOf(haystack) != [haystack.len]T) unreachable;
    const offs = indexOfNoneComptime(T, haystack[start..].*, excluded) orelse
        return null;
    return start + offs;
}

pub fn indexOfNoneComptime(
    comptime T: type,
    comptime haystack: anytype,
    comptime excluded: anytype,
) ?comptime_int {
    if (@TypeOf(haystack) != [haystack.len]T) unreachable;
    if (@TypeOf(excluded) != [excluded.len]T) unreachable;
    if (excluded.len == 0) unreachable;

    if (haystack.len == 0) return null;

    const len = haystack.len;

    var mask_bit_vec: @Vector(len, u1) = .{@intFromBool(true)} ** len;
    @setEvalBranchQuota(@min(std.math.maxInt(u32), (excluded.len + 1) * 100));
    for (excluded) |ex| {
        const ex_vec: @Vector(len, T) = @splat(ex);
        const match_bits: @Vector(len, u1) = @bitCast(haystack != ex_vec);
        mask_bit_vec = mask_bit_vec & match_bits;
    }

    const mask: std.meta.Int(.unsigned, len) = @bitCast(mask_bit_vec);
    const idx = @ctz(mask);
    return if (idx == haystack.len) null else idx;
}

pub inline fn containsScalarComptime(
    comptime T: type,
    comptime haystack: anytype,
    comptime needle: T,
) bool {
    comptime {
        if (@TypeOf(haystack) != [haystack.len]T) unreachable;
        const needle_vec: @Vector(haystack.len, T) = @splat(needle);
        const matches = haystack == needle_vec;
        return @reduce(.Or, matches);
    }
}

pub fn ImplicitDeref(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => |info| switch (info.size) {
            .One => info.child,
            else => T,
        },
        else => T,
    };
}

pub fn GetPayloadIfErrorUnion(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .ErrorUnion => |errun| errun.payload,
        else => T,
    };
}

pub inline fn typeIsComptimeOnly(comptime T: type) ?bool {
    comptime return switch (@typeInfo(T)) {
        .Type => true,
        .Void => false,
        .Bool => false,
        .NoReturn => null,
        .Int => false,
        .Float => false,
        .Pointer => |pointer| switch (pointer.size) {
            .One => switch (@typeInfo(pointer.child)) {
                .Opaque => false,
                else => typeIsComptimeOnly(pointer.child),
            },
            else => typeIsComptimeOnly(pointer.child),
        },
        .Array => |array| array.len != 0 and typeIsComptimeOnly(array.child) orelse return null,
        .Struct => |structure| for (structure.fields) |field| {
            if (field.is_comptime) continue;
            if (!(typeIsComptimeOnly(field.type) orelse return null)) continue;
            break true;
        } else false,
        .ComptimeFloat => true,
        .ComptimeInt => true,
        .Undefined => null,
        .Null => null,
        .Optional => |optional| if (optional.child == noreturn) false else typeIsComptimeOnly(optional.child),
        .ErrorUnion => |err_union| blk: {
            if (err_union.payload == noreturn) {
                if (@typeInfo(err_union.error_set).ErrorSet != null and
                    @typeInfo(err_union.error_set).ErrorSet.?.len == 0)
                {
                    break :blk null;
                }
                break :blk false;
            }
            break :blk typeIsComptimeOnly(err_union.payload);
        },
        .ErrorSet => false,
        .Enum => |enumeration| typeIsComptimeOnly(enumeration.tag_type).?,
        .Union => |@"union"| blk: {
            if (@"union".tag_type != null and
                typeIsComptimeOnly(@"union".tag_type.?).?)
            {
                break :blk true;
            }
            var all_noreturn = true;
            break :blk for (@"union".fields) |field| switch (@typeInfo(field.type)) {
                .NoReturn => {},
                else => {
                    all_noreturn = false;
                    if (typeIsComptimeOnly(field.type).?) break true;
                },
            } else if (all_noreturn) null else false;
        },
        .Fn => true,
        .Opaque => null,
        .Frame => false,
        .AnyFrame => false,
        .Vector => |vector| typeIsComptimeOnly(vector.child) and @compileError("Vectors aren't supposed to have comptime-only elements?"),
        .EnumLiteral => true,
    };
}
