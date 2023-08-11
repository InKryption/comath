const std = @import("std");
const assert = std.debug.assert;

const util = @This();

pub inline fn simdOr(a: anytype, b: anytype) @Vector(@typeInfo(@TypeOf(a, b)).Vector.len, bool) {
    const T = @TypeOf(a, b);
    const len = @typeInfo(T).Vector.len;
    const a_bits: @Vector(len, u1) = @bitCast(a);
    const b_bits: @Vector(len, u1) = @bitCast(b);
    return @bitCast(a_bits | b_bits);
}
pub inline fn simdAnd(a: anytype, b: anytype) @Vector(@typeInfo(@TypeOf(a, b)).Vector.len, bool) {
    const T = @TypeOf(a, b);
    const len = @typeInfo(T).Vector.len;
    const a_bits: @Vector(len, u1) = @bitCast(a);
    const b_bits: @Vector(len, u1) = @bitCast(b);
    return @bitCast(a_bits & b_bits);
}

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

pub inline fn replaceScalarComptime(
    comptime T: type,
    comptime input: []const T,
    comptime needle: T,
    comptime replacement: T,
) [input.len]T {
    comptime {
        const needle_vec: @Vector(input.len, T) = @splat(needle);
        const replacement_vec: @Vector(input.len, T) = @splat(replacement);
        const matches = input[0..].* == needle_vec;
        return @select(T, matches, replacement_vec, input[0..].*);
    }
}

/// Parse digits with the given base into a comptime_int.
/// Optimised to use a linear but low amount of eval branch quota
pub const parseComptimeInt = struct {
    fn parseComptimeInt(
        comptime str: []const u8,
        comptime base: u8,
    ) error{ InvalidCharacter, EmptyString }!comptime_int {
        return parseComptimeIntImpl(dedupe.scalarSlice(u8, str), base);
    }
    fn parseComptimeIntImpl(
        comptime str: []const u8,
        comptime base: u8,
    ) error{InvalidCharacter}!comptime_int {
        const StrVec = @Vector(str.len, u8);
        const str_vec: StrVec = str[0..].*;

        // const underscore_eqls = str_vec == @as(StrVec, @splat('_'));
        const underscore_eqls = str_vec == .{'_'} ** str.len;
        const digit_eqls = simdAnd(str_vec >= @as(StrVec, @splat('0')), str_vec <= @as(StrVec, @splat('9')));
        const lower_alpha_eqls = simdAnd(str_vec >= @as(StrVec, @splat('a')), str_vec <= @as(StrVec, @splat('z')));
        const upper_alpha_eqls = simdAnd(str_vec >= @as(StrVec, @splat('A')), str_vec <= @as(StrVec, @splat('Z')));
        const ints = str_vec -
            @select(u8, digit_eqls, @as(StrVec, @splat('0')), @as(StrVec, @splat(0))) -
            @select(u8, lower_alpha_eqls, @as(StrVec, @splat('a' + 10)), @as(StrVec, @splat(0))) -
            @select(u8, upper_alpha_eqls, @as(StrVec, @splat('A' + 10)), @as(StrVec, @splat(0)));
        if (!@reduce(.And, simdAnd(
            simdOr(
                ints < @as(StrVec, @splat(base)),
                underscore_eqls,
            ),
            simdOr(
                simdOr(digit_eqls, underscore_eqls),
                simdOr(lower_alpha_eqls, upper_alpha_eqls),
            ),
        ))) return error.InvalidCharacter;

        const MapSubEntry = struct { sub: u8, base: u8 };
        var digit_map_sub: [255]MapSubEntry = .{MapSubEntry{ .sub = undefined, .base = 0 }};
        digit_map_sub['0' .. '9' + 1].* = .{.{ .base = base, .sub = '0' + 0 }} ** ('9' + 1 - '0');
        digit_map_sub['a' .. 'z' + 1].* = .{.{ .base = base, .sub = 'a' + 10 }} ** ('z' + 1 - 'a');
        digit_map_sub['A' .. 'Z' + 1].* = .{.{ .base = base, .sub = 'A' + 10 }} ** ('Z' + 1 - 'A');

        @setEvalBranchQuota(@min(std.math.maxInt(u32), str.len + 1));
        var x = 0;
        for (str) |c| {
            const dms = digit_map_sub[c];
            const value = c - dms.sub;
            x *= dms.base;
            x += value;
        }
        return x;
    }
}.parseComptimeInt;

pub inline fn indexOfNonePosComptime(
    comptime T: type,
    comptime haystack: []const T,
    comptime start: comptime_int,
    comptime excluded: []const T,
) ?comptime_int {
    const offs = indexOfNoneComptime(T, haystack[start..], excluded) orelse
        return null;
    return start + offs;
}
pub const indexOfNoneComptime = struct {
    fn indexOfNoneComptime(
        comptime T: type,
        comptime haystack: []const T,
        comptime excluded: []const T,
    ) ?comptime_int {
        const dd_hs = dedupe.scalarSlice(T, haystack[0..].*);
        const dd_ex = dedupe.scalarSlice(T, excluded[0..].*);
        return indexOfNoneComptimeImpl(T, dd_hs, dd_ex);
    }
    fn indexOfNoneComptimeImpl(
        comptime T: type,
        comptime haystack: []const T,
        comptime excluded: []const T,
    ) ?comptime_int {
        assert(excluded.len != 0);
        if (haystack.len == 0) return null;

        const arr = haystack[0..].*;

        var trues: @Vector(arr.len, bool) = .{true} ** arr.len;
        @setEvalBranchQuota(@min(std.math.maxInt(u32), (excluded.len + 1) * 100));
        for (excluded) |ex| {
            const ex_vec: @Vector(arr.len, T) = @splat(ex);
            // const prev: @Vector(arr.len, u1) = @bitCast(trues);
            const current = arr != ex_vec;
            // trues = @bitCast(prev & current);
            trues = simdAnd(trues, current);
        }

        const mask: std.meta.Int(.unsigned, arr.len) = @bitCast(trues);
        const idx = @ctz(mask);
        return if (idx == haystack.len) null else idx;
    }
}.indexOfNoneComptime;

pub inline fn implicitDeref(ptr_or_val: anytype) ImplicitDeref(@TypeOf(ptr_or_val)) {
    return switch (@typeInfo(@TypeOf(ptr_or_val))) {
        .Pointer => |info| switch (info.size) {
            .One => ptr_or_val.*,
            else => ptr_or_val,
        },
        else => ptr_or_val,
    };
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

pub inline fn namespaceDecl(comptime Ns: type, comptime name: []const u8) ?if (@hasDecl(Ns, name)) @TypeOf(@field(Ns, name)) else noreturn {
    return if (@hasDecl(Ns, name)) @field(Ns, name) else null;
}
