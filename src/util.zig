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

inline fn eqlComptime(comptime T: type, comptime a: []const T, comptime b: []const T) bool {
    comptime return @reduce(.And, vec(a[0..].*) == vec(b[0..].*));
}

pub inline fn vec(comptime T: type, init: anytype) @Vector(init.len, T) {
    return init[0..].*;
}
pub inline fn simdOr(a: anytype, b: anytype) @Vector(@typeInfo(@TypeOf(a, b).Vector.len), bool) {
    const T = @TypeOf(a, b);
    const len = @typeInfo(T).Vector.len;
    const a_bits: @Vector(len, u1) = @bitCast(a);
    const b_bits: @Vector(len, u1) = @bitCast(b);
    return @bitCast(a_bits | b_bits);
}
pub inline fn simdAnd(a: anytype, b: anytype) @Vector(@typeInfo(@TypeOf(a, b).Vector.len), bool) {
    const T = @TypeOf(a, b);
    const len = @typeInfo(T).Vector.len;
    const a_bits: @Vector(len, u1) = @bitCast(a);
    const b_bits: @Vector(len, u1) = @bitCast(b);
    return @bitCast(a_bits & b_bits);
}

/// Parse digits with the given base into a comptime_int.
/// Optimised to use a linear but low amount of eval branch quota
pub const parseComptimeInt = struct {
    fn parseComptimeInt(
        comptime str: []const u8,
        comptime base: u8,
    ) error{ InvalidCharacter, EmptyString }!comptime_int {
        return parseComptimeIntImpl(dedupeSlice(u8, str), base);
    }
    fn parseComptimeIntImpl(
        comptime str: []const u8,
        comptime base: u8,
    ) error{InvalidCharacter}!comptime_int {
        const str_vec = vec(u8, str[0..].*);
        const StrVec = @TypeOf(str_vec);

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
        const dd_hs = dedupeSlice(T, haystack);
        const dd_ex = dedupeSlice(T, excluded);
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
            const prev: @Vector(arr.len, u1) = @bitCast(trues);
            const current: @Vector(arr.len, u1) = @bitCast(vec(T, arr) != ex_vec);
            trues = @bitCast(prev & current);
        }

        return std.simd.firstTrue(trues) orelse return null;
    }
}.indexOfNoneComptime;

pub const dedupeSlice = struct {
    inline fn dedupeSlice(comptime T: type, comptime slice: []const T) *const [slice.len]T {
        comptime return dedupeSliceImpl(slice[0..].*);
    }
    fn dedupeSliceImpl(comptime array: anytype) *const [array.len]@typeInfo(@TypeOf(array)).Array.child {
        comptime {
            const Array = @TypeOf(array);
            assert(Array == [array.len]@typeInfo(Array).Array.child);
            return &array;
        }
    }
}.dedupeSlice;

pub fn ImplicitDeref(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => |info| switch (info.size) {
            .One => info.child,
            else => T,
        },
        else => T,
    };
}
