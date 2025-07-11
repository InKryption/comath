const std = @import("std");

pub fn NamespaceOf(comptime T: type) ?type {
    return switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum" => T,
        .pointer => |pointer| blk: {
            if (pointer.size != .One) break :blk null;
            if (pointer.child == anyopaque) break :blk null;
            break :blk switch (@typeInfo(pointer.child)) {
                .@"struct", .@"union", .@"enum" => pointer.child,
                else => break :blk null,
            };
        },
        else => null,
    };
}

pub fn ImplicitDeref(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .pointer => |info| switch (info.size) {
            .One => info.child,
            else => T,
        },
        else => T,
    };
}

pub fn GetPayloadIfErrorUnion(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .error_union => |error_union| error_union.payload,
        else => T,
    };
}

pub inline fn dedupeScalarSlice(
    comptime T: type,
    comptime array: anytype,
) *const [array.len]T {
    comptime return &array;
}

pub inline fn eqlComptime(comptime T: type, comptime a: []const T, comptime b: []const T) bool {
    comptime {
        if (a.len != b.len) return false;
        const len = a.len;
        const V = @Vector(len, T);
        return @reduce(.And, @as(V, a[0..].*) == @as(V, b[0..].*));
    }
}

pub inline fn typeIsComptimeOnly(comptime T: type) ?bool {
    comptime return switch (@typeInfo(T)) {
        .type => true,
        .void => false,
        .bool => false,
        .noreturn => null,
        .int => false,
        .float => false,
        .pointer => |pointer| switch (pointer.size) {
            .One => switch (@typeInfo(pointer.child)) {
                .@"opaque" => false,
                else => typeIsComptimeOnly(pointer.child),
            },
            else => typeIsComptimeOnly(pointer.child),
        },
        .array => |array| array.len != 0 and typeIsComptimeOnly(array.child) orelse return null,
        .@"struct" => |structure| for (structure.fields) |field| {
            if (field.is_comptime) continue;
            if (!(typeIsComptimeOnly(field.type) orelse return null)) continue;
            break true;
        } else false,
        .comptime_float => true,
        .comptime_int => true,
        .undefined => null,
        .null => null,
        .optional => |optional| if (optional.child == noreturn) false else typeIsComptimeOnly(optional.child),
        .error_union => |err_union| blk: {
            if (err_union.payload == noreturn) {
                if (@typeInfo(err_union.error_set).error_set != null and
                    @typeInfo(err_union.error_set).error_set.?.len == 0)
                {
                    break :blk null;
                }
                break :blk false;
            }
            break :blk typeIsComptimeOnly(err_union.payload);
        },
        .error_set => false,
        .@"enum" => |enumeration| typeIsComptimeOnly(enumeration.tag_type).?,
        .@"union" => |@"union"| blk: {
            if (@"union".tag_type != null and
                typeIsComptimeOnly(@"union".tag_type.?).?)
            {
                break :blk true;
            }
            var all_noreturn = true;
            break :blk for (@"union".fields) |field| switch (@typeInfo(field.type)) {
                .noreturn => {},
                else => {
                    all_noreturn = false;
                    if (typeIsComptimeOnly(field.type).?) break true;
                },
            } else if (all_noreturn) null else false;
        },
        .@"fn" => true,
        .@"opaque" => null,
        .frame => false,
        .@"anyframe" => false,
        .vector => |vector| typeIsComptimeOnly(vector.child) and @compileError("Vectors aren't supposed to have comptime-only elements?"),
        .enum_literal => true,
    };
}
