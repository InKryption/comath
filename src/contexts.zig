const comath = @import("main.zig");
const std = @import("std");
const util = @import("util");

const simple_ctx = @import("contexts/simple.zig");
pub const Simple = simple_ctx.SimpleCtx;
pub const simple = simple_ctx.simpleCtx;

const fn_method_ctx = @import("contexts/fn_method.zig");
pub const FnMethod = fn_method_ctx.FnMethodCtx;
pub const fnMethod = fn_method_ctx.fnMethodCtx;

comptime {
    _ = simple_ctx;
    _ = fn_method_ctx;
}

pub fn DefaultEvalNumberLiteral(comptime src: []const u8) type {
    return switch (std.zig.parseNumberLiteral(src)) {
        .int, .big_int => comptime_int,
        .float => comptime_float,
        .failure => noreturn,
    };
}
pub fn defaultEvalNumberLiteral(comptime src: []const u8) DefaultEvalNumberLiteral(src) {
    return switch (std.zig.parseNumberLiteral(src)) {
        .int => |val| val,
        .big_int => |base| blk: {
            var x = 0;
            const digits_start = if (base == .decimal) 0 else 2;
            for (src[digits_start..]) |digit| {
                const val = switch (digit) {
                    '0'...'9' => digit - '0',
                    'A'...'F' => digit - 'A' + 10,
                    'a'...'f' => digit - 'a' + 10,
                    else => @compileError("Invalid integer literal '" ++ src ++ "'"),
                };
                if (val >= @intFromEnum(base)) @compileError("Invalid integer literal '" ++ src ++ "'");
                x *= @intFromEnum(base);
                x += val;
            }
            break :blk x;
        },
        .float => std.fmt.parseFloat(f128, src) catch |err| @compileError(@errorName(err)),
        .failure => |failure| @compileError(switch (failure) {
            .leading_zero => "Invalid leading zeroes in '" ++ src ++ "'",
            .digit_after_base => "Expected digit after base in '" ++ src ++ "'",
            .upper_case_base, .invalid_float_base => "Invalid base in '" ++ src ++ "'",
            .repeated_underscore, .invalid_underscore_after_special => "Invalid underscore in '" ++ src ++ "'",
            .invalid_digit => |info| std.fmt.comptimePrint(
                "Invalid digit '{c}' in '{s}' with base '{s}'",
                .{ src[info.i], src, @tagName(info.base) },
            ),
            .invalid_digit_exponent => |exp_idx| std.fmt.comptimePrint(
                "Invalid exponent '{c}' in '{s}'",
                .{ src[exp_idx], src },
            ),
            .duplicate_period => "Duplicate periods in '" ++ src ++ "'",
            .duplicate_exponent => "Duplicate exponents in '" ++ src ++ "'",
            .exponent_after_underscore => "Exponent after underscore in '" ++ src ++ "'",
            .special_after_underscore => |spec_idx| std.fmt.comptimePrint(
                "Invalid '{c}' after underscore in '{s}'",
                .{ src[spec_idx], src },
            ),
            .trailing_special,
            .trailing_underscore,
            .invalid_character,
            .invalid_exponent_sign,
            => |err_idx| std.fmt.comptimePrint(
                "Invalid '{c}' in '{s}'",
                .{ src[err_idx], src },
            ),
        }),
    };
}
