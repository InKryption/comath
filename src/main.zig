const std = @import("std");
const assert = std.debug.assert;

fn tokenizeExpr(comptime expr_str: []const u8) []const Token {
    if (dedupeSlice(u8, expr_str) != expr_str.ptr) {
        return tokenizeExpr(dedupeSlice(u8, expr_str));
    }

    var tokens: []const Token = &.{};

    @setEvalBranchQuota(expr_str.len * 1000);
    var i = 0;
    while (i < expr_str.len) switch (expr_str.ptr[i]) {
        ' ', '\t', '\n', '\r' => i += 1,
        '(' => {
            tokens = tokens ++ &[_]Token{.paren_open};
            i += 1;
        },
        ')' => {
            tokens = tokens ++ &[_]Token{.paren_close};
            i += 1;
        },
        '.' => {
            tokens = tokens ++ &[_]Token{.period};
            i += 1;
        },
        '\'' => {
            const start = i;
            i += 1;
            const end = while (std.mem.indexOfScalarPos(u8, expr_str, i, '\'')) |end| {
                i = end + 1;
                switch (expr_str[end - 1]) {
                    '\\' => continue,
                    '\'' => @compileError("Empty character literal in '" ++ expr_str ++ "'"),
                    else => {},
                }
                break i;
            } else @compileError("Unclosed character literal in '" ++ expr_str ++ "'");

            const char_src = expr_str[start..end];
            const codepoint = switch (std.zig.parseCharLiteral(char_src)) {
                .success => |codepoint| codepoint,
                .failure => |failure| handleStringParseFailure(char_src, failure),
            };
            tokens = tokens ++ &[_]Token{.{ .char_literal = codepoint }};
        },
        '0'...'9' => {
            const token = blk: {
                var tokenizer = std.zig.Tokenizer.init(expr_str ++ &[_:0]u8{});
                tokenizer.index = i;
                break :blk tokenizer.next();
            };

            switch (token.tag) {
                .number_literal => {},
                else => @compileError( //
                    "Unexpected tag '" ++ @tagName(token.tag) ++
                    "' from '" ++ expr_str[token.loc.start..token.loc.end] //
                ),
            }

            const num_src = expr_str[token.loc.start..token.loc.end];
            const val = switch (std.zig.parseNumberLiteral(num_src)) {
                .int => |int| Token{ .integer = int },
                .big_int => |base| blk: {
                    const digits = num_src[if (base != .decimal) 2 else 0..].*;
                    const int = parseComptimeIntDigits(@intFromEnum(base), digits) catch |err| @compileError(std.fmt.comptimePrint(
                        "Encountered '{s}' in '{s}' while parsing integer",
                        .{ @errorName(err), num_src },
                    ));
                    break :blk Token{ .integer = int };
                },
                .float => Token{ .float = std.fmt.parseFloat(f128, num_src) catch |err| switch (err) {
                    error.InvalidCharacter => |e| @compileError(@errorName(e) ++ " in '" ++ num_src ++ "' while parsing float"),
                } },
                .failure => |failure| handleNumberParseFailure(num_src, failure),
            };
            tokens = tokens ++ &[_]Token{val};
            i = token.loc.end;
        },
        else => {
            const start = i;

            const remaining = expr_str[start..].*;
            const remain_vec: @Vector(remaining.len, u8) = remaining;

            var trues: @Vector(remaining.len, bool) = .{false} ** remaining.len;

            // use SIMD to avoid needing too much eval branch quota
            const excluded_chars = [_]u8{ ' ', '\t', '\n', '\r', '_', '(', ')', '\'', '\"', ',', '.' };
            @setEvalBranchQuota(excluded_chars.len * 10);
            for (excluded_chars) |excluded| {
                const excluded_vec = @splat(remaining.len, excluded);
                const prev_bits: @Vector(remaining.len, u1) = @bitCast(trues);
                const current_bits: @Vector(remaining.len, u1) = @bitCast(remain_vec == excluded_vec);
                trues = @bitCast(prev_bits | current_bits);
            }
            const first_true: comptime_int = std.simd.firstTrue(trues) orelse remaining.len;
            tokens = tokens ++ &[_]Token{.{ .other = expr_str[start..][0..first_true] }};
            i += first_true;
        },
    };

    return tokens;
}

test tokenizeExpr {
    try comptime std.testing.expectEqualDeep(
        tokenizeExpr("3"),
        &[_]Token{.{ .integer = 3 }},
    );
    try comptime std.testing.expectEqualDeep(
        tokenizeExpr("7.1234"),
        &[_]Token{.{ .float = 7.1234 }},
    );
    try comptime std.testing.expectEqualDeep(
        tokenizeExpr("7.1234 / 2"),
        &[_]Token{
            .{ .float = 7.1234 },
            .{ .other = "/" },
            .{ .integer = 2 },
        },
    );

    {
        const expected_tokens = tokenizeExpr("foo(7.1234 / 2)");
        try comptime std.testing.expectEqualDeep(
            expected_tokens,
            &[_]Token{
                .{ .other = "foo" },
                .paren_open,
                .{ .float = 7.1234 },
                .{ .other = "/" },
                .{ .integer = 2 },
                .paren_close,
            },
        );
        try comptime std.testing.expectEqualDeep(
            expected_tokens,
            tokenizeExpr("foo ( 7.1234/ 2)"),
        );
    }
}

const Token = union(enum) {
    paren_open,
    paren_close,
    comma,
    period,
    char_literal: comptime_int,
    integer: comptime_int,
    float: comptime_float,
    other: []const u8,
};

inline fn handleStringParseFailure(char_src: []const u8, failure: std.zig.string_literal.Error) noreturn {
    switch (failure) {
        .invalid_escape_character => |idx| @compileError(std.fmt.comptimePrint(
            "Unrecognized escape character '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_hex_digit => |idx| @compileError(std.fmt.comptimePrint(
            "Expected hex digit, got '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .empty_unicode_escape_sequence => @compileError("Found empty unicode escape sequence in " ++ char_src),
        .expected_hex_digit_or_rbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected hex digit or rbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .invalid_unicode_codepoint => @compileError("Invalid unicode codepoint in " ++ char_src),
        .expected_lbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected lbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_rbrace => |idx| @compileError(std.fmt.comptimePrint(
            "Expected rbrace, found '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
        .expected_single_quote => @compileError("Unescaped single quote in " ++ char_src),
        .invalid_character => |idx| @compileError(std.fmt.comptimePrint(
            "Invalid character '{c}' in {s}",
            .{ char_src[idx], char_src },
        )),
    }
}

inline fn handleNumberParseFailure(num_src: []const u8, failure: std.zig.number_literal.Error) noreturn {
    switch (failure) {
        .leading_zero => @compileError("Invalid leading zeroes in '" ++ num_src ++ "'"),
        .digit_after_base => @compileError("Expected digit after base in '" ++ num_src ++ "'"),
        .upper_case_base, .invalid_float_base => @compileError("Invalid base in '" ++ num_src ++ "'"),
        .repeated_underscore, .invalid_underscore_after_special => @compileError("Invalid underscore in '" ++ num_src ++ "'"),
        .invalid_digit => |info| @compileError(std.fmt.comptimePrint(
            "Invalid digit '{c}' in '{s}' with base '{s}'",
            .{ num_src[info.i], num_src, @tagName(info.base) },
        )),
        .invalid_digit_exponent => |exp_idx| @compileError(std.fmt.comptimePrint(
            "Invalid exponent '{c}' in '{s}'",
            .{ num_src[exp_idx], num_src },
        )),
        .duplicate_period => @compileError("Duplicate periods in '" ++ num_src ++ "'"),
        .duplicate_exponent => @compileError("Duplicate exponents in '" ++ num_src ++ "'"),
        .exponent_after_underscore => @compileError("Exponent after underscore in '" ++ num_src ++ "'"),
        .special_after_underscore => |spec_idx| @compileError(std.fmt.comptimePrint(
            "Invalid '{c}' after underscore in '{s}'",
            .{ num_src[spec_idx], num_src },
        )),
        .trailing_special,
        .trailing_underscore,
        .invalid_character,
        .invalid_exponent_sign,
        => |err_idx| @compileError(std.fmt.comptimePrint(
            "Invalid '{c}' in '{s}'",
            .{ num_src[err_idx], num_src },
        )),
    }
}

inline fn parseComptimeIntDigits(
    comptime base: u8,
    comptime digits: anytype,
) error{InvalidCharacter}!comptime_int {
    assert(@TypeOf(digits) == [digits.len]u8);
    var x = 0;
    for (digits) |c| {
        if (c == '_') continue;
        const digit_val = try std.fmt.charToDigit(c, base);
        x *= base;
        x += digit_val;
    }
    return x;
}

inline fn dedupeSliceSlice(
    comptime T: type,
    comptime slices: []const []const T,
) *const [slices.len][]const T {
    comptime return dedupeSliceSliceImpl(T, slices[0..].*, false);
}
fn dedupeSliceSliceImpl(
    comptime T: type,
    comptime array: anytype,
    comptime checked: bool,
) *const [array.len][]const T {
    const Array = @TypeOf(array);
    comptime assert(Array == [array.len]@typeInfo(Array).Array.child);
    if (checked) return &array;

    var must_dedupe = false;
    var result = array;
    for (&result) |*item| {
        if (dedupeSlice(T, item.*) == item.*.ptr) continue;
        must_dedupe = true;
        item.* = dedupeSlice(T, item.*);
    }

    if (must_dedupe)
        return dedupeSliceSliceImpl(T, result, true);
    return &result;
}

inline fn dedupeSlice(comptime T: type, comptime slice: []const T) *const [slice.len]T {
    comptime return dedupeSliceImpl(slice[0..].*);
}
noinline fn dedupeSliceImpl(comptime array: anytype) *const [array.len]@typeInfo(@TypeOf(array)).Array.child {
    comptime {
        const Array = @TypeOf(array);
        assert(Array == [array.len]@typeInfo(Array).Array.child);
        return &array;
    }
}
