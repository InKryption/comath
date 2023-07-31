const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");

const Tokenizer = @This();
index: comptime_int = 0,
can_be_unary: bool = true,

pub const operator_symbols = [_]u8{
    '!', '#', '$', '%', '&', '*',
    '+', '-', '/', '<', '=', '>',
    '?', '@', '~', '^', '|', ':',
};

pub inline fn next(
    comptime state: *Tokenizer,
    comptime buffer: []const u8,
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
) ?Token {
    comptime return state.nextImpl(buffer, UnOpEnum, BinOpEnum);
}
fn nextImpl(
    comptime tokenizer: *Tokenizer,
    comptime buffer: []const u8,
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
) ?Token {
    const peek_res = tokenizer.peekImpl(buffer, UnOpEnum, BinOpEnum) orelse return null;
    tokenizer.* = peek_res.state;
    return peek_res.token;
}

pub inline fn peek(comptime state: Tokenizer, comptime buffer: []const u8) ?Token {
    const result = state.peekImpl(util.dedupeSlice(u8, buffer)) orelse return null;
    return result.token;
}
const PeekRes = struct {
    token: Token,
    state: Tokenizer,
};

fn peekImpl(
    comptime state: Tokenizer,
    comptime buffer: []const u8,
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
) ?PeekRes {
    const OpEnum = if (state.can_be_unary) UnOpEnum else BinOpEnum;
    switch ((buffer ++ &[_:0]u8{})[state.index]) {
        0 => return null,
        ' ', '\t', '\n', '\r' => {
            const whitespace_end = util.indexOfNonePosComptime(u8, buffer, state.index + 1, &.{ ' ', '\t', '\n', '\r' }) orelse buffer.len;
            var fwd_state = state;
            fwd_state.index = whitespace_end;
            return fwd_state.peekImpl(buffer, UnOpEnum, BinOpEnum);
        },
        '(' => return .{ .state = .{ .index = state.index + 1, .can_be_unary = true }, .token = .paren_open },
        ')' => return .{ .state = .{ .index = state.index + 1, .can_be_unary = false }, .token = .paren_close },
        '[' => return .{ .state = .{ .index = state.index + 1, .can_be_unary = true }, .token = .bracket_open },
        ']' => return .{ .state = .{ .index = state.index + 1, .can_be_unary = false }, .token = .bracket_close },
        'a'...'z',
        'A'...'Z',
        '_',
        => {
            const start = state.index;
            const end = endOfIdent(buffer, start);
            const ident = buffer[start..end];
            return .{
                .state = .{ .index = end, .can_be_unary = false },
                .token = .{ .ident = ident },
            };
        },
        '.' => {
            const start = state.index + 1;
            const end = endOfIdent(buffer, start);
            const ident = buffer[start..end];
            if (ident.len == 0) @compileError("Expected identifier following period");
            return .{
                .state = .{ .index = end, .can_be_unary = false },
                .token = .{ .field = ident },
            };
        },
        '0'...'9', '\'' => {
            var idx = state.index;
            @setEvalBranchQuota(@min(std.math.maxInt(u32), (buffer.len - idx) * 100));
            var zig_tokenizer = std.zig.Tokenizer.init(buffer[idx..] ++ &[_:0]u8{});
            const tok = zig_tokenizer.next();

            const literal_src = buffer[idx..][tok.loc.start..tok.loc.end];
            idx += literal_src.len;

            @setEvalBranchQuota(@min(std.math.maxInt(u32), buffer.len - idx));
            const literal_tok: Token = switch (tok.tag) {
                .char_literal => Token{ .char = parseCharLiteral(literal_src[0..].*) },
                .number_literal => switch (std.zig.parseNumberLiteral(literal_src)) {
                    .int => |int| Token{ .integer = int },
                    .big_int => |base| Token{ .integer = blk: {
                        const digits = literal_src[if (base == .decimal) 0 else 2..].*;
                        break :blk util.parseComptimeInt(@intFromEnum(base), digits) catch |err| @compileError(
                            "Encountered '" ++ @errorName(err) ++ "' while parsing '" ++ literal_src ++ "'",
                        );
                    } },
                    .float => Token{ .float = literal_src },
                    .failure => |failure| handleNumberParseFailure(literal_src, failure),
                },
                else => @compileError( //
                    "Unexpected token '" ++ literal_src ++ //
                    "' with tag '" ++ @tagName(tok.tag) ++ "'" //
                ),
            };
            return .{
                .state = .{ .index = idx, .can_be_unary = false },
                .token = literal_tok,
            };
        },
        else => |first_byte| {
            const start = state.index;
            const symbols_end = util.indexOfNonePosComptime(u8, buffer, start, &operator_symbols) orelse buffer.len;
            if (start == symbols_end) @compileError("Unexpected byte '" ++ &.{first_byte} ++ "'");
            var end = symbols_end;
            while (end != start) : (end -= 1) {
                if (@hasField(OpEnum, buffer[start..end])) break;
            } else @compileError("Unknown operator '" ++ buffer[start..symbols_end] ++ "'");
            return .{
                .state = .{ .index = end, .can_be_unary = true },
                .token = @unionInit(Token, if (state.can_be_unary) "un_op" else "bin_op", buffer[start..end]),
            };
        },
    }
    return null;
}
inline fn endOfIdent(
    comptime buffer: []const u8,
    comptime start: comptime_int,
) comptime_int {
    return util.indexOfNonePosComptime(u8, buffer, start + 1, //
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ //
        "abcdefghijklmnopqrstuvwxyz" ++ //
        "0123456789" ++ "_" //
    ) orelse buffer.len;
}

fn testTokenizer(
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
    comptime buffer: []const u8,
    comptime expected: []const Token,
) !void {
    comptime var tokenizer = Tokenizer{};
    inline for (expected) |tok| {
        try util.testing.expectEqualDeep(tok, tokenizer.next(buffer, UnOpEnum, BinOpEnum));
    }
    try util.testing.expectEqualDeep(null, tokenizer.next(buffer, UnOpEnum, BinOpEnum));
}

test Tokenizer {
    try testTokenizer(
        enum { @"-", @"!", @"~" },
        enum { @"-", @"+", @"*", @"/", @"%", @"^", @"|" },
        \\~    3.0   +       3     -
        \\-    !     (       'a'   ]
        \\*    )     /       [     ]
        \\^    ]     |     a_b_C   %
    ,
        &.{
            .{ .un_op = "~" },  .{ .float = "3.0" },      .{ .bin_op = "+" },    .{ .integer = 3 },       .{ .bin_op = "-" },
            .{ .un_op = "-" },  .{ .un_op = "!" },        .{ .paren_open = {} }, .{ .char = 'a' },        .{ .bracket_close = {} },
            .{ .bin_op = "*" }, .{ .paren_close = {} },   .{ .bin_op = "/" },    .{ .bracket_open = {} }, .{ .bracket_close = {} },
            .{ .bin_op = "^" }, .{ .bracket_close = {} }, .{ .bin_op = "|" },    .{ .ident = "a_b_C" },   .{ .bin_op = "%" },
        },
    );

    try testTokenizer(enum { @"~" }, enum { @"+", @"*", @"-" }, "(x - 2) * (3 + ~y)", &.{
        .paren_open,
        .{ .ident = "x" },
        .{ .bin_op = "-" },
        .{ .integer = 2 },
        .paren_close,
        .{ .bin_op = "*" },
        .paren_open,
        .{ .integer = 3 },
        .{ .bin_op = "+" },
        .{ .un_op = "~" },
        .{ .ident = "y" },
        .paren_close,
    });
}

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

inline fn parseCharLiteral(comptime char_src: anytype) comptime_int {
    assert(@TypeOf(char_src) == [char_src.len]u8);
    assert('\'' == char_src[0]);
    assert('\'' == char_src[char_src.len - 1]);
    if (char_src.len < 3) @compileError("Invalid empty character '" ++ &char_src ++ "'");
    return switch (std.zig.parseCharLiteral(&char_src)) {
        .success => |codepoint| codepoint,
        .failure => |failure| handleStringParseFailure(&char_src, failure),
    };
}
pub const Token = union(enum) {
    ident: []const u8,
    /// '.' field
    field: []const u8,
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,

    un_op: []const u8,
    bin_op: []const u8,

    /// '('
    paren_open,
    /// ')'
    paren_close,
    /// '['
    bracket_open,
    /// ']'
    bracket_close,
};
