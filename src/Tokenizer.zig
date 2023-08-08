const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");

const Tokenizer = @This();
index: comptime_int = 0,
op_symbols_end: ?comptime_int = null,

pub const operator_characters: []const u8 = &[_]u8{
    '!', '#', '$', '%', '&', '*',
    '+', '-', '/', '<', '=', '>',
    '?', '@', '~', '^', '|', ':',
};
pub const identifier_characters: []const u8 =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
    "abcdefghijklmnopqrstuvwxyz" ++
    "0123456789" ++ "_";

pub const Token = union(enum) {
    /// no more tokens left
    eof,

    ident: []const u8,
    /// '.' field
    field: []const u8,
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,

    /// Must call `nextOp` in order to advance
    op_symbols,

    /// ','
    comma,
    /// '('
    paren_open,
    /// ')'
    paren_close,
    /// '['
    bracket_open,
    /// ']'
    bracket_close,
};

pub inline fn next(
    comptime tokenizer: *Tokenizer,
    comptime buffer: []const u8,
) Token {
    comptime {
        const result = tokenizer.peekImpl(util.dedupeSlice(u8, buffer));
        tokenizer.* = result.state;
        return result.token;
    }
}
pub inline fn nextOp(
    comptime tokenizer: *Tokenizer,
    comptime buffer: []const u8,
    comptime OpEnum: type,
) OpEnum {
    comptime {
        const result = tokenizer.peekOpImpl(util.dedupeSlice(u8, buffer), OpEnum);
        tokenizer.* = result.state;
        return @field(OpEnum, result.op[0..]);
    }
}
pub inline fn peek(
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
) ?Token {
    const result = tokenizer.peekImpl(util.dedupeSlice(u8, buffer));
    return result.token;
}
pub inline fn peekOp(
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
    comptime OpEnum: type,
) OpEnum {
    comptime {
        const result = tokenizer.peekOpImpl(util.dedupeSlice(u8, buffer), OpEnum);
        return result.op;
    }
}

const PeekOpRes = struct {
    op: []const u8,
    state: Tokenizer,
};
fn peekOpImpl(
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
    comptime OpEnum: type,
) PeekOpRes {
    comptime {
        const start = tokenizer.index;
        const end = tokenizer.op_symbols_end orelse @compileError("Must only call after encountering `.op_symbols`");
        const op = tokenizeOpEnumFrom(OpEnum, util.dedupeSlice(u8, buffer[start..end]));
        const new_index = tokenizer.index + op.len;
        return .{
            .op = op,
            .state = .{
                .index = new_index,
                .op_symbols_end = if (new_index != end) end else null,
            },
        };
    }
}
fn tokenizeOpEnumFrom(comptime OpEnum: type, comptime buffer: []const u8) []const u8 {
    comptime {
        var end = buffer.len;
        while (end != 0 and !@hasField(OpEnum, buffer[0..end])) : (end -= 1) {}
        if (end != 0) return util.dedupeSlice(u8, buffer[0..end]);
        @compileError("Unrecognized operator '" ++ buffer ++ "'");
    }
}

const PeekRes = struct {
    token: Token,
    state: Tokenizer,
};
fn peekImpl(
    comptime tokenizer: Tokenizer,
    comptime buffer: []const u8,
) PeekRes {
    if (tokenizer.op_symbols_end != null) return .{
        .token = .op_symbols,
        .state = tokenizer,
    };
    switch ((buffer ++ &[_:0]u8{})[tokenizer.index]) {
        0 => return .{
            .state = tokenizer,
            .token = .eof,
        },
        ' ', '\t', '\n', '\r' => {
            const whitespace_end = util.indexOfNonePosComptime(u8, buffer, tokenizer.index + 1, &.{ ' ', '\t', '\n', '\r' }) orelse buffer.len;
            var fwd_state = tokenizer;
            fwd_state.index = whitespace_end;
            return fwd_state.peekImpl(buffer);
        },
        ',' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .comma },
        '(' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .paren_open },
        ')' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .paren_close },
        '[' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .bracket_open },
        ']' => return .{ .state = .{ .index = tokenizer.index + 1 }, .token = .bracket_close },
        'a'...'z',
        'A'...'Z',
        '_',
        => {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer, start + 1, identifier_characters) orelse buffer.len;
            const ident = buffer[start..end];
            return .{
                .state = .{ .index = end },
                .token = .{ .ident = ident },
            };
        },
        '.' => {
            const start = tokenizer.index + 1;
            const end = util.indexOfNonePosComptime(u8, buffer, start + 1, identifier_characters ++ operator_characters) orelse buffer.len;
            const ident = buffer[start..end];
            if (ident.len == 0) @compileError("Expected identifier following period");
            return .{
                .state = .{ .index = end },
                .token = .{ .field = ident },
            };
        },
        '0'...'9', '\'' => {
            var idx = tokenizer.index;
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
                .state = .{ .index = idx },
                .token = literal_tok,
            };
        },
        else => |first_byte| {
            const start = tokenizer.index;
            const end = util.indexOfNonePosComptime(u8, buffer, start, operator_characters) orelse buffer.len;
            if (start == end) @compileError("Unexpected byte '" ++ &[_]u8{first_byte} ++ "'");
            return .{
                .state = .{ .index = start, .op_symbols_end = end },
                .token = .op_symbols,
            };
        },
    }
    return null;
}

fn testTokenizer(
    comptime buffer: []const u8,
    comptime OpEnum: type,
    comptime expected: anytype,
) !void {
    comptime var tokenizer = Tokenizer{};
    inline for (expected) |tok| {
        if ((@TypeOf(tok) == @TypeOf(.enum_literal) or
            @TypeOf(tok) == OpEnum) and @hasField(OpEnum, @tagName(tok)))
        {
            try util.testing.expectEqualDeep(Token{ .op_symbols = {} }, tokenizer.next(buffer));
            try util.testing.expectEqual(@as(OpEnum, tok), tokenizer.nextOp(buffer, OpEnum));
        } else {
            try util.testing.expectEqualDeep(@as(Token, tok), tokenizer.next(buffer));
        }
    }
    try util.testing.expectEqualDeep(.eof, tokenizer.next(buffer));
    try util.testing.expectEqualDeep(.eof, tokenizer.next(buffer));
}

test Tokenizer {
    try testTokenizer(
        \\~      3.0    +       3     -
        \\-      !      (       'a'   ]
        \\*      )      /       [     ]
        \\^      ]      |     a_b_C   %
        \\.foo   .b@r   .?
    ,
        enum { @"~", @"+", @"-", @"!", @"*", @"^", @"|", @"%", @"/" },
        .{
            .@"~",               .{ .float = "3.0" },      .@"+",                 .{ .integer = 3 },       .@"-",
            .@"-",               .@"!",                    .{ .paren_open = {} }, .{ .char = 'a' },        .{ .bracket_close = {} },
            .@"*",               .{ .paren_close = {} },   .@"/",                 .{ .bracket_open = {} }, .{ .bracket_close = {} },
            .@"^",               .{ .bracket_close = {} }, .@"|",                 .{ .ident = "a_b_C" },   .@"%",
            .{ .field = "foo" }, .{ .field = "b@r" },      .{ .field = "?" },
        },
    );

    try testTokenizer("(x - 2) * (3 + ~y)", enum { @"-", @"*", @"+", @"~" }, &.{
        .paren_open,
        .{ .ident = "x" },
        .@"-",
        .{ .integer = 2 },
        .paren_close,
        .@"*",
        .paren_open,
        .{ .integer = 3 },
        .@"+",
        .@"~",
        .{ .ident = "y" },
        .paren_close,
    });

    try testTokenizer("x+-3", enum { @"+-" }, .{
        .{ .ident = "x" },
        .@"+-",
        .{ .integer = 3 },
    });
    try testTokenizer("x+-3", enum { @"+", @"-" }, .{
        .{ .ident = "x" },
        .@"+",
        .@"-",
        .{ .integer = 3 },
    });
}

test "Tokenizer operator edge cases" {
    const ConfusingOpEnum1 = enum { @"+", @"++", @"-", @"--" };
    try testTokenizer("+      ", ConfusingOpEnum1, .{.@"+"});
    try testTokenizer("++     ", ConfusingOpEnum1, .{.@"++"});
    try testTokenizer("+ +    ", ConfusingOpEnum1, .{ .@"+", .@"+" });
    try testTokenizer("++ ++  ", ConfusingOpEnum1, .{ .@"++", .@"++" });
    try testTokenizer("+ ++   ", ConfusingOpEnum1, .{ .@"+", .@"++" });
    try testTokenizer("++ +   ", ConfusingOpEnum1, .{ .@"++", .@"+" });
    try testTokenizer("++++   ", ConfusingOpEnum1, .{ .@"++", .@"++" });
    try testTokenizer("+++++  ", ConfusingOpEnum1, .{ .@"++", .@"++", .@"+" });
    try testTokenizer("++++++ ", ConfusingOpEnum1, .{ .@"++", .@"++", .@"++" });
    try testTokenizer("+++++++", ConfusingOpEnum1, .{ .@"++", .@"++", .@"++", .@"+" });

    try testTokenizer("-      ", ConfusingOpEnum1, .{.@"-"});
    try testTokenizer("--     ", ConfusingOpEnum1, .{.@"--"});
    try testTokenizer("- -    ", ConfusingOpEnum1, .{ .@"-", .@"-" });
    try testTokenizer("-- --  ", ConfusingOpEnum1, .{ .@"--", .@"--" });
    try testTokenizer("- --   ", ConfusingOpEnum1, .{ .@"-", .@"--" });
    try testTokenizer("-- -   ", ConfusingOpEnum1, .{ .@"--", .@"-" });
    try testTokenizer("----   ", ConfusingOpEnum1, .{ .@"--", .@"--" });
    try testTokenizer("-----  ", ConfusingOpEnum1, .{ .@"--", .@"--", .@"-" });
    try testTokenizer("------ ", ConfusingOpEnum1, .{ .@"--", .@"--", .@"--" });
    try testTokenizer("-------", ConfusingOpEnum1, .{ .@"--", .@"--", .@"--", .@"-" });

    const ConfusingOpEnum2 = enum { @"#", @"$", @"#$", @"$#" };
    try testTokenizer("#      ", ConfusingOpEnum2, .{.@"#"});
    try testTokenizer("##     ", ConfusingOpEnum2, .{ .@"#", .@"#" });
    try testTokenizer("$      ", ConfusingOpEnum2, .{.@"$"});
    try testTokenizer("$$     ", ConfusingOpEnum2, .{ .@"$", .@"$" });

    try testTokenizer("#$     ", ConfusingOpEnum2, .{.@"#$"});
    try testTokenizer("$#     ", ConfusingOpEnum2, .{.@"$#"});

    try testTokenizer("#$#    ", ConfusingOpEnum2, .{ .@"#$", .@"#" });
    try testTokenizer("$#$    ", ConfusingOpEnum2, .{ .@"$#", .@"$" });

    try testTokenizer("#$#$   ", ConfusingOpEnum2, .{ .@"#$", .@"#$" });
    try testTokenizer("$#$#   ", ConfusingOpEnum2, .{ .@"$#", .@"$#" });

    try testTokenizer("#$$#   ", ConfusingOpEnum2, .{ .@"#$", .@"$#" });
    try testTokenizer("$##$   ", ConfusingOpEnum2, .{ .@"$#", .@"#$" });
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
