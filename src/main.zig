const std = @import("std");
const assert = std.debug.assert;

const Token = union(enum) {
    ident: []const u8,
    integer: comptime_int,
    char: comptime_int,
    float: []const u8,
    op: Operator,

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

pub const Operator = enum {
    @".",

    @"^",
    @"|",
    @"&",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",

    @"++",
    @"**",

    @"!",
    @"==",
    @"!=",
    @"<",
    @">",
    @"<=",
    @">=",
};

const Tokenizer = struct {
    index: comptime_int = 0,

    inline fn next(comptime state: *Tokenizer, comptime buffer: []const u8) ?Token {
        comptime return state.nextImpl(buffer);
    }

    fn nextImpl(comptime tokenizer: *Tokenizer, comptime buffer: []const u8) ?Token {
        while (tokenizer.index < buffer.len) switch (buffer[tokenizer.index]) {
            ' ', '\t', '\n', '\r' => {
                tokenizer.index += 1;
                continue;
            },
            '(' => {
                tokenizer.index += 1;
                return .paren_open;
            },
            ')' => {
                tokenizer.index += 1;
                return .paren_close;
            },
            '[' => {
                tokenizer.index += 1;
                return .bracket_open;
            },
            ']' => {
                tokenizer.index += 1;
                return .bracket_close;
            },
            ',' => {
                tokenizer.index += 1;
                return .comma;
            },
            'a'...'z',
            'A'...'Z',
            '_',
            => {
                const start = tokenizer.index;
                const end = indexOfNonePosComptime(u8, buffer, tokenizer.index + 1, //
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ //
                    "abcdefghijklmnopqrstuvwxyz" ++ //
                    "0123456789" ++ "_" //
                ) orelse buffer.len;

                const ident = buffer[start..end];
                tokenizer.index += ident.len;

                return Token{ .ident = ident };
            },
            '0'...'9', '\'' => {
                @setEvalBranchQuota(@min(std.math.maxInt(u32), buffer.len - tokenizer.index));
                var zig_tokenizer = std.zig.Tokenizer.init(buffer ++ &[_:0]u8{});
                zig_tokenizer.index = tokenizer.index;
                const tok = zig_tokenizer.next();

                const literal_src = buffer[tok.loc.start..tok.loc.end];
                assert(literal_src.len != 0);
                tokenizer.index += literal_src.len;

                const literal_tok: Token = switch (tok.tag) {
                    .char_literal => Token{ .char = parseCharLiteral(literal_src[0..].*) },
                    .number_literal => switch (std.zig.parseNumberLiteral(literal_src)) {
                        .int => |int| Token{ .integer = int },
                        .big_int => |base| Token{ .integer = blk: {
                            const digits = literal_src[if (base == .decimal) 0 else 2..].*;
                            break :blk parseComptimeIntDigits(@intFromEnum(base), digits) catch |err| @compileError(
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
                return literal_tok;
            },
            else => |c| {
                if (!containsComptime(u8, enumTagNameCharSet(Operator), c)) @compileError(
                    std.fmt.comptimePrint("Unexpected character '{'}'", .{std.zig.fmtEscapes(&.{c})}),
                );
                const start = tokenizer.index;
                @setEvalBranchQuota(@min(std.math.maxInt(u32), (buffer.len - tokenizer.index) * 100));
                const end = indexOfNonePosComptime(
                    u8,
                    buffer,
                    start,
                    enumTagNameCharSet(Operator),
                ) orelse buffer.len;
                const str = buffer[start..end];
                tokenizer.index += str.len;
                const op = std.meta.stringToEnum(Operator, str) orelse
                    @compileError("Unexpected token '" ++ str ++ "'");
                return Token{ .op = op };
            },
        };
        return null;
    }
};

fn testTokenizer(comptime buffer: []const u8, comptime expected: []const ?Token) !void {
    comptime var tokenizer = Tokenizer{};
    inline for (expected) |tok| {
        try std.testing.expectEqualDeep(tok, tokenizer.next(buffer));
    }
}

test Tokenizer {
    try testTokenizer(
        \\3.0   +    3     -
        \\!     (    'a'   )
        \\]     [    /     *
        \\++    **   ==    !=
        \\<     >    <=    >=
        \\^     |    &     a_b_C
    , &.{
        .{ .float = "3.0" },      .{ .op = .@"+" },        .{ .integer = 3 }, .{ .op = .@"-" },
        .{ .op = .@"!" },         .{ .paren_open = {} },   .{ .char = 'a' },  .{ .paren_close = {} },
        .{ .bracket_close = {} }, .{ .bracket_open = {} }, .{ .op = .@"/" },  .{ .op = .@"*" },
        .{ .op = .@"++" },        .{ .op = .@"**" },       .{ .op = .@"==" }, .{ .op = .@"!=" },
        .{ .op = .@"<" },         .{ .op = .@">" },        .{ .op = .@"<=" }, .{ .op = .@">=" },
        .{ .op = .@"^" },         .{ .op = .@"|" },        .{ .op = .@"&" },  .{ .ident = "a_b_C" },
        null,
    });
    try testTokenizer("foo(3, 4.1)", &.{
        .{ .ident = "foo" },
        .{ .paren_open = {} },
        .{ .integer = 3 },
        .{ .comma = {} },
        .{ .float = "4.1" },
        .{ .paren_close = {} },
        null,
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

const containsComptime = struct {
    inline fn containsComptime(
        comptime T: type,
        comptime haystack: []const T,
        comptime needle: T,
    ) bool {
        comptime return containsComptimeImpl(
            T,
            dedupeSlice(T, haystack),
            needle,
        );
    }
    fn containsComptimeImpl(
        comptime T: type,
        comptime haystack: []const T,
        comptime needle: T,
    ) bool {
        comptime {
            const needle_vec = @splat(haystack.len, needle);
            return @reduce(.Or, haystack[0..].* == needle_vec);
        }
    }
}.containsComptime;

const enumTagNameCharSet = struct {
    inline fn enumTagNameCharSet(comptime E: type) *const [enumTagNameCharSetImpl(E).len]u8 {
        comptime return enumTagNameCharSetImpl(E)[0..];
    }
    fn enumTagNameCharSetImpl(comptime E: type) []const u8 {
        comptime {
            var chars: []const u8 = "";
            var set = std.bit_set.IntegerBitSet(std.math.maxInt(u8)).initEmpty();

            for (@typeInfo(E).Enum.fields) |field| {
                for (field.name) |c| {
                    if (set.isSet(c)) continue;
                    set.set(c);
                    chars = chars ++ &[_]u8{c};
                }
            }

            return chars;
        }
    }
}.enumTagNameCharSet;

inline fn indexOfNonePosComptime(
    comptime T: type,
    comptime haystack: []const T,
    comptime start: comptime_int,
    comptime excluded: []const T,
) ?comptime_int {
    const offs = indexOfNoneComptime(T, haystack[start..], excluded) orelse
        return null;
    return start + offs;
}
const indexOfNoneComptime = struct {
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
        const vec: @Vector(arr.len, T) = arr;

        var trues: @Vector(arr.len, bool) = .{true} ** arr.len;
        @setEvalBranchQuota(@min(std.math.maxInt(u32), excluded.len + 1));
        for (excluded) |ex| {
            const ex_vec = @splat(arr.len, ex);
            const prev: @Vector(arr.len, u1) = @bitCast(trues);
            const current: @Vector(arr.len, u1) = @bitCast(ex_vec != vec);
            trues = @bitCast(prev & current);
        }

        return std.simd.firstTrue(trues) orelse return null;
    }
}.indexOfNoneComptime;

const dedupeSlice = struct {
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
