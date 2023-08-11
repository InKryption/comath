const std = @import("std");
const assert = std.debug.assert;

const util = @import("util");

const Tokenizer = @import("Tokenizer.zig");
const operator = @import("operator.zig");

/// Represents a non-integer value, represented by
/// the source code can be interpreted as needed.
pub const Number = struct {
    src: []const u8,

    pub fn asFloat(comptime number: Number) comptime_float {
        comptime return std.fmt.parseFloat(f128, number.src) catch |err| @compileError(@errorName(err));
    }
};

/// Represents a character literal value, distinguished from `comptime_int`.
pub const Char = enum(comptime_int) {
    _,
};

pub fn parseExpr(
    comptime expr: []const u8,
    comptime UnOpEnum: ?type,
    comptime BinOpEnum: ?type,
    comptime relations: anytype,
) ExprNode {
    comptime {
        var enum_fields: []const std.builtin.Type.EnumField = &.{};
        enum_fields = enum_fields ++ @typeInfo(UnOpEnum orelse enum {}).Enum.fields;
        enum_fields = enum_fields ++ @typeInfo(BinOpEnum orelse enum {}).Enum.fields;
        for (enum_fields) |field| {
            const i = util.indexOfNoneComptime(u8, field.name, Tokenizer.operator_characters) orelse continue;
            const list = list: {
                var str: []const u8 = "";
                for (Tokenizer.operator_characters) |sym|
                    str = str ++ std.fmt.comptimePrint(" * '{c}'\n", .{sym});
                break :list str;
            };
            @compileError(std.fmt.comptimePrint(
                \\Operator cannot contain byte '{c}' (found in operator '{s}').
                \\An operator may contain any combination of the following symbols:
                \\{s}
            , .{ field.name[i], field.name, list }));
        }

        const deduped_expr = util.dedupe.scalarSlice(u8, expr[0..].*);
        const res = parseExprImpl(deduped_expr, .none, .{}, UnOpEnum, BinOpEnum, relations);
        return res.result;
    }
}

fn ParseExprTester(
    comptime UnOp: type,
    comptime BinOp: type,
    comptime relations: operator.RelationMap(BinOp),
) type {
    return struct {
        fn expectEqual(
            comptime expr: []const u8,
            comptime expected: ExprNode,
        ) !void {
            const actual = parseExpr(expr, UnOp, BinOp, relations);
            if (!actual.eql(expected)) {
                @compileError(std.fmt.comptimePrint("Expected `{}`, got `{}`", .{ expected.fmt(.{ .verbose_paren = true }), actual.fmt(.{ .verbose_paren = true }) }));
            }
        }
    };
}

test parseExpr {
    const helper = struct {
        inline fn int(comptime val: comptime_int) ExprNode {
            return .{ .integer = val };
        }
        inline fn float(comptime src: []const u8) ExprNode {
            return .{ .float = Number{ .src = src } };
        }
        inline fn ident(comptime name: []const u8) ExprNode {
            return .{ .ident = name };
        }
        inline fn char(comptime val: comptime_int) ExprNode {
            return .{ .char = @enumFromInt(val) };
        }
        inline fn group(comptime expr: ExprNode) ExprNode {
            return .{ .group = &expr };
        }
        inline fn binOp(comptime lhs: ExprNode, comptime op: []const u8, comptime rhs: ExprNode) ExprNode {
            return .{ .bin_op = &.{
                .lhs = lhs,
                .op = op,
                .rhs = rhs,
            } };
        }
        inline fn unOp(comptime op: []const u8, comptime expr: ExprNode) ExprNode {
            return .{ .un_op = &.{
                .op = op,
                .val = expr,
            } };
        }
        inline fn fieldAccess(comptime expr: ExprNode, comptime field: []const u8) ExprNode {
            return .{ .field_access = &.{
                .accessed = expr,
                .accessor = field,
            } };
        }
        inline fn indexAccess(comptime lhs: ExprNode, comptime idx: []const ExprNode) ExprNode {
            return .{ .index_access = &.{
                .accessed = lhs,
                .accessor = idx,
            } };
        }
        inline fn funcCall(comptime callee: ExprNode, comptime args: []const ExprNode) ExprNode {
            return .{ .func_call = &.{
                .callee = callee,
                .args = args,
            } };
        }
    };
    const int = helper.int;
    const float = helper.float;
    const ident = helper.ident;
    const char = helper.char;
    const group = helper.group;
    const binOp = helper.binOp;
    const unOp = helper.unOp;
    const fieldAccess = helper.fieldAccess;
    const indexAccess = helper.indexAccess;
    const funcCall = helper.funcCall;
    const Tester = ParseExprTester(
        enum { @"-", @"~", @"!" },
        enum { @"-", @"+", @"*", @"/", @"^" },
        .{
            .@"-" = operator.relation(.left, 0),
            .@"+" = operator.relation(.left, 0),
            .@"*" = operator.relation(.left, 1),
            .@"/" = operator.relation(.left, 1),
            .@"^" = operator.relation(.right, 2),
        },
    );

    try Tester.expectEqual("423_324", int(423_324));
    try Tester.expectEqual("-423_324", unOp("-", int(423_324)));
    try Tester.expectEqual("~-423_324", unOp("~", unOp("-", int(423_324))));
    try Tester.expectEqual("~(-423_324)", unOp("~", group(unOp("-", int(423_324)))));
    try Tester.expectEqual("!('\u{A0}' + a ^ (3 / y.z))", unOp("!", group(binOp(
        char('\u{A0}'),
        "+",
        binOp(
            ident("a"),
            "^",
            group(binOp(int(3), "/", fieldAccess(ident("y"), "z"))),
        ),
    ))));
    try Tester.expectEqual("3 + -2", binOp(
        int(3),
        "+",
        unOp("-", int(2)),
    ));
    try Tester.expectEqual("(y + 2) * x", binOp(
        group(binOp(ident("y"), "+", int(2))),
        "*",
        ident("x"),
    ));
    try Tester.expectEqual("y + 2 * x", binOp(
        ident("y"),
        "+",
        binOp(int(2), "*", ident("x")),
    ));
    try Tester.expectEqual("2.0 * y ^ 3", binOp(
        float("2.0"),
        "*",
        binOp(ident("y"), "^", int(3)),
    ));
    try Tester.expectEqual("2 ^ 3 ^ 4", binOp(int(2), "^", binOp(int(3), "^", int(4))));

    try Tester.expectEqual("a.b", fieldAccess(ident("a"), "b"));
    try Tester.expectEqual("a + b.c", binOp(ident("a"), "+", fieldAccess(ident("b"), "c")));
    try Tester.expectEqual("(a + b).c", fieldAccess(group(binOp(ident("a"), "+", ident("b"))), "c"));
    try Tester.expectEqual("foo.b@r", fieldAccess(ident("foo"), "b@r"));
    try Tester.expectEqual("foo.?", fieldAccess(ident("foo"), "?"));
    try Tester.expectEqual("foo.0", fieldAccess(ident("foo"), "0"));

    try Tester.expectEqual("a[b]", indexAccess(ident("a"), &.{ident("b")}));
    try Tester.expectEqual("(a)[(b)]", indexAccess(group(ident("a")), &.{group(ident("b"))}));
    try Tester.expectEqual("(~(a + b))[(c)]", indexAccess(
        group(unOp("~", group(binOp(ident("a"), "+", ident("b"))))),
        &.{group(ident("c"))},
    ));

    try Tester.expectEqual("foo()", funcCall(ident("foo"), &.{}));
    try Tester.expectEqual("foo(bar)", funcCall(ident("foo"), &.{ident("bar")}));
    try Tester.expectEqual("foo(bar,)", funcCall(ident("foo"), &.{ident("bar")}));
    try Tester.expectEqual("foo(bar,baz)", funcCall(ident("foo"), &.{ ident("bar"), ident("baz") }));
    try Tester.expectEqual("foo(bar, baz, )", funcCall(ident("foo"), &.{ ident("bar"), ident("baz") }));

    try Tester.expectEqual("foo[]", indexAccess(ident("foo"), &.{}));
    try Tester.expectEqual("foo[bar]", indexAccess(ident("foo"), &.{ident("bar")}));
    try Tester.expectEqual("foo[bar,]", indexAccess(ident("foo"), &.{ident("bar")}));
    try Tester.expectEqual("foo[bar,baz]", indexAccess(ident("foo"), &.{ ident("bar"), ident("baz") }));
    try Tester.expectEqual("foo[bar, baz, ]", indexAccess(ident("foo"), &.{ ident("bar"), ident("baz") }));
}

const NestType = enum(comptime_int) { none, paren, bracket };
const ParseExprImplInnerUpdate = struct {
    ends_with_comma: bool,
    tokenizer: Tokenizer,
    result: ExprNode,
};
fn parseExprImpl(
    comptime expr: []const u8,
    comptime nest_type: NestType,
    comptime tokenizer_init: Tokenizer,
    comptime UnOpEnum: ?type,
    comptime BinOpEnum: ?type,
    comptime relations: anytype,
) ParseExprImplInnerUpdate {
    comptime {
        var result: ExprNode = .null;
        var tokenizer = tokenizer_init;
        var ends_with_comma = false;

        @setEvalBranchQuota(expr.len + expr.len / 2);
        var can_be_unary = true;
        while (true) switch (tokenizer.next(expr)) {
            .eof => break,
            inline //
            .ident,
            .integer,
            => |val, tag| {
                can_be_unary = false;
                result = result.concatExpr(@unionInit(ExprNode, @tagName(tag), val));
            },
            .char => |val| {
                can_be_unary = false;
                result = result.concatExpr(.{ .char = @enumFromInt(val) });
            },
            .float => |val| {
                can_be_unary = false;
                result = result.concatExpr(.{ .float = Number{ .src = val } });
            },
            .field => |field| {
                can_be_unary = false;
                result = result.concatFieldAccess(field);
            },
            .op_symbols => |op_symbols| {
                var start = 0;
                var len = op_symbols.len;
                while (start < op_symbols.len) {
                    if (len == 0) {
                        @compileError("Unexpected operator symbols '" ++ op_symbols[start..] ++ "'");
                    }
                    const OpEnum = blk: {
                        if (can_be_unary) break :blk UnOpEnum orelse @compileError(
                            "No `UnOp` enum was given to parse the expected unary operator(s) in '" ++ op_symbols[start..],
                        );
                        break :blk BinOpEnum orelse @compileError(
                            "No `BinOp` enum was given to parse the expected binary operator in '" ++ op_symbols[start..],
                        );
                    };
                    const op = op_symbols[start..][0..len];
                    if (!@hasField(OpEnum, op)) {
                        len -= 1;
                        continue;
                    }
                    start += len;
                    len = op_symbols.len - start;
                    if (!can_be_unary) {
                        can_be_unary = true;
                        result = result.concatBinOp(op, relations);
                    } else {
                        result = result.concatUnOp(op);
                    }
                }
            },
            inline .paren_open, .bracket_open => |_, tag| {
                const inner_nest_type = switch (tag) {
                    .paren_open => .paren,
                    .bracket_open => .bracket,
                    else => unreachable,
                };

                can_be_unary = false;
                var args: []const ExprNode = &.{};
                while (true) {
                    const update = parseExprImpl(expr, inner_nest_type, tokenizer, UnOpEnum, BinOpEnum, relations);
                    tokenizer = update.tokenizer;
                    if (update.result != .null) {
                        args = args ++ &[_]ExprNode{update.result};
                    }
                    if (!update.ends_with_comma) break;
                }
                result = result.concatFunctionArgsOrJustGroup(inner_nest_type, util.dedupe.scalarSlice(ExprNode, args[0..].*));
            },
            .comma => {
                can_be_unary = undefined;
                ends_with_comma = true;
                break;
            },
            .paren_close => {
                can_be_unary = undefined;
                if (nest_type != .paren) @compileError("Unexpected closing parentheses");
                break;
            },
            .bracket_close => {
                can_be_unary = undefined;
                if (nest_type != .bracket) @compileError("Unexpected closing bracket");
                break;
            },
        };
        return .{
            .ends_with_comma = ends_with_comma,
            .tokenizer = tokenizer,
            .result = result,
        };
    }
}

fn parseOpEnumFromStartOf(comptime OpEnum: type, comptime buffer: []const u8) []const u8 {
    comptime {
        var end = buffer.len;
        while (end != 0 and !@hasField(OpEnum, buffer[0..end])) : (end -= 1) {}
        if (end != 0) return util.dedupe.scalarSlice(u8, buffer[0..end].*);
        @compileError("Unrecognized operator '" ++ buffer ++ "'");
    }
}

pub const ExprNode = union(enum) {
    null,
    ident: []const u8,
    integer: comptime_int,
    char: Char,
    float: Number,
    group: *const ExprNode,
    field_access: *const FieldAccess,
    index_access: *const IndexAccess,
    func_call: *const FuncCall,
    un_op: *const UnOp,
    bin_op: *const BinOp,

    pub inline fn eql(comptime a: ExprNode, comptime b: ExprNode) bool {
        const tag_a: @typeInfo(ExprNode).Union.tag_type.? = a;
        const tag_b: @typeInfo(ExprNode).Union.tag_type.? = b;
        comptime if (tag_a != tag_b) return false;
        const val_a = @field(a, @tagName(tag_a));
        const val_b = @field(b, @tagName(tag_b));
        comptime return switch (tag_a) {
            .null => true,
            .ident => util.eqlComptime(u8, val_a, val_b),
            .integer => val_a == val_b,
            .char => val_a == val_b,
            .float => util.eqlComptime(u8, val_a.src, val_b.src),
            .group => val_a.eql(val_b.*),
            .field_access => val_a.accessed.eql(val_b.accessed) and util.eqlComptime(u8, val_a.accessor, val_b.accessor),
            .index_access => val_a.accessed.eql(val_b.accessed) and
                val_a.accessor.len == val_b.accessor.len and blk: {
                break :blk for (
                    val_a.accessor,
                    val_b.accessor,
                ) |accessor_a, accessor_b| {
                    if (!accessor_a.eql(accessor_b)) break false;
                } else true;
            },
            .func_call => val_a.callee.eql(val_b.callee) and
                val_a.args.len == val_b.args.len and blk: {
                break :blk for (
                    val_a.args,
                    val_b.args,
                ) |arg_a, arg_b| {
                    if (!arg_a.eql(arg_b)) break false;
                } else true;
            },
            .un_op => util.eqlComptime(u8, val_a.op, val_b.op) and val_a.val.eql(val_b.val),
            .bin_op => util.eqlComptime(u8, val_a.op, val_b.op) and val_a.lhs.eql(val_b.lhs),
        };
    }

    inline fn concatUnOp(
        comptime base: ExprNode,
        comptime op: []const u8,
    ) ExprNode {
        return switch (base) {
            .null => ExprNode{ .un_op = &.{
                .op = util.dedupe.scalarSlice(u8, op[0..].*),
                .val = .null,
            } },
            .func_call => @compileError("TODO: handle"),
            .un_op => |un| un.concatOp(op),
            .bin_op => |bin| ExprNode{ .bin_op = &.{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatUnOp(op),
            } },

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => @compileError("Unexpected token '" ++ op ++ "'"),
        };
    }

    inline fn concatBinOp(
        comptime base: ExprNode,
        comptime op: []const u8,
        comptime relations: anytype,
    ) ExprNode {
        return switch (base) {
            .null => @compileError("Unexpected token '" ++ op ++ "'"),

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            .func_call,
            => .{ .bin_op = &.{
                .lhs = base,
                .op = util.dedupe.scalarSlice(u8, op[0..].*),
                .rhs = .null,
            } },

            .un_op => |un| switch (un.val) {
                .null => @compileError("Unexpected token '" ++ op ++ "'"),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                .func_call,
                => .{ .bin_op = &.{
                    .lhs = base,
                    .op = util.dedupe.scalarSlice(u8, op[0..].*),
                    .rhs = .null,
                } },

                .bin_op => unreachable,
                .un_op => unreachable,
            },

            .bin_op => |bin| switch (bin.rhs) {
                .null => @compileError("Unexpected token '" ++ op ++ "'"),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => blk: {
                    const relation_map = switch (@typeInfo(@TypeOf(relations))) {
                        .Optional => relations orelse .{},
                        .Null => .{},
                        else => relations,
                    };
                    const lhs_rel: operator.Relation = @as(?operator.Relation, @field(relation_map, bin.op)) orelse @compileError("Missing relationship definition for operator '" ++ bin.op ++ "'");
                    const rhs_rel: operator.Relation = @as(?operator.Relation, @field(relation_map, op)) orelse @compileError("Missing relationship definition for operator '" ++ op ++ "'");

                    if (lhs_rel.prec == rhs_rel.prec and lhs_rel.assoc != rhs_rel.assoc) {
                        @compileError(bin.op ++ " cannot be chained with " ++ op);
                    }
                    if (lhs_rel.prec < rhs_rel.prec or
                        (lhs_rel.prec == rhs_rel.prec and lhs_rel.assoc == .right))
                    {
                        break :blk .{ .bin_op = &.{
                            .lhs = bin.lhs,
                            .op = bin.op,
                            .rhs = .{ .bin_op = &.{
                                .lhs = bin.rhs,
                                .op = util.dedupe.scalarSlice(u8, op[0..].*),
                                .rhs = .null,
                            } },
                        } };
                    }
                    break :blk .{ .bin_op = &.{
                        .lhs = base,
                        .op = util.dedupe.scalarSlice(u8, op[0..].*),
                        .rhs = .null,
                    } };
                },
                .func_call => @compileError("TODO: handle"),
                .bin_op, .un_op => .{ .bin_op = &.{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatBinOp(op, relations),
                } },
            },
        };
    }
    inline fn concatExpr(comptime base: ExprNode, comptime new: ExprNode) ExprNode {
        return switch (base) {
            .null => new,

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),

            .func_call => |fc| .{ .func_call = &.{
                .callee = fc.callee,
                .args = fc.args ++ if (new.group.* == .null) &.{} else &[_]ExprNode{new.group.*},
            } },

            .bin_op => |bin| switch (bin.rhs) {
                .null => .{ .bin_op = &.{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = new,
                } },

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),
                .func_call => @compileError("TODO: handle"),
                .bin_op,
                .un_op,
                => .{ .bin_op = &.{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatExpr(new),
                } },
            },
            .un_op => |un| un.insertExprAsInnerTarget(new),
        };
    }
    inline fn concatFunctionArgsOrJustGroup(comptime base: ExprNode, comptime delimiter: enum { paren, bracket }, comptime args: []const ExprNode) ExprNode {
        return switch (base) {
            .null => .{ .group = if (args.len != 1 or delimiter != .paren) @compileError("Group must be comprised of exactly 1 expression and be delimited by parentheses") else &args[0] },
            .ident,
            .field_access,
            .integer,
            .char,
            .float,
            .group,
            .index_access,
            .func_call,
            => switch (delimiter) {
                .paren => .{ .func_call = &.{
                    .callee = base,
                    .args = args,
                } },
                .bracket => .{ .index_access = &.{
                    .accessed = base,
                    .accessor = args,
                } },
            },
            .bin_op => |bin| .{ .bin_op = &.{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatFunctionArgsOrJustGroup(delimiter, args),
            } },
            .un_op => |un| .{ .un_op = &.{
                .op = un.op,
                .val = un.val.concatFunctionArgsOrJustGroup(delimiter, args),
            } },
        };
    }
    inline fn concatFieldAccess(comptime base: ExprNode, comptime field: []const u8) ExprNode {
        return switch (base) {
            .null => @compileError("Unexpected token '." ++ field ++ "'"),
            .integer,
            .char,
            .float,
            .ident,
            .field_access,
            .index_access,
            .group,
            => .{ .field_access = &.{
                .accessed = base,
                .accessor = util.dedupe.scalarSlice(u8, field[0..].*),
            } },
            .func_call => @compileError("TODO: handle"),
            .bin_op => |bin| .{ .bin_op = &.{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatFieldAccess(field),
            } },
            .un_op => |un| .{ .un_op = .{
                .op = un.op,
                .val = un.val.concatFieldAccess(field),
            } },
        };
    }

    const FieldAccess = struct {
        accessed: ExprNode,
        accessor: []const u8,

        inline fn dedupe(comptime fa: FieldAccess) FieldAccess {
            return .{
                .accessed = fa.accessed.dedupe(),
                .accessor = util.dedupeSlice(u8, fa.accessor),
            };
        }
    };
    const IndexAccess = struct {
        accessed: ExprNode,
        accessor: []const ExprNode,

        inline fn dedupe(comptime ia: IndexAccess) IndexAccess {
            return .{
                .accessed = ia.accessed.dedupe(),
                .accessor = ia.accessor.dedupe(),
            };
        }
    };
    pub const FuncCall = struct {
        callee: ExprNode,
        args: []const ExprNode,
    };
    const BinOp = struct {
        lhs: ExprNode,
        op: []const u8,
        rhs: ExprNode,
    };
    const UnOp = struct {
        op: []const u8,
        val: ExprNode,

        inline fn insertExprAsInnerTarget(comptime un: UnOp, comptime expr: ExprNode) ExprNode {
            return switch (un.val) {
                .null => .{ .un_op = &.{
                    .op = un.op,
                    .val = expr,
                } },
                .un_op => |inner| .{ .un_op = &.{
                    .op = un.op,
                    .val = inner.insertExprAsInnerTarget(expr),
                } },
                else => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{un.val.fmt()})),
            };
        }

        inline fn concatOp(comptime un: UnOp, comptime op: []const u8) ExprNode {
            const updated = un.concatOpInnerImpl(op) orelse @compileError("Unexpected token '" ++ op ++ "'");
            return .{ .un_op = &updated };
        }

        /// returns null if the inner-most target of the unary operations is already present,
        /// meaning the unary operator can simply be used as the LHS of a binary operation
        inline fn concatOpInnerImpl(comptime un: UnOp, comptime op: []const u8) ?UnOp {
            switch (un.val) {
                .null => return .{
                    .op = un.op,
                    .val = .{ .un_op = &.{
                        .op = util.dedupe.scalarSlice(u8, op[0..].*),
                        .val = .null,
                    } },
                },
                .un_op => |inner| return if (inner.concatOpInnerImpl(op)) |updated| .{
                    .op = un.op,
                    .val = .{ .un_op = &updated },
                },
                else => return null,
            }
        }
    };

    inline fn fmt(
        comptime expr: ExprNode,
        comptime config: Fmt.Config,
    ) Fmt {
        return .{ .expr = expr, .config = config };
    }
    const Fmt = struct {
        expr: ExprNode,
        config: Config,

        const Config = struct {
            verbose_paren: bool = false,
        };

        pub fn format(
            comptime formatter: Fmt,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            _ = options;
            if (fmt_str.len != 0) std.fmt.invalidFmtError(fmt_str, formatter);
            const str = switch (formatter.expr) {
                .null => "null",
                .ident => |ident| ident,
                .field_access => |field| std.fmt.comptimePrint("{}.{s}", .{ field.accessed.fmt(formatter.config), field.accessor }),
                .index_access => |index| std.fmt.comptimePrint("{}[{}]", .{ index.accessed.fmt(formatter.config), index.accessor.fmt(formatter.config) }),
                .func_call => |fc| std.fmt.comptimePrint("{}", .{fc.callee}) ++ args: {
                    var args: []const u8 = "";
                    args = args ++ "(";
                    if (fc.args.len != 0) {
                        args = args ++ std.fmt.comptimePrint("{}", .{fc.args[0].fmt(formatter.config)});
                    }
                    for (fc.args[1..]) |arg| {
                        args = args ++ std.fmt.comptimePrint(", {}", .{arg.fmt(formatter.config)});
                    }
                    args = args ++ ")";
                    break :args args;
                },
                .integer => |int| std.fmt.comptimePrint("{d}", .{int}),
                .char => |char| std.fmt.comptimePrint("'{u}'", .{@intFromEnum(char)}),
                .float => |num| std.fmt.comptimePrint(num.src),
                .group => |group| std.fmt.comptimePrint("({})", .{group.fmt(formatter.config)}),
                .bin_op => |bin_op| std.fmt.comptimePrint("{} {s} {}", .{ bin_op.lhs.fmt(formatter.config), bin_op.op, bin_op.rhs.fmt(formatter.config) }),
                .un_op => |un_op| std.fmt.comptimePrint("{s}{}", .{ un_op.op, un_op.val.fmt(formatter.config) }),
            };
            if (formatter.config.verbose_paren) switch (formatter.expr) {
                .bin_op, .un_op => {
                    try writer.writeAll("(" ++ str ++ ")");
                    return;
                },
                else => {},
            };
            try writer.writeAll(str);
        }
    };
};
