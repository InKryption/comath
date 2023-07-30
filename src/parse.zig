const std = @import("std");
const assert = std.debug.assert;

const util = @import("util.zig");

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
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
    comptime relations: operator.RelationMap(BinOpEnum),
) ExprNode {
    comptime {
        const deduped_expr = util.dedupeSlice(u8, expr);
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
            return std.testing.expectEqualDeep(expected, actual);
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
            return .{ .bin_op = .{
                .lhs = &lhs,
                .op = op,
                .rhs = &rhs,
            } };
        }
        inline fn unOp(comptime op: []const u8, comptime expr: ExprNode) ExprNode {
            return .{ .un_op = .{
                .op = op,
                .val = &expr,
            } };
        }
        inline fn fieldAccess(comptime expr: ExprNode, comptime field: []const u8) ExprNode {
            return .{ .field_access = .{
                .accessed = &expr,
                .accessor = field,
            } };
        }
        inline fn indexAccess(comptime lhs: ExprNode, comptime idx: ExprNode) ExprNode {
            return .{ .index_access = &.{
                .accessed = lhs,
                .accessor = idx,
            } };
        }
    };

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

    try Tester.expectEqual("423_324", helper.int(423_324));
    try Tester.expectEqual("-423_324", helper.unOp("-", helper.int(423_324)));
    try Tester.expectEqual("~-423_324", helper.unOp("~", helper.unOp("-", helper.int(423_324))));
    try Tester.expectEqual("~(-423_324)", helper.unOp("~", helper.group(helper.unOp("-", helper.int(423_324)))));
    try Tester.expectEqual("a.b", helper.fieldAccess(helper.ident("a"), "b"));
    try Tester.expectEqual("a[b]", helper.indexAccess(helper.ident("a"), helper.ident("b")));
    try Tester.expectEqual("!('\u{A0}' + a ^ (3 / y.z))", helper.unOp("!", helper.group(helper.binOp(
        helper.char('\u{A0}'),
        "+",
        helper.binOp(
            helper.ident("a"),
            "^",
            helper.group(helper.binOp(helper.int(3), "/", helper.fieldAccess(helper.ident("y"), "z"))),
        ),
    ))));
    try Tester.expectEqual("3 + -2", helper.binOp(
        helper.int(3),
        "+",
        helper.unOp("-", helper.int(2)),
    ));
    try Tester.expectEqual("(y + 2) * x", helper.binOp(
        helper.group(helper.binOp(helper.ident("y"), "+", helper.int(2))),
        "*",
        helper.ident("x"),
    ));
    try Tester.expectEqual("y + 2 * x", helper.binOp(
        helper.ident("y"),
        "+",
        helper.binOp(helper.int(2), "*", helper.ident("x")),
    ));
    try Tester.expectEqual("2.0 * y ^ 3", helper.binOp(
        helper.float("2.0"),
        "*",
        helper.binOp(helper.ident("y"), "^", helper.int(3)),
    ));
    try Tester.expectEqual("2 ^ 3 ^ 4", helper.binOp(helper.int(2), "^", helper.binOp(helper.int(3), "^", helper.int(4))));
}

const NestType = enum(comptime_int) { none, paren, bracket };
const ParseExprImplInnerUpdate = struct {
    tokenizer: Tokenizer,
    result: ExprNode,
};
fn parseExprImpl(
    comptime expr: []const u8,
    comptime nest_type: NestType,
    comptime tokenizer_init: Tokenizer,
    comptime UnOpEnum: type,
    comptime BinOpEnum: type,
    comptime relations: operator.RelationMap(BinOpEnum),
) ParseExprImplInnerUpdate {
    comptime {
        var result: ExprNode = .null;
        var tokenizer = tokenizer_init;
        while (tokenizer.next(expr, UnOpEnum, BinOpEnum)) |tok| {
            const res_copy = result;
            switch (tok) {
                inline //
                .ident,
                .integer,
                => |val, tag| result = res_copy.concatExpr(@unionInit(ExprNode, @tagName(tag), val)),
                .char => |val| result = res_copy.concatExpr(.{ .char = @enumFromInt(val) }),
                .float => |val| result = res_copy.concatExpr(.{ .float = Number{ .src = val } }),
                .field => |field| result = res_copy.concatFieldAccess(field),
                .bin_op => |op| result = res_copy.concatBinOp(@field(BinOpEnum, op), relations),
                .un_op => |op| result = res_copy.concatUnOp(@tagName(@field(UnOpEnum, op))),

                .paren_open => {
                    const update = parseExprImpl(expr, .paren, tokenizer, UnOpEnum, BinOpEnum, relations);
                    tokenizer = update.tokenizer;
                    result = res_copy.concatExpr(.{ .group = update.result.dedupe() });
                },
                .paren_close => {
                    if (nest_type != .paren) @compileError("Unexpected closing parentheses");
                    break;
                },

                .bracket_open => {
                    const update = parseExprImpl(expr, .bracket, tokenizer, UnOpEnum, BinOpEnum, relations);
                    tokenizer = update.tokenizer;
                    result = .{ .index_access = &.{
                        .accessed = res_copy,
                        .accessor = update.result,
                    } };
                },
                .bracket_close => {
                    if (nest_type != .bracket) @compileError("Unexpected closing bracket");
                    break;
                },
            }
        }
        return .{
            .tokenizer = tokenizer,
            .result = result,
        };
    }
}

pub const ExprNode = union(enum) {
    null,
    ident: []const u8,
    integer: comptime_int,
    char: Char,
    float: Number,
    group: *const ExprNode,
    field_access: FieldAccess,
    index_access: *const IndexAccess,
    un_op: UnOp,
    bin_op: BinOp,

    inline fn concatUnOp(
        comptime base: ExprNode,
        comptime op: []const u8,
    ) ExprNode {
        return switch (base) {
            .null => ExprNode{ .un_op = .{
                .op = util.dedupeSlice(u8, op),
                .val = ExprNode.dedupe(.null),
            } },
            .un_op => |un| un.concatOp(op).*,
            .bin_op => |bin| ExprNode{ .bin_op = .{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatUnOp(op).dedupe(),
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
        comptime op: anytype,
        comptime precedence: operator.RelationMap(@TypeOf(op)),
    ) ExprNode {
        return switch (base) {
            .null => @compileError("Unexpected token '" ++ @tagName(op) ++ "'"),

            .ident,
            .field_access,
            .index_access,
            .integer,
            .char,
            .float,
            .group,
            => .{ .bin_op = .{
                .lhs = base.dedupe(),
                .op = util.dedupeSlice(u8, @tagName(op)),
                .rhs = ExprNode.dedupe(.null),
            } },

            .un_op => |un| switch (un.val.*) {
                .null => @compileError("Unexpected token '" ++ @tagName(op) ++ "'"),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => .{ .bin_op = .{
                    .lhs = base.dedupe(),
                    .op = op,
                    .rhs = ExprNode.dedupe(.null),
                } },

                .bin_op => unreachable,
                .un_op => unreachable,
            },

            .bin_op => |bin| switch (bin.rhs.*) {
                .null => @compileError("Unexpected token '" ++ @tagName(op) ++ "'"),

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => blk: {
                    const old_rel: operator.Relation = @field(precedence, bin.op);
                    const new_rel: operator.Relation = @field(precedence, @tagName(op));
                    if (old_rel.prec == new_rel.prec and old_rel.assoc != new_rel.assoc) {
                        @compileError(bin.op ++ " cannot be chained with " ++ @tagName(op));
                    }
                    if (old_rel.prec < new_rel.prec or
                        (old_rel.prec == new_rel.prec and old_rel.assoc == .right))
                    {
                        break :blk .{ .bin_op = .{
                            .lhs = bin.lhs,
                            .op = bin.op,
                            .rhs = ExprNode.dedupe(.{ .bin_op = .{
                                .lhs = bin.rhs,
                                .op = util.dedupeSlice(u8, @tagName(op)),
                                .rhs = ExprNode.dedupe(.null),
                            } }),
                        } };
                    }
                    break :blk .{ .bin_op = .{
                        .lhs = base.dedupe(),
                        .op = util.dedupeSlice(u8, @tagName(op)),
                        .rhs = ExprNode.dedupe(.null),
                    } };
                },
                .bin_op, .un_op => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatOp(op).dedupe(),
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

            .bin_op => |bin| switch (bin.rhs.*) {
                .null => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = new.dedupe(),
                } },

                .ident,
                .field_access,
                .index_access,
                .integer,
                .char,
                .float,
                .group,
                => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{new.fmt()})),
                .bin_op,
                .un_op,
                => .{ .bin_op = .{
                    .lhs = bin.lhs,
                    .op = bin.op,
                    .rhs = bin.rhs.concatExpr(new).dedupe(),
                } },
            },
            .un_op => |un| un.insertExprAsInnerTarget(new),
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
            => .{ .field_access = .{
                .accessed = base.dedupe(),
                .accessor = util.dedupeSlice(u8, field),
            } },
            .bin_op => |bin| .{ .bin_op = .{
                .lhs = bin.lhs,
                .op = bin.op,
                .rhs = bin.rhs.concatFieldAccess(field).dedupe(),
            } },
            .un_op => |un| .{ .un_op = .{
                .op = un.op,
                .val = un.val.concatFieldAccess(field),
            } },
        };
    }

    const dedupe = struct {
        inline fn dedupe(comptime expr: ExprNode) *const ExprNode {
            comptime return switch (expr) {
                .null => dedupeImpl(expr),
                .ident => |ident| dedupeImpl(.{ .ident = util.dedupeSlice(u8, ident) }),
                .integer => dedupeImpl(expr),
                .char => dedupeImpl(expr),
                .float => |num| dedupeImpl(.{ .float = Number{ .src = util.dedupeSlice(u8, num.src) } }),
                .group => |group| dedupeImpl(.{ .group = group.dedupe() }),
                .field_access => |fa| dedupeImpl(.{ .field_access = .{
                    .accessed = fa.accessed.dedupe(),
                    .accessor = util.dedupeSlice(u8, fa.accessor),
                } }),
                .index_access => |ia| dedupeImpl(.{ .index_access = .{
                    .accessed = ia.accessed.dedupe(),
                    .accessor = ia.accessor.dedupe(),
                } }),
                .bin_op => |bin| dedupeImpl(.{ .bin_op = .{
                    .lhs = bin.lhs.dedupe(),
                    .op = bin.op,
                    .rhs = bin.rhs.dedupe(),
                } }),
                .un_op => |un| dedupeImpl(.{ .un_op = .{
                    .op = un.op,
                    .val = un.val.dedupe(),
                } }),
            };
        }
        fn dedupeImpl(comptime expr: ExprNode) *const ExprNode {
            return &expr;
        }
    }.dedupe;

    const FieldAccess = struct {
        accessed: *const ExprNode,
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
        accessor: ExprNode,

        inline fn dedupe(comptime ia: IndexAccess) IndexAccess {
            return .{
                .accessed = ia.accessed.dedupe(),
                .accessor = ia.accessor.dedupe(),
            };
        }
    };
    const BinOp = struct {
        lhs: *const ExprNode,
        op: []const u8,
        rhs: *const ExprNode,

        inline fn dedupe(comptime bin: BinOp) BinOp {
            return .{
                .lhs = bin.lhs.dedupe(),
                .op = bin.op,
                .rhs = bin.rhs.dedupe(),
            };
        }
    };
    const UnOp = struct {
        op: []const u8,
        val: *const ExprNode,

        inline fn dedupe(comptime un: UnOp) UnOp {
            return .{
                .op = un.op,
                .val = un.val.dedupe(),
            };
        }

        inline fn insertExprAsInnerTarget(comptime un: UnOp, comptime expr: ExprNode) ExprNode {
            return switch (un.val.*) {
                .null => .{ .un_op = .{
                    .op = un.op,
                    .val = expr.dedupe(),
                } },
                .un_op => |inner| .{ .un_op = .{
                    .op = un.op,
                    .val = inner.insertExprAsInnerTarget(expr).dedupe(),
                } },
                else => @compileError(std.fmt.comptimePrint("Unexpected token '{}'", .{un.val.fmt()})),
            };
        }

        inline fn concatOp(comptime un: UnOp, comptime op: []const u8) *const ExprNode {
            const updated = un.concatOpInnerImpl(op) orelse @compileError("Unexpected token '" ++ op ++ "'");
            return ExprNode.dedupe(.{ .un_op = updated });
        }

        /// returns null if the inner-most target of the unary operations is already present,
        /// meaning the unary operator can simply be used as the LHS of a binary operation
        inline fn concatOpInnerImpl(comptime un: UnOp, comptime op: []const u8) ?UnOp {
            switch (un.val.*) {
                .null => return .{
                    .op = un.op,
                    .val = ExprNode.dedupe(.{ .un_op = .{
                        .op = util.dedupeSlice(u8, op),
                        .val = ExprNode.dedupe(.null),
                    } }),
                },
                .un_op => |inner| return if (inner.concatOpInnerImpl(op)) |updated| .{
                    .op = un.op,
                    .val = ExprNode.dedupe(.{ .un_op = updated }),
                },
                else => return null,
            }
        }
    };

    inline fn fmt(comptime expr: ExprNode) Fmt {
        return .{ .expr = expr };
    }
    const Fmt = struct {
        expr: ExprNode,

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
                .field_access => |field| std.fmt.comptimePrint("{}.{s}", .{ field.accessed.fmt(), field.accessor }),
                .index_access => |index| std.fmt.comptimePrint("{}[{}]", .{ index.accessed.fmt(), index.accessor.fmt() }),
                .integer => |int| std.fmt.comptimePrint("{d}", .{int}),
                .char => |char| std.fmt.comptimePrint("'{u}'", .{@intFromEnum(char)}),
                .float => |num| std.fmt.comptimePrint(num.src),
                .group => |group| std.fmt.comptimePrint("({})", .{group.fmt()}),
                .bin_op => |bin_op| std.fmt.comptimePrint("{} {s} {}", .{ bin_op.lhs.fmt(), @tagName(bin_op.op), bin_op.rhs.fmt() }),
                .un_op => |un_op| std.fmt.comptimePrint("{s}{}", .{ @tagName(un_op.op), un_op.val.fmt() }),
            };
            try writer.writeAll(str);
        }
    };
};
