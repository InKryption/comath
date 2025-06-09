const comath = @import("../main.zig");
const std = @import("std");
const util = @import("util");

pub inline fn fnMethod(
    sub_ctx: anytype,
    /// must be a struct literal wherein each field name is an operator,
    /// with a string literal value corresponding to the method name, or a list of possible method names.
    /// ie `.{ .@"+" = "add", .@"-" = &.{ "sub", "neg" } }`
    comptime method_names: anytype,
) FnMethod(@TypeOf(sub_ctx), method_names) {
    const SubCtx = @TypeOf(sub_ctx);
    if (util.NamespaceOf(SubCtx) == null) @compileError(
        "Expected struct/union/enum, or pointer to struct/union/enum/opaque, got '" ++ @typeName(SubCtx) ++ "'" ++
            if (SubCtx != type) "" else " (" ++ @typeName(sub_ctx) ++ ")",
    );
    return .{ .sub_ctx = sub_ctx };
}

pub fn FnMethod(
    comptime SubCtx: type,
    comptime method_names: anytype,
) type {
    {
        const Deduped = DedupedMethodNames(method_names);
        if (@TypeOf(method_names) != Deduped) {
            return FnMethod(SubCtx, Deduped{});
        }
    }
    return struct {
        sub_ctx: SubCtx,
        const Self = @This();
        const Ns = util.NamespaceOf(SubCtx) orelse struct {};

        pub inline fn matchUnOp(comptime str: []const u8) bool {
            return @hasDecl(Ns, "matchUnOp") and Ns.matchUnOp(str);
        }

        pub inline fn matchBinOp(comptime str: []const u8) bool {
            return @hasDecl(Ns, "matchBinOp") and Ns.matchBinOp(str);
        }

        pub inline fn orderBinOp(comptime lhs: []const u8, comptime rhs: []const u8) ?comath.Order {
            return Ns.orderBinOp(lhs, rhs);
        }

        pub fn EvalNumberLiteral(comptime src: []const u8) type {
            if (@hasDecl(Ns, "EvalNumberLiteral")) {
                return Ns.EvalNumberLiteral(src);
            }
            return comath.ctx.DefaultEvalNumberLiteral(src);
        }
        pub inline fn evalNumberLiteral(comptime src: []const u8) EvalNumberLiteral(src) {
            if (@hasDecl(Ns, "evalNumberLiteral")) {
                return Ns.evalNumberLiteral(src);
            }
            return comath.ctx.defaultEvalNumberLiteral(src);
        }

        pub fn EvalIdent(comptime ident: []const u8) type {
            if (@hasDecl(Ns, "EvalIdent")) {
                return Ns.EvalIdent(ident);
            }
            return noreturn;
        }
        pub fn evalIdent(ctx: Self, comptime ident: []const u8) !EvalIdent(ident) {
            if (@hasDecl(Ns, "EvalIdent")) {
                return ctx.sub_ctx.evalIdent(ident);
            }
            comptime unreachable;
        }

        pub fn EvalProperty(comptime T: type, comptime field: []const u8) type {
            if (getOpMapping(T, "evalProperty", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).@"fn";
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const val: (method_info.params[0].type orelse T) = undefined;
                const field_arg = if (method_info.params[1].type) |FieldArg| castStringTo(FieldArg, field) else field;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(T, name)(val, field_arg)));
            }
            return Ns.EvalProperty(T, field);
        }
        pub inline fn evalProperty(ctx: Self, val: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(val), field) {
            const T = @TypeOf(val);
            if (getOpMapping(@TypeOf(val), "evalProperty", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).@"fn";

                const field_arg = if (method_info.params[1].type) |FieldArg| castStringTo(FieldArg, field) else field;
                return @field(T, name)(val, field_arg);
            }
            return ctx.sub_ctx.evalProperty(val, field);
        }

        pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {
            if (getOpMapping(Lhs, "evalIndexAccess", method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Rhs, name)));
                const method_info = @typeInfo(Method).@"fn";
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Lhs) = undefined;
                const rhs: (method_info.params[0].type orelse Rhs) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(Lhs, name)(lhs, rhs)));
            }
            return Ns.EvalIndexAccess(Lhs, Rhs);
        }
        pub inline fn evalIndexAccess(ctx: Self, lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            if (getOpMapping(Lhs, "evalIndexAccess", method_names, 2)) |name| {
                return @field(Lhs, name)(lhs, rhs);
            }
            return ctx.sub_ctx.evalIndexAccess(lhs, rhs);
        }

        pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {
            const args_info = @typeInfo(Args).@"struct";
            if (getOpMapping(Callee, "evalFuncCall", method_names, 1 + args_info.fields.len)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Callee, name)));
                const method_info = @typeInfo(Method).@"fn";
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Callee) = undefined;
                const rhs: (method_info.params[0].type orelse Args) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@call(.auto, @field(Callee, name), .{lhs} ++ rhs)));
            }
            return Ns.EvalFuncCall(Callee, Args);
        }
        pub inline fn evalFuncCall(ctx: Self, callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {
            const Callee = @TypeOf(callee);
            const Args = @TypeOf(args);
            const args_info = @typeInfo(Args).@"struct";
            if (getOpMapping(Callee, "evalFuncCall", method_names, 1 + args_info.fields.len)) |name| {
                return @call(.auto, @field(Callee, name), .{callee} ++ args);
            }
            return ctx.sub_ctx.evalFuncCall(callee, args);
        }

        pub fn EvalUnOp(comptime op: []const u8, comptime T: type) type {
            if (getOpMapping(T, op, method_names, 1)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(T, name)));
                const method_info = @typeInfo(Method).@"fn";
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const val: (method_info.params[0].type orelse T) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(T, name)(val)));
            }
            return Ns.EvalUnOp(op, T);
        }
        pub inline fn evalUnOp(ctx: Self, comptime op: []const u8, val: anytype) !EvalUnOp(op, @TypeOf(val)) {
            const Val = @TypeOf(val);
            if (comptime getOpMapping(Val, op, method_names, 1)) |name| {
                return @field(Val, name)(val);
            }
            return ctx.sub_ctx.evalUnOp(op, val);
        }

        pub fn EvalBinOp(comptime Lhs: type, comptime op: []const u8, comptime Rhs: type) type {
            if (comptime getOpMapping(Lhs, op, method_names, 2)) |name| {
                const Method = util.ImplicitDeref(@TypeOf(@field(Lhs, name)));
                const method_info = @typeInfo(Method).@"fn";
                if (method_info.return_type) |Ret| return util.GetPayloadIfErrorUnion(Ret);

                const lhs: (method_info.params[0].type orelse Lhs) = undefined;
                const rhs: (method_info.params[1].type orelse Rhs) = undefined;
                return util.GetPayloadIfErrorUnion(@TypeOf(@field(Lhs, name)(lhs, rhs)));
            }
            return Ns.EvalBinOp(Lhs, op, Rhs);
        }
        pub inline fn evalBinOp(ctx: Self, lhs: anytype, comptime op: []const u8, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {
            const Lhs = @TypeOf(lhs);
            if (comptime getOpMapping(Lhs, op, method_names, 2)) |name| {
                return @field(Lhs, name)(lhs, rhs);
            }
            return ctx.sub_ctx.evalBinOp(lhs, op, rhs);
        }

        pub fn EvalMethodCall(comptime lhs: type, comptime method: []const u8, comptime Args: type) type {
            if (@hasDecl(Ns, "EvalMethodCall")) {
                if (Ns.EvalMethodCall(lhs, method, Args) != noreturn) {
                    return Ns.EvalMethodCall(lhs, method, Args);
                }
            }
            return noreturn;
        }
        pub fn evalMethodCall(ctx: @This(), self_param: anytype, comptime method: []const u8, args: anytype) !EvalMethodCall(@TypeOf(self_param), method, @TypeOf(args)) {
            const func = @field(util.ImplicitDeref(@TypeOf(self_param)), method);
            return ctx.evalFuncCall(func, .{self_param} ++ args);
        }

    };
}

inline fn castStringTo(comptime T: type, comptime str: []const u8) T {
    comptime switch (@typeInfo(T)) {
        .@"enum" => return @field(T, str),
        .array => return str[0..].*,
        .pointer => |pointer| ptr: {
            var sentinel: ?u8 = null;
            switch (pointer.size) {
                .Slice, .Many => {
                    if (pointer.child != u8) break :ptr;
                    if (pointer.sentinel) |maybe_ptr| blk: {
                        const ptr = maybe_ptr orelse break :blk;
                        sentinel = @as(*align(1) const pointer.child, @ptrCast(ptr)).*;
                    }
                },
                .One => switch (@typeInfo(pointer.child)) {
                    .@"enum" => return &@field(pointer.child, str),
                    .array => |array| {
                        if (array.child != u8) break :ptr;
                        if (array.sentinel) |maybe_ptr| blk: {
                            const ptr = maybe_ptr orelse break :blk;
                            sentinel = @as(*align(1) const pointer.child, @ptrCast(ptr)).*;
                        }
                    },
                },
                else => break :ptr,
            }
            return if (sentinel) |s|
                str[0..] ++ &[_:s]u8{}
            else
                str[0..];
        },
        else => {},
    };
    @compileError("Can't cast to type " ++ @typeName(T));
}

inline fn getOpMapping(
    comptime Operand: type,
    comptime op: []const u8,
    comptime mapping: anytype,
    comptime arity: comptime_int,
) ?[]const u8 {
    comptime {
        if (!@hasField(@TypeOf(mapping), op)) return null;
        const entry = @field(mapping, op);

        const T = util.ImplicitDeref(Operand);
        switch (@typeInfo(T)) {
            .@"struct", .@"union", .@"enum", .@"opaque" => {},
            else => return null,
        }

        const is_single = @TypeOf(entry) == []const u8;
        const entry_list: []const []const u8 = if (is_single) &.{entry} else entry;

        var idx: comptime_int = entry_list.len;

        const result = for (entry_list, 0..) |name, i| {
            if (!@hasDecl(T, name)) continue;
            const Fn = util.ImplicitDeref(@TypeOf(@field(T, name)));
            if (@typeInfo(Fn).@"fn".params.len != arity) continue;
            idx = i + 1;
            break name;
        } else null;

        for (entry_list[idx..]) |next_name| {
            if (!@hasDecl(T, next_name)) continue;
            const NextFn = util.ImplicitDeref(@TypeOf(@field(T, next_name)));
            if (@typeInfo(NextFn).@"fn".params.len != arity) continue;
            @compileError("Ambiguous resolution between method '" ++ result.? ++ "' and '" ++ next_name ++ "'");
        }

        return result;
    }
}

fn DedupedMethodNames(comptime method_names: anytype) type {
    const info = @typeInfo(@TypeOf(method_names)).@"struct";

    @setEvalBranchQuota(1000 + info.fields.len * 10);
    var fields: [info.fields.len]std.builtin.Type.StructField = undefined;
    for (&fields, info.fields) |*new, old| {
        const T = switch (@typeInfo(old.type)) {
            .pointer => |pointer| switch (@typeInfo(pointer.child)) {
                .int, .comptime_int => []const u8,
                .array => |array| switch (@typeInfo(array.child)) {
                    .int, .comptime_int => []const u8,
                    else => []const []const u8,
                },
                else => []const []const u8,
            },
            else => @compileError("Unexpected type " ++ @typeName(old.type)),
        };
        const old_value: T = @as(*align(1) const old.type, @ptrCast(old.default_value_ptr)).*;
        new.* = .{
            .name = util.dedupe.scalarValue(old.name[0..].*),
            .type = T,
            .is_comptime = true,
            .default_value_ptr = @ptrCast(util.dedupe.scalarValue(old_value)),
            .alignment = 0,
        };
    }

    const deduped_fields = util.dedupe.scalarSlice(std.builtin.Type.StructField, fields);
    return DedupedMethodNamesImpl(deduped_fields);
}
fn DedupedMethodNamesImpl(comptime fields: []const std.builtin.Type.StructField) type {
    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .backing_integer = null,
        .is_tuple = false,
        .decls = &.{},
        .fields = fields,
    } });
}

test fnMethod {
    const CustomNum = enum(i32) {
        _,

        pub inline fn from(value: anytype) @This() {
            return @enumFromInt(value);
        }

        pub inline fn add(self: @This(), other: @This()) @This() {
            return @enumFromInt(@intFromEnum(self) + @intFromEnum(other));
        }
        pub inline fn sub(self: @This(), other: @This()) @This() {
            return @enumFromInt(@intFromEnum(self) - @intFromEnum(other));
        }
        pub inline fn neg(self: @This()) @This() {
            return @enumFromInt(-@intFromEnum(self));
        }
        pub inline fn mul(self: @This(), other: anytype) @This() {
            return @enumFromInt(@intFromEnum(self) * switch (@TypeOf(other)) {
                @This() => @intFromEnum(other),
                comptime_int => other,
                else => |Other| @compileError("Unexpected type " ++ @typeName(Other)),
            });
        }
    };

    const fm_ctx = fnMethod(comath.ctx.simple({}), .{
        .@"+" = "add",
        .@"-" = &.{ "sub", "neg" },
        .@"*" = "mul",
        .evalFuncCall = "mul",
    });
    try std.testing.expectEqual(CustomNum.from(2), comath.eval("a + -b - c", fm_ctx, .{
        .a = @as(CustomNum, @enumFromInt(22)),
        .b = @as(CustomNum, @enumFromInt(9)),
        .c = @as(CustomNum, @enumFromInt(11)),
    }));
    try std.testing.expectEqual(CustomNum.from(77), comath.eval("a(b) * 1", fm_ctx, .{
        .a = @as(CustomNum, @enumFromInt(11)),
        .b = @as(CustomNum, @enumFromInt(7)),
    }));
    try std.testing.expectEqual(CustomNum.from(62), comath.eval("num(31)(2)", fm_ctx, .{
        .num = CustomNum.from,
    }));
}
