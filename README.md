# comath: comptime math
The main function this library provides is `eval`, which is of the form
```zig
fn eval(
    comptime expr: []const u8,
    ctx: anytype,
    inputs: anytype,
) !Eval(expr, @TypeOf(ctx), @TypeOf(inputs))
```
wherein the `expr` is expected to be source code representing an expression comprised of:
* Identifiers, whose value should be assigned by a field in `inputs` of the same name
* Integer, float, or character literals
* Grouped expressions delimited by parentheses
* Field accesses (`expression`.`identifier`|`operator symbols`)
* Index accesses (`expression`[`expression`])
* Function calls (`expression`(`expressions`))
* Unary operations (`operator` `expression`)
* Binary operations (`expression` `operator` `expression`)

Given an expression that is successfully parsed into an AST at comptime, said AST will then be evaluated.
The semantics of all the operations are defined by the `ctx` parameter, which should be a type that conforms to a concrete interface,
which can be described in pseudo-code as:
```zig
{
    /// Should contain tags whose names correspond to operators, which
    /// must only be comprised of symbols contained in `operator.symbols`
    pub const UnOp = enum {...};
    /// Same constraints as `UnOp`
    pub const BinOp = enum {...};
    /// Struct value whose fields all correspond to the binary operators defined by `BinOp`,
    /// each with a value of type `operator.Relation` describing the binary operator's precedence
    /// level and associativity.
    pub const relations: operator.RelationMap(BinOp) = .{...};

    /// corresponds to `lhs.field`
    pub fn EvalProperty(comptime Lhs: type, comptime field: []const u8) type {...}
    pub fn evalProperty(ctx: @This(), lhs: anytype, comptime field: []const u8) !EvalProperty(@TypeOf(lhs), field) {...}

    /// corresponds to `lhs[rhs]`
    pub fn EvalIndexAccess(comptime Lhs: type, comptime Rhs: type) type {...}
    pub fn evalIndexAccess(ctx: @This(), lhs: anytype, rhs: anytype) !EvalIndexAccess(@TypeOf(lhs), @TypeOf(rhs)) {...}

    /// corresponds to `callee(args...)`, where `args` is a tuple whose elements are the arguments to `callee`
    pub fn EvalFuncCall(comptime Callee: type, comptime Args: type) type {...}
    pub fn evalFuncCall(ctx: @This(), callee: anytype, args: anytype) !EvalFuncCall(@TypeOf(callee), @TypeOf(args)) {...}

    /// corresponds to `op value`
    pub fn EvalUnOp(comptime op: UnOp, comptime T: type) type {...}
    pub fn evalUnOp(ctx: @This(), comptime op: UnOp, value: anytype) !EvalUnOp(op, @TypeOf(value)) {...}

    /// corresponds to `op value`
    pub fn EvalBinOp(comptime Lhs: type, comptime op: BinOp, comptime Rhs: type) type {...}
    pub fn evalBinOp(ctx: @This(), lhs: anytype, comptime op: BinOp, rhs: anytype) !EvalBinOp(@TypeOf(lhs), op, @TypeOf(rhs)) {...}
}
```

Some built-in contexts are provided in the `contexts` namespace, both for use and for reference.

Example code:
```zig
const std = @import("std");
const comath = @import("comath");

test {
    const ctx = comath.contexts.simpleCtx({});
    const value = comath.eval("a * 2", ctx, .{ .a = 4 }) catch |err| switch (err) {};
    try std.testing.expect(value == 8);
}
```
