const std = @import("std");
const util = @import("util");

const comath = @This();

pub const Eval = @import("eval.zig").Eval;
pub const eval = @import("eval.zig").eval;

pub const operator = @import("operator.zig");
pub const contexts = @import("contexts.zig");

comptime {
    _ = @import("eval.zig");
    _ = operator;
    _ = contexts;
}
