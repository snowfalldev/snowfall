const target = @import("builtin").target;
const std = @import("std");

pub const JITSource = struct {
    pub fn init() !JITSource {}
};

pub const JITOutput = struct {
    bytes: std.ArrayList(u8),

    pub fn init() !JITOutput {}
};
