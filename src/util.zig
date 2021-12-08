const std = @import("std");

/// Allocgate has drastically changed how allocators work, but most of the 
/// breaking changes are due to this type, which we can introspect.
pub const Allocator = @TypeOf(std.testing.allocator);
