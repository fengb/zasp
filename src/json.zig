const std = @import("std");
const builtin = @import("builtin");

pub const path = @import("json/path.zig");

const log = std.log.scoped(.zasp);

const fifo_block_size = 16;

pub fn Formatter(comptime T: type) type {
    return struct {
        data: T,

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            // TODO: convert stringify options
            return std.json.stringify(self.data, .{ .string = .{ .String = .{} } }, writer);
        }
    };
}

pub fn format(data: anytype) Formatter(@TypeOf(data)) {
    return .{ .data = data };
}

pub fn stream(reader: anytype) Stream(@TypeOf(reader)) {
    return .{
        .reader = reader,
        .parser = std.json.StreamingParser.init(),
    };
}

pub const StreamState = struct {
    parser: std.json.StreamingParser,
    element_number: usize,
    count: usize,
};

pub fn Stream(comptime Reader: type) type {
    return struct {
        const Self = @This();

        reader: Reader,
        parser: std.json.StreamingParser,

        // Helper for determining parse state validity
        element_number: usize = 0,
        count: usize = 0,
        // TODO: add more reasons for failure (parse error, etc)
        parse_failure: ?ParseFailure = null,

        _root: ?Element = null,
        // Unread bytes inside `back_fifo`. If this is 0, the following call to nextByte() will read in a new block (16 bytes)
        _back_cursor: usize = 0,
        _back_fifo: std.fifo.LinearFifo(u8, .{ .Static = 0x1000 }) = std.fifo.LinearFifo(u8, .{ .Static = 0x1000 }).init(),

        const ParseFailure = union(enum) {
            wrong_element: struct { wanted: ElementType, actual: ElementType },
        };

        const ElementType = enum { Object, Array, String, Number, Boolean, Null };

        pub const Error = Reader.Error || ParseError;
        pub const ParseError = std.json.StreamingParser.Error || error{
            WrongElementType,
            UnexpectedEndOfJson,
        };

        pub const Element = struct {
            ctx: *Self,
            kind: ElementType,

            element_number: usize,
            stack_level: usize,

            fn init(ctx: *Self) Error!?Element {
                ctx.assertState(&.{ .ValueBegin, .ValueBeginNoClosing, .TopLevelBegin });

                const start_state = ctx.parser.state;

                var byte: u8 = undefined;
                const kind: ElementType = blk: {
                    while (true) {
                        byte = try ctx.nextByte();

                        if (try ctx.feed(byte)) |token| {
                            switch (token) {
                                .ArrayBegin => break :blk .Array,
                                .ObjectBegin => break :blk .Object,
                                .ArrayEnd, .ObjectEnd => return null,
                                else => ctx.assertFailure("Element unrecognized: {}", .{token}),
                            }
                        }

                        if (ctx.parser.state != start_state) {
                            switch (ctx.parser.state) {
                                .String => break :blk .String,
                                .Number, .NumberMaybeDotOrExponent, .NumberMaybeDigitOrDotOrExponent => break :blk .Number,
                                .TrueLiteral1, .FalseLiteral1 => break :blk .Boolean,
                                .NullLiteral1 => break :blk .Null,
                                else => ctx.assertFailure("Element unrecognized: {}", .{ctx.parser.state}),
                            }
                        }
                    }
                };
                ctx.element_number += 1;
                return Element{
                    .ctx = ctx,
                    .kind = kind,
                    .element_number = ctx.element_number,
                    .stack_level = ctx.parser.stack.len,
                };
            }

            pub fn pathMatch(self: Element, comptime T: type) !T {
                return try path.match(self, T);
            }

            pub fn pathMatchAlloc(self: Element, allocator: std.mem.Allocator, comptime T: type) !T {
                return try path.matchAlloc(allocator, self, T);
            }

            pub fn pathMatchFree(_: Element, allocator: std.mem.Allocator, value: anytype) void {
                return try path.freeMatch(allocator, value);
            }

            pub fn boolean(self: Element) Error!bool {
                try self.validateType(.Boolean);
                self.ctx.assertState(&.{ .TrueLiteral1, .FalseLiteral1 });

                switch ((try self.finalizeToken()).?) {
                    .True => return true,
                    .False => return false,
                    else => unreachable,
                }
            }

            pub fn optionalBoolean(self: Element) Error!?bool {
                if (try self.checkOptional()) {
                    return null;
                } else {
                    return try self.boolean();
                }
            }

            pub fn optionalNumber(self: Element, comptime T: type) !?T {
                if (try self.checkOptional()) {
                    return null;
                } else {
                    return try self.number(T);
                }
            }

            pub fn number(self: Element, comptime T: type) !T {
                try self.validateType(.Number);

                switch (@typeInfo(T)) {
                    .Int => {
                        // +1 for converting floor -> ceil
                        // +1 for negative sign
                        // +1 for simplifying terminating character detection
                        const max_digits = std.math.log10(std.math.maxInt(T)) + 3;
                        var buffer: [max_digits]u8 = undefined;

                        return try std.fmt.parseInt(T, try self.numberBuffer(&buffer), 10);
                    },
                    .Float => {
                        const max_digits = 0x1000; // Yeah this is a total kludge, but floats are hard. :(
                        var buffer: [max_digits]u8 = undefined;

                        return try std.fmt.parseFloat(T, try self.numberBuffer(&buffer));
                    },
                    else => @compileError("Unsupported number type '" ++ @typeName(T) ++ "'"),
                }
            }

            fn numberBuffer(self: Element, buffer: []u8) (Error || error{Overflow})![]u8 {
                // First byte already went into the state machine parser
                buffer[0] = self.ctx.peekPrevByte();

                for (buffer[1..]) |*c, i| {
                    const byte = try self.ctx.nextByte();

                    if (try self.ctx.feed(byte)) |token| {
                        const len = i + 1;
                        self.ctx.assert(token == .Number);
                        self.ctx.assert(token.Number.count == len);
                        return buffer[0..len];
                    } else {
                        c.* = byte;
                    }
                }

                return error.Overflow;
            }

            pub fn stringBuffer(self: Element, buffer: []u8) (Error || error{StreamTooLong})![]u8 {
                const reader = try self.stringReader();
                const size = try reader.readAll(buffer);
                if (size == buffer.len) {
                    if (reader.readByte()) |_| {
                        return error.StreamTooLong;
                    } else |err| switch (err) {
                        error.EndOfStream => {},
                        else => |e| return e,
                    }
                }
                return buffer[0..size];
            }

            pub fn stringBoundedArray(self: Element, comptime max_size: usize) (Error || error{StreamTooLong})!std.BoundedArray(u8, max_size) {
                var result: std.BoundedArray(u8, max_size) = undefined;
                const string = try self.stringBuffer(&result.buffer);
                result.len = string.len;
                return result;
            }

            const StringReader = std.io.Reader(
                Element,
                Error,
                (struct {
                    fn read(self: Element, buffer: []u8) Error!usize {
                        if (self.ctx.parser.state != .String) {
                            return 0;
                        }

                        var i: usize = 0;
                        while (i < buffer.len) : (i += 1) {
                            const byte = try self.ctx.nextByte();

                            if (try self.ctx.feed(byte)) |token| {
                                self.ctx.assert(token == .String);
                                return i;
                            } else if (byte == '\\') {
                                const next = try self.ctx.nextByte();
                                self.ctx.assert((try self.ctx.feed(next)) == null);

                                buffer[i] = switch (next) {
                                    '"' => '"',
                                    '/' => '/',
                                    '\\' => '\\',
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    'b' => 0x08, // backspace
                                    'f' => 0x0C, // form feed
                                    'u' => {
                                        var hexes: [4]u8 = undefined;
                                        for (hexes) |*hex| {
                                            hex.* = try self.ctx.nextByte();
                                            self.ctx.assert((try self.ctx.feed(hex.*)) == null);
                                        }
                                        const MASK = 0b111111;
                                        const charpoint = std.fmt.parseInt(u16, &hexes, 16) catch unreachable;
                                        switch (charpoint) {
                                            0...0x7F => buffer[i] = @intCast(u8, charpoint),
                                            0x80...0x07FF => {
                                                buffer[i] = 0xC0 | @intCast(u8, charpoint >> 6);
                                                i += 1;
                                                buffer[i] = 0x80 | @intCast(u8, charpoint & MASK);
                                            },
                                            0x0800...0xFFFF => {
                                                buffer[i] = 0xE0 | @intCast(u8, charpoint >> 12);
                                                i += 1;
                                                buffer[i] = 0x80 | @intCast(u8, charpoint >> 6 & MASK);
                                                i += 1;
                                                buffer[i] = 0x80 | @intCast(u8, charpoint & MASK);
                                            },
                                        }
                                        continue;
                                    },
                                    // should have been handled by the internal parser
                                    else => unreachable,
                                };
                            } else {
                                buffer[i] = byte;
                            }
                        }

                        return buffer.len;
                    }
                }).read,
            );

            pub fn stringReader(self: Element) Error!StringReader {
                try self.validateType(.String);

                return StringReader{ .context = self };
            }

            pub fn optionalStringReader(self: Element) Error!?StringReader {
                if (try self.checkOptional()) {
                    return null;
                } else {
                    return try self.stringReader();
                }
            }

            pub fn optionalStringBuffer(self: Element, buffer: []u8) (Error || error{StreamTooLong})!?[]u8 {
                if (try self.checkOptional()) {
                    return null;
                } else {
                    return try self.stringBuffer(buffer);
                }
            }

            pub fn optionalStringBoundedArray(self: Element, comptime max_size: usize) (Error || error{StreamTooLong})!?std.BoundedArray(u8, max_size) {
                if (try self.checkOptional()) {
                    return null;
                } else {
                    return try self.stringBoundedArray(max_size);
                }
            }

            pub fn arrayNext(self: Element) Error!?Element {
                try self.validateType(.Array);

                // This array has been closed out.
                // TODO: evaluate to see if this is actually robust
                if (self.ctx.parser.stack.len < self.stack_level) {
                    return null;
                }

                // Scan for next element
                while (self.ctx.parser.state == .ValueEnd) {
                    if (try self.ctx.feed(try self.ctx.nextByte())) |token| {
                        self.ctx.assert(token == .ArrayEnd);
                        return null;
                    }
                }

                return try Element.init(self.ctx);
            }

            fn ObjectMatch(comptime TagType: type) type {
                return struct {
                    key: TagType,
                    value: Element,
                };
            }

            pub fn objectMatch(self: Element, comptime Enum: type) !?ObjectMatch(Enum) {
                comptime var string_keys: []const []const u8 = &.{};
                inline for (std.meta.fields(Enum)) |field| {
                    string_keys = string_keys ++ [_][]const u8{field.name};
                }

                const raw_match = (try self.objectMatchAny(string_keys)) orelse return null;
                inline for (string_keys) |key| {
                    if (std.mem.eql(u8, key, raw_match.key)) {
                        return ObjectMatch(Enum){ .key = @field(Enum, key), .value = raw_match.value };
                    }
                }
                unreachable;
            }

            const ObjectMatchString = ObjectMatch([]const u8);

            pub fn objectNextBuffer(self: Element, key_buffer: []u8) (Error || error{StreamTooLong})!?ObjectMatchString {
                try self.validateType(.Object);

                // This object has been closed out.
                // TODO: evaluate to see if this is actually robust
                if (self.ctx.parser.stack.len < self.stack_level) {
                    return null;
                }

                // Scan for next element
                while (self.ctx.parser.state == .ValueEnd) {
                    if (try self.ctx.feed(try self.ctx.nextByte())) |token| {
                        self.ctx.assert(token == .ObjectEnd);
                        return null;
                    }
                }

                const key_element = (try Element.init(self.ctx)) orelse return null;
                const key = try key_element.stringBuffer(key_buffer);

                // Skip over the colon
                while (self.ctx.parser.state == .ObjectSeparator) {
                    _ = try self.ctx.feed(try self.ctx.nextByte());
                }

                return ObjectMatchString{
                    .key = key,
                    .value = (try Element.init(self.ctx)).?,
                };
            }

            pub fn objectMatchOne(self: Element, key: []const u8) Error!?ObjectMatchString {
                return self.objectMatchAny(&[_][]const u8{key});
            }

            pub fn objectMatchAny(self: Element, keys: []const []const u8) Error!?ObjectMatchString {
                try self.validateType(.Object);

                while (true) {
                    // This object has been closed out.
                    // TODO: evaluate to see if this is actually robust
                    if (self.ctx.parser.stack.len < self.stack_level) {
                        return null;
                    }

                    // Scan for next element
                    while (self.ctx.parser.state == .ValueEnd) {
                        if (try self.ctx.feed(try self.ctx.nextByte())) |token| {
                            self.ctx.assert(token == .ObjectEnd);
                            return null;
                        }
                    }

                    const key_element = (try Element.init(self.ctx)) orelse return null;
                    self.ctx.assert(key_element.kind == .String);

                    const key_match = try key_element.stringFind(keys);

                    // Skip over the colon
                    while (self.ctx.parser.state == .ObjectSeparator) {
                        _ = try self.ctx.feed(try self.ctx.nextByte());
                    }

                    if (key_match) |key| {
                        // Match detected
                        return ObjectMatchString{
                            .key = key,
                            .value = (try Element.init(self.ctx)).?,
                        };
                    } else {
                        // Skip over value
                        const value_element = (try Element.init(self.ctx)).?;
                        _ = try value_element.finalizeToken();
                    }
                }
            }

            fn stringFind(self: Element, checks: []const []const u8) !?[]const u8 {
                self.ctx.assert(self.kind == .String);

                var last_byte: u8 = undefined;
                var prev_match: []const u8 = &[0]u8{};
                var parsed_len: usize = 0;
                var string_complete = false;

                for (checks) |check| {
                    if (string_complete and std.mem.eql(u8, check, prev_match[0 .. parsed_len - 1])) {
                        return check;
                    }

                    if (parsed_len > check.len) {
                        continue;
                    }
                    if (parsed_len >= 2 and !std.mem.eql(u8, check[0 .. parsed_len - 2], prev_match[0 .. parsed_len - 2])) {
                        continue;
                    }
                    if (parsed_len >= 1 and (parsed_len - 1 >= check.len or check[parsed_len - 1] != last_byte)) {
                        continue;
                    }

                    prev_match = check;
                    while (!string_complete and parsed_len <= check.len and
                        (parsed_len < 1 or check[parsed_len - 1] == last_byte)) : (parsed_len += 1)
                    {
                        last_byte = try self.ctx.nextByte();
                        if (try self.ctx.feed(last_byte)) |token| {
                            self.ctx.assert(token == .String);
                            string_complete = true;
                            if (parsed_len == check.len) {
                                return check;
                            }
                        }
                    }
                }

                if (!string_complete) {
                    const token = try self.finalizeToken();
                    self.ctx.assert(token.? == .String);
                }
                return null;
            }

            fn checkOptional(self: Element) !bool {
                if (self.kind != .Null) return false;
                self.ctx.assertState(&.{.NullLiteral1});

                _ = try self.finalizeToken();
                return true;
            }

            fn validateType(self: Element, wanted: ElementType) error{WrongElementType}!void {
                if (self.kind != wanted) {
                    self.ctx.parse_failure = ParseFailure{
                        .wrong_element = .{ .wanted = wanted, .actual = self.kind },
                    };
                    return error.WrongElementType;
                }
            }

            /// Dump the rest of this element into a writer.
            pub fn debugDump(self: Element, writer: anytype) !void {
                const state = self.ctx.save();
                errdefer self.ctx.restore(state) catch {};

                try writer.writeByte(self.ctx.peekPrevByte());
                _ = try self.finalizeTokenWriter(writer);

                try self.ctx.restore(state);
            }

            pub fn finalizeToken(self: Element) Error!?std.json.Token {
                return self.finalizeTokenWriter(null);
            }

            fn finalizeTokenWriter(self: Element, writer: anytype) !?std.json.Token {
                switch (self.kind) {
                    .Boolean, .Null, .Number, .String => {
                        self.ctx.assert(self.element_number == self.ctx.element_number);

                        switch (self.ctx.parser.state) {
                            .ValueEnd, .TopLevelEnd, .ValueBeginNoClosing => return null,
                            else => {},
                        }
                    },
                    .Array, .Object => {
                        if (self.ctx.parser.stack.len == self.stack_level - 1) {
                            // Assert the parser state
                            return null;
                        } else {
                            self.ctx.assert(self.ctx.parser.stack.len >= self.stack_level);
                        }
                    },
                }

                while (true) {
                    const byte = try self.ctx.nextByte();
                    if (try self.ctx.feed(byte)) |token| {
                        if (@TypeOf(writer) != @TypeOf(null)) {
                            // Number termination feeds in an extra byte
                            if (self.kind != .Number) {
                                try writer.writeByte(byte);
                            }
                        }
                        switch (self.kind) {
                            .Boolean => self.ctx.assert(token == .True or token == .False),
                            .Null => self.ctx.assert(token == .Null),
                            .Number => self.ctx.assert(token == .Number),
                            .String => self.ctx.assert(token == .String),
                            .Array => {
                                if (self.ctx.parser.stack.len >= self.stack_level) {
                                    continue;
                                }
                                // Number followed by ArrayEnd generates two tokens at once
                                // causing raw token assertion to be unreliable.
                                self.ctx.assert(byte == ']');
                                return .ArrayEnd;
                            },
                            .Object => {
                                if (self.ctx.parser.stack.len >= self.stack_level) {
                                    continue;
                                }
                                // Number followed by ObjectEnd generates two tokens at once
                                // causing raw token assertion to be unreliable.
                                self.ctx.assert(byte == '}');
                                return .ObjectEnd;
                            },
                        }
                        return token;
                    } else {
                        if (@TypeOf(writer) != @TypeOf(null)) {
                            try writer.writeByte(byte);
                        }
                    }
                }
            }
        };

        pub fn root(self: *Self) Error!Element {
            if (self._root == null) {
                self._root = (try Element.init(self)).?;
            }
            return self._root.?;
        }

        pub fn save(self: Self) StreamState {
            return .{
                .parser = self.parser,
                .element_number = self.element_number,
                .count = self.count,
            };
        }

        pub fn restore(self: *Self, state: StreamState) error{NoSpaceLeft}!void {
            const new_back_cursor = self.count - state.count + self._back_cursor;
            if (new_back_cursor > self._back_fifo.buf.len) {
                return error.NoSpaceLeft;
            }

            self.parser = state.parser;
            self.element_number = state.element_number;
            self.count = state.count;

            self._back_cursor = new_back_cursor;
        }

        pub fn format(ctx: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = ctx;
            _ = fmt;
            _ = options;
            return writer.print("{s}{{ [TODO: add useful body] }}", .{@typeName(Self)});
        }

        fn assertState(ctx: *Self, valids: []const std.json.StreamingParser.State) void {
            for (valids) |valid| {
                if (ctx.parser.state == valid) {
                    return;
                }
            }
            ctx.assertFailure("Unexpected state: {}", .{ctx.parser.state});
        }

        fn assert(ctx: *Self, cond: bool) void {
            if (!cond) {
                std.debug.print("{}", .{ctx.debugInfo()});
                unreachable;
            }
        }

        fn assertFailure(ctx: *Self, comptime fmt: []const u8, args: anytype) noreturn {
            if (std.debug.runtime_safety) {
                std.debug.print("{}\n", .{ctx.debugInfo()});
                var buffer: [0x1000]u8 = undefined;
                @panic(std.fmt.bufPrint(&buffer, fmt, args) catch &buffer);
            }
            unreachable;
        }

        const DebugInfo = struct {
            ctx: *Self,

            pub fn format(self: DebugInfo, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = fmt;
                _ = options;

                // TODO: ingest some additional bytes

                const fifo = &self.ctx._back_fifo;
                if (fifo.head + fifo.count <= fifo.buf.len) {
                    try writer.writeAll(fifo.buf[fifo.head..][0..fifo.count]);
                } else {
                    try writer.writeAll(fifo.buf[fifo.head..]);
                    const remaining = fifo.count - fifo.head;
                    try writer.writeAll(fifo.buf[0..remaining]);
                }

                if (self.ctx.parse_failure) |parse_failure| switch (parse_failure) {
                    .wrong_element => |wrong_element| {
                        try writer.print("WrongElementType - wanted: {s}", .{@tagName(wrong_element.wanted)});
                    },
                };
            }
        };

        pub fn debugInfo(ctx: *Self) DebugInfo {
            return .{ .ctx = ctx };
        }

        fn peekPrevByte(ctx: *Self) u8 {
            return ctx._back_fifo.peekItem(ctx._back_fifo.count - ctx._back_cursor - 1);
        }

        fn nextByte(ctx: *Self) Error!u8 {
            if (ctx._back_cursor == 0) {
                if (ctx._back_fifo.writableLength() == 0) {
                    ctx._back_fifo.discard(fifo_block_size);
                }

                const write_size = std.math.min(ctx._back_fifo.writableLength(), fifo_block_size);

                const buf = ctx._back_fifo.writableSlice(0)[0..write_size];
                const read = ctx.reader.read(buf) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfJson,
                    else => |e| return e,
                };
                ctx._back_fifo.update(read);
                ctx._back_cursor = @intCast(u8, read);
            }

            defer ctx._back_cursor -= 1;
            return ctx._back_fifo.peekItem(ctx._back_fifo.count - ctx._back_cursor);
        }

        // A simpler feed() to enable one liners.
        // token2 can only be close object/array and we don't need it
        fn feed(ctx: *Self, byte: u8) !?std.json.Token {
            ctx.count += 1;
            var token1: ?std.json.Token = undefined;
            var token2: ?std.json.Token = undefined;
            try ctx.parser.feed(byte, &token1, &token2);
            return token1;
        }
    };
}

fn expectEqual(actual: anytype, expected: ExpectedType(@TypeOf(actual))) !void {
    try std.testing.expectEqual(expected, actual);
}

fn ExpectedType(comptime ActualType: type) type {
    if (@typeInfo(ActualType) == .Union) {
        return std.meta.Tag(ActualType);
    } else {
        return ActualType;
    }
}

test "boolean" {
    var fbs = std.io.fixedBufferStream("[true]");
    var str = stream(fbs.reader());

    const root = try str.root();
    const element = (try root.arrayNext()).?;
    try expectEqual(element.kind, .Boolean);
    try expectEqual(try element.boolean(), true);
}

test "null" {
    var fbs = std.io.fixedBufferStream("[null]");
    var str = stream(fbs.reader());

    const root = try str.root();
    const element = (try root.arrayNext()).?;
    try expectEqual(element.kind, .Null);
    try expectEqual(try element.optionalBoolean(), null);
}

test "integer" {
    {
        var fbs = std.io.fixedBufferStream("[1]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(u8), 1);
    }
    {
        // Technically invalid, but we don't str far enough to find out
        var fbs = std.io.fixedBufferStream("[123,]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(u8), 123);
    }
    {
        var fbs = std.io.fixedBufferStream("[-128]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(i8), -128);
    }
    {
        var fbs = std.io.fixedBufferStream("[456]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(element.number(u8), error.Overflow);
    }
}

test "float" {
    {
        var fbs = std.io.fixedBufferStream("[1.125]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(f32), 1.125);
    }
    {
        // Technically invalid, but we don't str far enough to find out
        var fbs = std.io.fixedBufferStream("[2.5,]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(f64), 2.5);
    }
    {
        var fbs = std.io.fixedBufferStream("[-1]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(try element.number(f64), -1);
    }
    {
        var fbs = std.io.fixedBufferStream("[1e64]");
        var str = stream(fbs.reader());

        const root = try str.root();
        const element = (try root.arrayNext()).?;
        try expectEqual(element.kind, .Number);
        try expectEqual(element.number(f64), 1e64);
    }
}

test "string" {
    {
        var fbs = std.io.fixedBufferStream(
            \\"hello world"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        try expectEqual(element.kind, .String);
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("hello world", try element.stringBuffer(&buffer));
    }
}

test "string escapes" {
    {
        var fbs = std.io.fixedBufferStream(
            \\"hello\nworld\t"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        try expectEqual(element.kind, .String);
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("hello\nworld\t", try element.stringBuffer(&buffer));
    }
}

test "string unicode escape" {
    {
        var fbs = std.io.fixedBufferStream(
            \\"\u0024"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("$", try element.stringBuffer(&buffer));
    }
    {
        var fbs = std.io.fixedBufferStream(
            \\"\u00A2"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("¢", try element.stringBuffer(&buffer));
    }
    {
        var fbs = std.io.fixedBufferStream(
            \\"\u0939"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("ह", try element.stringBuffer(&buffer));
    }
    {
        var fbs = std.io.fixedBufferStream(
            \\"\u20AC"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("€", try element.stringBuffer(&buffer));
    }
    {
        var fbs = std.io.fixedBufferStream(
            \\"\uD55C"
        );
        var str = stream(fbs.reader());

        const element = try str.root();
        var buffer: [100]u8 = undefined;
        try std.testing.expectEqualStrings("한", try element.stringBuffer(&buffer));
    }
}

test "empty array" {
    var fbs = std.io.fixedBufferStream("[]");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    try expectEqual(try root.arrayNext(), null);
}

test "array of simple values" {
    var fbs = std.io.fixedBufferStream("[false, true, null]");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);
    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.boolean(), false);
    }

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.boolean(), true);
    }

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.optionalBoolean(), null);
    }

    try expectEqual(try root.arrayNext(), null);
}

test "array of numbers" {
    var fbs = std.io.fixedBufferStream("[1, 2, -3]");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.number(u8), 1);
    }

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.number(u8), 2);
    }

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try expectEqual(try item.number(i8), -3);
    }

    try expectEqual(try root.arrayNext(), null);
}

test "array of strings" {
    var fbs = std.io.fixedBufferStream(
        \\["hello", "world"]);
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    var buffer: [100]u8 = undefined;
    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "hello", try item.stringBuffer(&buffer));
    }

    {
        const item = (try root.arrayNext()) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "world", try item.stringBuffer(&buffer));
    }

    try expectEqual(try root.arrayNext(), null);
}

test "array early finalize" {
    var fbs = std.io.fixedBufferStream(
        \\[1, 2, 3]
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    while (try root.arrayNext()) |_| {
        _ = try root.finalizeToken();
    }
}

test "objects ending in number" {
    var fbs = std.io.fixedBufferStream(
        \\[{"id":0},{"id": 1}, {"id": 2}]
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    while (try root.arrayNext()) |obj| {
        if (try obj.objectMatchOne("banana")) |_| {
            std.debug.panic("How did this match?", .{});
        }
    }
}

test "empty object" {
    var fbs = std.io.fixedBufferStream("{}");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    try expectEqual(try root.objectMatchOne(""), null);
}

test "object next" {
    var fbs = std.io.fixedBufferStream(
        \\{"foo": true, "bar": false}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    var key_buffer: [0x100]u8 = undefined;
    {
        const match = (try root.objectNextBuffer(&key_buffer)) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "foo", match.key);
        try expectEqual(try match.value.boolean(), true);
    }

    {
        const match = (try root.objectNextBuffer(&key_buffer)) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "bar", match.key);
        try expectEqual(try match.value.boolean(), false);
    }

    try expectEqual(try root.objectNextBuffer(&key_buffer), null);
}

test "object match" {
    var fbs = std.io.fixedBufferStream(
        \\{"foo": true, "bar": false}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    {
        const match = (try root.objectMatchOne("foo")) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "foo", match.key);
        try expectEqual(try match.value.boolean(), true);
    }

    {
        const match = (try root.objectMatchOne("bar")) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "bar", match.key);
        try expectEqual(try match.value.boolean(), false);
    }
}

test "object match any" {
    var fbs = std.io.fixedBufferStream(
        \\{"foo": true, "foobar": false, "bar": null}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    {
        const match = (try root.objectMatchAny(&[_][]const u8{ "foobar", "foo" })) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "foo", match.key);
        try expectEqual(try match.value.boolean(), true);
    }

    {
        const match = (try root.objectMatchAny(&[_][]const u8{ "foo", "foobar" })) orelse return error.ExpectedValue;
        try std.testing.expectEqualSlices(u8, "foobar", match.key);
        try expectEqual(try match.value.boolean(), false);
    }
}

test "object match long match fail" {
    var fbs = std.io.fixedBufferStream(
        \\{"foobar": false}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    try expectEqual(try root.objectMatchAny(&[_][]const u8{ "foobaz", "foo" }), null);
}

test "object match" {
    var fbs = std.io.fixedBufferStream(
        \\{"foo": true, "foobar": false, "bar": null}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    {
        const match = (try root.objectMatch(enum { foobar, foo })) orelse return error.ExpectedValue;
        try expectEqual(match.key, .foo);
        try expectEqual(try match.value.boolean(), true);
    }

    {
        const match = (try root.objectMatch(enum { foo, foobar })) orelse return error.ExpectedValue;
        try expectEqual(match.key, .foobar);
        try expectEqual(try match.value.boolean(), false);
    }
}

test "object match not found" {
    var fbs = std.io.fixedBufferStream(
        \\{"foo": [[]], "bar": false, "baz": {}}
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    try expectEqual(try root.objectMatchOne("???"), null);
}

fn expectValidElement(e: anytype) Stream(std.io.FixedBufferStream([]const u8).Reader).Error!void {
    switch (e.kind) {
        // TODO: test objects better
        .Object => _ = try e.finalizeToken(),
        .Array => {
            while (try e.arrayNext()) |child| {
                try expectValidElement(child);
            }
        },
        .String => _ = try e.finalizeToken(),
        // TODO: fix inferred errors
        // .Number => _ = try e.number(u64),
        .Number => _ = try e.finalizeToken(),
        .Boolean => _ = try e.boolean(),
        .Null => _ = try e.optionalBoolean(),
    }
}

fn expectValidParseOutput(input: []const u8) !void {
    var fbs = std.io.fixedBufferStream(input);
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectValidElement(root);
}

test "smoke" {
    try expectValidParseOutput(
        \\[[], [], [[]], [[""], [], [[], 0], null], false]
    );
}

test "finalizeToken on object" {
    var fbs = std.io.fixedBufferStream("{}");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Object);

    try expectEqual(try root.finalizeToken(), .ObjectEnd);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
}

test "finalizeToken on string" {
    var fbs = std.io.fixedBufferStream(
        \\"foo"
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .String);

    try expectEqual((try root.finalizeToken()).?, .String);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
    try expectEqual(try root.finalizeToken(), null);
}

test "finalizeToken on number" {
    var fbs = std.io.fixedBufferStream("[[1234,5678]]");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    const inner = (try root.arrayNext()).?;
    try expectEqual(inner.kind, .Array);

    const first = (try inner.arrayNext()).?;
    try expectEqual(first.kind, .Number);
    try expectEqual((try first.finalizeToken()).?, .Number);
    try expectEqual(try first.finalizeToken(), null);
    try expectEqual(try first.finalizeToken(), null);
    try expectEqual(try first.finalizeToken(), null);
    try expectEqual(try first.finalizeToken(), null);

    const second = (try inner.arrayNext()).?;
    try expectEqual(second.kind, .Number);
    try expectEqual((try second.finalizeToken()).?, .Number);
    try expectEqual(try second.finalizeToken(), null);
    try expectEqual(try second.finalizeToken(), null);
    try expectEqual(try second.finalizeToken(), null);
}

test "save/restore arrays" {
    var fbs = std.io.fixedBufferStream("[[1234, 5678]]");
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    const inner = (try root.arrayNext()).?;
    try expectEqual(inner.kind, .Array);

    const state = str.save();

    {
        const first = (try inner.arrayNext()).?;
        try expectEqual(try first.number(u32), 1234);
        const second = (try inner.arrayNext()).?;
        try expectEqual(try second.number(u32), 5678);
    }

    try str.restore(state);
    {
        const first = (try inner.arrayNext()).?;
        try expectEqual(try first.number(u32), 1234);
        const second = (try inner.arrayNext()).?;
        try expectEqual(try second.number(u32), 5678);
    }

    try str.restore(state);
    {
        const first = (try inner.arrayNext()).?;
        try expectEqual(try first.number(u32), 1234);
    }

    try str.restore(state);
    {
        const first = (try inner.arrayNext()).?;
        try expectEqual(try first.number(u32), 1234);
        const second = (try inner.arrayNext()).?;
        try expectEqual(try second.number(u32), 5678);
    }
}

test "debug dump" {
    var fbs = std.io.fixedBufferStream(
        \\["foo", 1, false, null, [0], {}]
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    var buf: [0x100]u8 = undefined;
    var dump = std.io.fixedBufferStream(&buf);

    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("\"foo\"", dump.getWritten());

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("\"foo\"", dump.getWritten());
    }
    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("1", dump.getWritten());
    }
    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("false", dump.getWritten());
    }
    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("null", dump.getWritten());
    }
    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("[0]", dump.getWritten());
    }
    {
        const item = (try root.arrayNext()).?;
        defer _ = item.finalizeToken() catch unreachable;

        dump.reset();
        try item.debugDump(dump.writer());
        try std.testing.expectEqualStrings("{}", dump.getWritten());
    }
}

test "save/restore objects" {
    var fbs = std.io.fixedBufferStream(
        \\[{ "foo": 69, "bar": 420 }]
    );
    var str = stream(fbs.reader());

    const root = try str.root();
    try expectEqual(root.kind, .Array);

    const inner = (try root.arrayNext()).?;
    try expectEqual(inner.kind, .Object);

    const state = str.save();

    {
        const match = (try inner.objectMatchOne("foo")).?;
        try expectEqual(try match.value.number(u32), 69);

        try expectEqual(try inner.objectMatchOne("foo"), null);
        try expectEqual(try inner.objectMatchOne("bar"), null);
    }

    try str.restore(state);
    {
        const match = (try inner.objectMatchOne("bar")).?;
        try expectEqual(try match.value.number(u32), 420);

        try expectEqual(try inner.objectMatchOne("foo"), null);
        try expectEqual(try inner.objectMatchOne("bar"), null);
    }

    try str.restore(state);
    {
        try expectEqual(try inner.objectMatchOne("banana"), null);
        try expectEqual(try inner.objectMatchOne("foo"), null);
        try expectEqual(try inner.objectMatchOne("bar"), null);
    }

    try str.restore(state);
    {
        const match = (try inner.objectMatchOne("foo")).?;
        try expectEqual(try match.value.number(u32), 69);

        const match2 = (try inner.objectMatchOne("bar")).?;
        try expectEqual(try match2.value.number(u32), 420);
    }
}

test {
    std.testing.refAllDecls(@This());
}
