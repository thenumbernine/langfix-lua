#!/usr/bin/env lua
-- same as parser.tests.parse except with the LuaFixed parser
local path = require 'ext.path'
local parser = require 'langfix.parser'
local tree = parser.parse((assert(path(assert(..., "expected filename")):read())))
--print(tree)	-- this will emit Lua code from the LuaFixed code
print(tree:toLuaFixed())	-- this will emit the original, minified
