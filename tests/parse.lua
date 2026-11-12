#!/usr/bin/env lua
-- same as parser.tests.parse except with the LuaFixed parser
local path = require 'ext.path'
local LuaFixedParser = require 'langfix.parser'
local parser = LuaFixedParser((assert(path(assert(..., "expected filename")):read())))
local tree = parser.tree
--print(tree)	-- this will emit Lua code from the LuaFixed code
print(tree:toLuaFixed{maintainSpan=true})	-- this will emit the original, minified
