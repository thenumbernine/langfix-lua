#!/usr/bin/env luajit
require 'ext'
if jit then require 'ext.ctypes' end
require 'langfix'

-- and while we're here , modify package.path to accept our new extension as well
-- or should this be done in langfix.lua?
-- or should it be done in run.sh?
local string = require 'ext.string'
local path = require 'ext.path'
local newext = 'rua'	-- idk
local parts = string.split(package.path, ';')
for i=#parts,1,-1 do
	local name, ext = path(parts[i]):getext()
	if ext == 'lua' then
		parts:insert(i, name..'.'..newext)
	end
end
package.path = parts:concat';'

-- shift global args
local oldarg = arg
arg = {[0]=oldarg[1], table.unpack(arg, 2)}

local fin = assert(..., 'expected filename')
if path(fin):exists() then
	assert(loadfile(fin))(select(2, ...))
else
	assert(load(fin))(select(2, ...))
end
