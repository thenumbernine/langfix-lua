#!/usr/bin/env luajit

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

assert(loadfile((...)))(select(2, ...))
