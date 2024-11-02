#!/usr/bin/env luajit
--[[
You can use this script as a drop-in replacement for Lua itself, to provide either a CLI or to run files within the "rua" environment/parser.
--]]

require 'ext'
--require 'local-default' -- ... but this uses a shim parser, which langfix does too, so hmm
if jit then require 'ext.ctypes' end
--require 'langfix'
require 'langfix.env'(_G)

-- and while we're here , modify package.path to accept our new extension as well
-- or should this be done in langfix.lua?
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
--print('arg', require 'ext.tolua'(arg))
local oldarg = arg
local runarg = arg[1]	-- file being run ...
arg = {table.unpack(arg, 1)}
--print('arg', require 'ext.tolua'(arg))

-- TODO here handle all flags, stop at -- or filename

local fn
local usedE
do
	local i = 1
	while i <= #arg do
		local s = arg[i]
		-- -l
		if s:sub(1,2) == '-l' then
			-- -llib
			if #s > 2 then
				require(s:sub(3))
				table.remove(arg, i)
				i = i - 1
			else
			-- -l lib
				if i == #arg then
					-- print help
				else
					require(arg[i+1])
					table.remove(arg, i)
					table.remove(arg, i)
					i = i - 1
				end
			end
		-- -e
		elseif s:sub(1,2) == '-e' then
			-- -ecode
			if #s > 2 then
				assert(load(s:sub(3)))(select(2, ...))
				usedE = true
				table.remove(arg, i)
				i = i - 1
			else
			-- -e code
				if i == #arg then
					-- print help
				else
					assert(load(arg[i+1]))()
					usedE = true
					table.remove(arg, i)
					table.remove(arg, i)
					i = i - 1
				end
			end
		elseif s:sub(1,2) == '-i' then
			-- maintain interactive mode even if -e was used
			usedE = false
		else
			-- if it's a flag then handle it
			-- otherwise stop and this is our filename
			fn = s
			-- but if we handle it then every index after it is the arsg right?
			arg = {table.unpack(arg, i+1)}
			break
		end
		i = i + 1
	end
end
--print('arg', require 'ext.tolua'(arg))
--print('fn', fn)
if not fn then
	if not usedE then
		-- interpretive mode here
		require 'interpreter'(_G)
	end
elseif not path(fn):exists() then
	arg[0] = fn
	io.stderr:write('lua: cannot open '..fn..': No such file or directory\n')
else
	assert(loadfile(fn))(table.unpack(arg))
end
