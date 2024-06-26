#!/usr/bin/env luajit
require 'ext'
--require 'local-default' -- ... but this uses a shim parser, which langfix does too, so hmm
if jit then require 'ext.ctypes' end
--require 'langfix'
require 'langfix.env'(_G)

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

-- TODO here handle all flags, stop at -- or filename

local fn
do
	local i = 1
	while i <= #arg do
		local s = arg[i]
		-- -e
		if s:sub(1,2) == '-e' then
			-- -ecode
			if #s > 2 then
				assert(load(s:sub(3)))(select(2, ...))
				table.remove(arg, i)
				i = i - 1
			else
			-- -e code
				if i == #arg then
					-- print help
				else
					assert(load(arg[i+1]))(select(2, ...))
					table.remove(arg, i)
					table.remove(arg, i)
					i = i - 1
				end
			end
		else
			-- if it's a flag then handle it
			-- otherwise stop and this is our filename
			fn = s
		end
		i = i + 1
	end
end
if not fn then
	-- interpretive mode here
	require 'interpreter'(_G)
elseif not path(fn):exists() then
	io.stderr:write('lua: cannot open '..fn..': No such file or directory')
else
	assert(loadfile(fin))(select(2, ...))
end
