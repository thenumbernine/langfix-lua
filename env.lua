--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
return function(env)
	local showcode = require 'template.showcode'
	local LuaFixedParser = require 'langfix.parser'

	-- set globals here

	env = env or _G

	-- put ffi in env namespace for idiv // operator
	local ffi	-- store it here for the langfix env table
	xpcall(function()
		ffi = require 'ffi'
		env.ffi = ffi
	end, function()
	end)

	-- put 'bit' in the env if it's not there ... for vanilla-lua 5.2 and 5.3(compat?)
	local native_bitops = load'x=y|z'
	if not native_bitops then
		env.bit = bit
		if not env.bit then
			-- TODO in this case, bit32 doesn't have all operations that the luajit bit table does ...
			env.bit = bit32
			if not env.bit32 then
				env.bit = require 'bit'	-- will this be there? or should I try ext.op ?
			end
		end
	end

	local langfix = {}	-- for builtin helpers.  maybe I'll put bitwise metatable invocation here. maybe I'll put other helper functions here.
	env.langfix = langfix
	langfix.ternary = function(t, cbt, cbf)
		if t then
			return cbt()
		else
			return cbf()
		end
	end
	langfix.nilcoalescing = function(t, cb)
		if t ~= nil then
			return t
		else
			return cb()
		end
	end

	langfix.optindex = function(t, k, optassign)
		if t == nil then
			return nil
		end
		local v = t[k]
		if v == nil and optassign then
			v = optassign()
			t[k] = v
		end
		return v
	end
	langfix.optcall = function(v, ...)
		if v == nil then
			return nil
		end
		return v(...)
	end
	langfix.optcallself = function(t, k, optassign, ...)
		return langfix.optcall(langfix.optindex(t, k, optassign), t, ...)
	end

	--local ztable = require '0-based'

	-- notice ffi doesn't load in vanilla lua, so for vanilla lua < 5.3 this will all break
	local native_idiv = load'x=y//z'
	local intptr_t = ffi and ffi.typeof'intptr_t' or nil
	if ffi then	-- luajit
		langfix.idiv = function(a, b)
			-- [[ as integers, but requires ffi access ...
			-- parenthesis required?
			-- I think I got away with not using () to wrap generated-code because of the fact that always the code was generated from sources with correct precedence as it was parsed
			-- so by the fact that the language was specified correctly, so was the AST represented correctly, and so the regenerated code was also correct.
			return intptr_t(a) / intptr_t(b)
			--]]
			--[[ as floats but with floor ... ?  needs a math.sign or math.trunc function, how easy is that to write without generating anonymous lambdas or temp variables?
			return '((function()'
				..' local x = '..args:concat'/'
				..' return x < 0 and math.ceil(x) or math.floor(x)'
				..' end)())'
			--]]
		end
	else	-- vanilla-lua without //
		langfix.idiv = function(a,b)
			return math.floor(a / b)
		end
	end

	require 'ext.load'(env).xforms:insert(function(data, source)
		local tree, result
		local parser = LuaFixedParser()
		local success, msg = parser:setData(data, source)
		if not success then return nil, msg end
		tree = parser.tree
		result = tree:toLua{maintainSpan=true}
--DEBUG(langfix):print('\n'..source..'\n'..showcode(result)..'\n')
		return result
	end)

	--[[
	this gets returned by `require 'langfix'`, and I am suspicious someone is writing it to _G.langfix, which can overwrite env.langfix ... all very ugly ... idk
	somehow somewhere someone was setting _G.langfix=true, and I can only guess it's because someone was setting _G.langfix = require'langfix' (only when using the -l option in vanilla-lua, even 5.3 and 5.4)
	 and couple that with the fact that this function returns nothing, so `require 'langfix'` will return true ...
	 in other words the need to return env.langfix here all stems from a design behavior that -l<library> should always assign `_G.library = require'library'`

	see the behavior yourself:
	a.lua:
		a=42
	lua -la -e "print('a',a)"
		a	true
	lua -e "require 'a' print('a', a)"
		a	42

	--]]
	return langfix
end
