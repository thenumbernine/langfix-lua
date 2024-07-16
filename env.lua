--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
return function(env)
	local table = require 'ext.table'
	local asserteq = require 'ext.assert'.eq
	local assertindex = require 'ext.assert'.index
	local template = require 'template'
	local showcode = require 'template.showcode'
	local LuaParser = require 'parser.lua.parser'
	local LuaTokenizer = require 'parser.lua.tokenizer'

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
			if cbt then
				return cbt()
			else
				return t
			end
		else
			if cbf then
				return cbf()
			end
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

	local LuaFixedTokenizer = LuaTokenizer:subclass()

	local LuaFixedParser = LuaParser:subclass()

	LuaFixedParser.ast = table(LuaFixedParser.ast)
	local ast = LuaFixedParser.ast

	-- I could insert >>> into the symbols and map it to luajit arshift ...
	ast._ashr = ast._op:subclass{type='ashr', op='>>>'}

	local assignops = table{
		{'concatto', '..='},
		{'addto', '+='},
		{'subto', '-='},
		{'multo', '*='},
		{'divto', '/='},
		{'idivto', '//=', not native_idiv and 'langfix.idiv(%1, %2)' or nil},
		{'modto', '%='},
		{'powto', '^='},
		{'bandto', '&=', not native_bitops and 'bit.band(%1, %2)' or nil},
		{'borto', '|=', not native_bitops and 'bit.bor(%1, %2)' or nil},
		-- oh wait, that's not-equals ... hmm someone didn't think that choice through ...
		-- coincidentally, xor is sometimes denoted as the not-equivalent symbol, because that's basically what it means, but how to distinguish between boolean and bitwise ...
		-- I would like an xor-equals ... but I also like ^ as power operator ... and I don't like ~= as not-equals, but to change that breaks Lua compatability ...
		{'bxorto', '~~=', not native_bitops and 'bit.bxor(%1, %2)' or '%1 ~ %2'},	-- tempting to use ⊕= and just use ⊕ for xor ...
		{'shlto', '<<=', not native_bitops and 'bit.lshift(%1, %2)' or nil},
		{'shrto', '>>=', not native_bitops and 'bit.rshift(%1, %2)' or nil},
		{'ashrto', '>>>=', not native_bitops and 'bit.arshift(%1, %2)' or nil},
	}:mapi(function(info)
		local name, op, binopexpr = table.unpack(info)
		binopexpr = binopexpr or '%1 '..op:sub(1, -2)..' %2'
		local cl = ast._assign:subclass{type=name, op=op, binopexpr=binopexpr}
		info[3] = cl
		ast['_'..name] = cl
		function cl:serialize(apply)
			local vars = table.mapi(self.vars, apply)
			local exprs = table.mapi(self.exprs, apply)
			return vars:concat','..' = '..vars:mapi(function(v,i)
				return '('
					..binopexpr
						:gsub('%%%d', function(j)
							if j == '%1' then return v end
							if j == '%2' then return exprs[i] end
						end)
					..')'
			end):concat','
		end
		return cl
	end)

	-- Make a new 'ast' namespace and subclass all former classes into it so that we don't mess with anyone using the base-class
	ast._idiv = ast._idiv:subclass()
	ast._band = ast._band:subclass()
	ast._bxor = ast._bxor:subclass()
	ast._bor = ast._bor:subclass()
	ast._shl = ast._shl:subclass()
	ast._shr = ast._shr:subclass()

	if not native_idiv then
		function ast._idiv:serialize(apply)
			return 'langfix.idiv('..table.mapi(self, apply):concat','..')'
		end
	end

	-- don't override these if we're running in pure-Lua (except arshift, which will need its own implementation somewhere ... TODO)
	if not native_bitops then
		for _,info in ipairs{
			{type='band'},
			{type='bxor'},
			{type='bor'},
			{type='bnot'},
			{type='shl', func='lshift'},
			{type='shr', func='rshift'},
			{type='ashr', func='arshift'},
		} do
			local func = info.func or info.type

			-- looks like the luajit bitness of 'bit' using Lua-numbers is 32-bit, even for 64-bit arch builds ...
			-- ... unless the input is a (U)LL-number-literal / (u)int64_t-cast-type
			-- ... in which case, 'rshift' is the Lua-equiv zero-padding, and 'arshift' fills with the lhs-most-bit to preserve sign
			local key = '_'..info.type
			local cl = assertindex(ast, key)
			--cl = cl:subclass()	-- TODO fixme
			ast[key] = cl

			-- funny how in lua `function a.b.c:d()` works but `function a.b['c']:d()` doesn't ...
			function cl:serialize(apply)
				local args = table.mapi(self, apply)
				--[[ ffi.arch bitness, or at least intptr_t's bitness, whatever that is (usu 64-bit for ffi.arch == x64)
				-- upside: always 64-bit, even when luajit bit.band would be 32-bit for Lua-numbers
				-- downside: always 64-bit, even when luajit bit.band would be 32-bit for int32_t's
				args = args:mapi(intptr_t)
				--]] -- or don't and just use luajit builtin bit lib bitness as is (usu 32-bit regardless of ffi.arch for Lua numbers, or 64-bit for boxed types):
				return '(bit.'..func..'('..args:concat','..'))'
			end
		end
	end

	local function commasep(exprs, apply)
		return table.mapi(exprs, apply):concat','
	end

	do
		local _ternary = ast._op:subclass()
		_ternary.type = 'ternary'
		ast._ternary = _ternary
		function _ternary:serialize(apply)
			return template([[
langfix.ternary(
	<?=apply(self[1])?>,
<? if self[2] then
?>	function() return <?=commasep(self[2], apply)?> end
<? else
?>	nil
<? end
?>,
<? if self[3] then
?>	function() return <?=commasep(self[3], apply)?> end
<? else
?>	nil
<? end
?>
)
]],			{
				self = self,
				apply = apply,
				commasep = commasep,
			})
		end
	end

	-- TODO these lambdas seem nice but don't always parse correctly
	-- Especially if you are immediately calling them -- in which case, two successive lambdas will try to use the 1st result to call the 2nd ....
	-- So if I have two statements that evaluate to lambdas, then at the statement-level I will need to insert ';' to separate them ...
	-- this tempts me to insert semicolons for all statements
	--[[
	for k,cl in pairs(ast) do
		if ast._stmt:isa(cl) then
			local cl2 = cl:subclass()
			ast['_'..cl2.type] = cl2
			function cl2:serialize(...)
				return cl2.super.serialize(self, ...)..';'
			end
		end
	end
	--]]

	ast._optindex = ast._index:subclass()
	ast._optindex.type = 'optindex'
	function ast._optindex:serialize(apply)
		return template([[
langfix.optindex(
	<?=apply(self.expr)?>,
	<?=apply(self.key)?>,
<? if self.optassign then
?>	function() return <?=apply(self.optassign)?> end
<? else
?>	nil
<? end
?>)
]], 	{
			self = self,
			apply = apply,
		})
	end

	-- ok here's where I start to break things
	-- indexself was only valid when a child of call, so call(indexself(t,k)) turned into t:k(), which was language shorthand for t.k(t)
	-- call(optindexself(t,'k'), v) is going to turn into 't?:k(v)', turns into 'function(t,k, ...) if t==nil then return end return t[k](t, ...) end)(t, 'k', v)
	ast._optindexself = ast._indexself:subclass()
	ast._optindexself.type = 'optindexself'
	function ast._optindexself:serialize(apply)
		-- this should only ever be placed under a call or optcall, which will handle it themselves
		error('here with parent '..tostring(self.parent.type))
		-- indexself key is a Lua string so don't apply()
		return template([[
langfix.optindex(
	<?=apply(self.expr)?>,
	<?=apply(ast._string(self.key))?>,
<? if self.optassign then
?>	function() return <?=apply(self.optassign)?> end
<? else
?>	nil
<? end
?>)
]],		{
			self = self,
			ast = ast,
			apply = apply,
		})
	end

	-- subclass the original _call, not our new one ...
	ast._optcall = ast._call:subclass()
	ast._optcall.type = 'optcall'
	-- TODO args are evaluated even if short-circuit fails (so it's not a short-ciruit, just an error avoidance)
	function ast._optcall:serialize(apply)
		local func = self.func
		if ast._optindexself:isa(func) then
			-- optcall optindexself
			return template([[
langfix.optcallself(
	<?=apply(func.expr)?>,
	<?=apply(ast._string(func.key))?>,
<? if func.optassign then
?>	function() return <?=apply(func.optassign)?> end
<? else
?>	nil
<? end
?>	<?=table.mapi(self.args, function(arg) return ', '..apply(arg) end):concat()?>
)
]],			{
				self = self,
				func = func,
				ast = ast,
				apply = apply,
				table = table,
			})
			-- indexself key is a Lua string so ... lazy I know
		else
			-- optcall anything else
			-- can optassign go here? does it mean anything?
			return template([[
langfix.optcall(<?=table{func}:append(self.args):mapi(apply):concat','?>)
]], 		{
				self = self,
				func = func,
				apply = apply,
				table = table,
			})
		end
	end

	-- and for optindexself to work, now I have to add exceptions to call...
	local _call = ast._call:subclass()
	ast._call = _call
	function _call:serialize(apply)
		local func = self.func
		if ast._optindexself:isa(func) then
			return template([[
langfix.optcallself(
	<?=apply(func.expr)?>,
	<?=apply(ast._string(func.key))?>,
<? if func.optassign then
?>	function() return <?=apply(func.optassign)?> end
<? else
?>	nil
<? end
?>	<?=table.mapi(self.args, function(arg) return ', '..apply(arg) end):concat()?>
)
]],			{
				self = self,
				func = func,
				apply = apply,
				ast = ast,
				table = table,
			})
		else
			return _call.super.serialize(self, apply)
		end
	end

	-- TODO just give the tokenizer the ast, make the child tokens enumerable, and enum and put them all in self.symbols
	function LuaFixedTokenizer:initSymbolsAndKeywords(...)
		LuaFixedTokenizer.super.initSymbolsAndKeywords(self, ...)

		self.symbols:insert(ast._ashr.op)
		for _,cl in ipairs(assignops) do
			self.symbols:insert(cl.op)
		end

		self.symbols:insert'//'	-- always add idiv symbol

		-- safe-navigation token, pairs with ?. ?: ?[ ?(
		self.symbols:insert'?.'
		self.symbols:insert'?['
		self.symbols:insert'?:'
		self.symbols:insert'?('

		self.symbols:insert'?'	-- ternary

		self.symbols:insert'??'	-- null-coalescence
	end

	-- parse *all* bitwise operators, and for LuaJIT make sure to replace them with bit.xxx function calls.

	function LuaFixedParser.parse(...)
		return LuaFixedParser(...).tree
	end

	function LuaFixedParser:init(data, source)
		self.version = 'Lua 5.4'
		self.useluajit = not not _G.jit

		-- 5.4 means we're going to include 5.2 symbols: ?? ~ & | << >>
		LuaFixedParser.super.init(self, data, self.version, source, self.useluajit)
	end

	function LuaFixedParser:buildTokenizer(data)
		return LuaFixedTokenizer(data, self.version, self.useluajit)
	end

	-- add op= parsing
	function LuaFixedParser:parse_assign(vars, from, ...)
		for _,cl in ipairs(assignops) do
			if self:canbe(cl.op, 'symbol') then
				return cl(vars, assert(self:parse_explist()))
					:setspan{from = from, to = self:getloc()}
			end
		end

		return LuaFixedParser.super.parse_assign(self, vars, from, ...)
	end

	-- copy of LuaParser:parse_prefixexp()
	-- but with ?'s inserted for optcall, optindex, optindexself
	function LuaFixedParser:parse_prefixexp()
		local ast = self.ast
		local prefixexp
		local from = self:getloc()

		if self:canbe('(', 'symbol') then
			local exp = assert(self:parse_exp())
			self:mustbe(')', 'symbol')
			prefixexp = ast._par(exp)
				:setspan{from = from, to = self:getloc()}
		elseif self:canbe(nil, 'name') then
			prefixexp = ast._var(self.lasttoken)
				:setspan{from = from, to = self:getloc()}
		else
			return
		end

		while true do
			local opt = self:canbe('?[', 'symbol')
			if opt or self:canbe('[', 'symbol') then
				local cl = opt and ast._optindex or ast._index
				prefixexp = cl(prefixexp, assert(self:parse_exp()))
				self:mustbe(']', 'symbol')
				prefixexp:setspan{from = from, to = self:getloc()}
			else
				opt = self:canbe('?.', 'symbol')
				if opt or self:canbe('.', 'symbol') then
					local cl = opt and ast._optindex or ast._index
					local sfrom = self:getloc()
					prefixexp = cl(
						prefixexp,
						ast._string(self:mustbe(nil, 'name'))
							:setspan{from = sfrom, to = self:getloc()}
					)
					:setspan{from = from, to = self:getloc()}
				else
					opt = self:canbe('?:', 'symbol')
					if opt or self:canbe(':', 'symbol') then
						local cl = opt and ast._optindexself or ast._indexself
						prefixexp = cl(
							prefixexp,
							self:mustbe(nil, 'name')
						):setspan{from = from, to = self:getloc()}

						-- it'd be nice to handle f?'strings' or f?{tables} just like we can do without ?'s
						-- but if I do that then I have to handle ? as a separate symbol to the indexes
						-- and in doing so it makes index and ternary mix up
						-- (another fix i had for this was changing ternary, but that's pretty established...)
						local args, clcall
						if self:canbe('?(', 'symbol') then
							-- no implicit () with string or table when using safe-navigation
							args = self:parse_explist() or {}
							self:mustbe(')', 'symbol')
							clcall = ast._optcall
						else
							args = self:parse_args()
							clcall = ast._call
						end

						assert(args, "function arguments expected")
						prefixexp = clcall(prefixexp, table.unpack(args))
							:setspan{from = from, to = self:getloc()}
					else
						local args, clcall
						if self:canbe('?(', 'symbol') then
							-- no implicit () with string or table when using safe-navigation
							args = self:parse_explist() or {}
							self:mustbe(')', 'symbol')
							clcall = ast._optcall
						else
							args = self:parse_args()
							if not args then break end
							clcall = ast._call
						end

						prefixexp = clcall(prefixexp, table.unpack(args))
							:setspan{from = from, to = self:getloc()}
					end
				end
			end

-- [[ safe-navigation suffix `:` for optional-assignment to the key if it doesn't exist
-- if you just want optional value, use ternary `? :` / null-coalescence `??`
			if opt and self:canbe('=', 'symbol') then
				if ast._optcall:isa(prefixexp) then
					error("safe-navigation-assign only works after indexes, not calls")
				end
				local exp = self:parse_exp()
				assert(exp, "safe-navigation-assignment ?. : expected an expression")
				prefixexp.optassign = exp
			end
--]]
		end

		return prefixexp
	end

-- [=[ ternary operator
	function LuaFixedParser:parse_exp()
		return self:parse_exp_ternary()	-- typically goes to parse_exp_or ...
	end

	function LuaFixedParser:parse_exp_ternary()
		local ast = self.ast
		local a = self:parse_exp_or()
		if not a then return end

		-- if we get a ( then handle many and expect a )
		-- if we don't then just expect one
		-- same logic as with single-expression lambdas
		local function parseOneOrMany(msg)
			local c
			if self:canbe('(', 'symbol') then
				c = self:parse_explist()
				assert(c, msg)
				self:mustbe(')', 'symbol')
			else
				--c = self:parse_prefixexp()
				--c = self:parse_exp_or()	-- parsing the next-precedence (exp_or) here means you'll have to wrap chained ?:'s in ()'s or else it'll mess up the parsing
				c = self:parse_exp_ternary()
				assert(c, msg)
				c = table{c}
			end
			return c
		end

		--if self:canbe('?', 'symbol') then
		-- TODO can't use ? or it messes with safe-navigation ... or I could change safe-navigation ...
		if self:canbe('?', 'symbol') then
			local b = parseOneOrMany"expected a ? b : c or a ?? c"

			-- should I allow the ternary to not provide an 'else', and it default to nil?
			self:mustbe(':', 'symbol')
			local c = parseOneOrMany"expected a ? b : c or a ?? c"

			a = ast._ternary(a, b, c)
				:setspan{from = a.span.from, to = self:getloc()}
		elseif self:canbe('??', 'symbol') then
			local b
			local c = parseOneOrMany"expected a ?? c"
			a = ast._ternary(a, b, c)
				:setspan{from = a.span.from, to = self:getloc()}
		end
		return a
	end
--]=]

	-- lambdas
	function LuaFixedParser:parse_functiondef()
		local from = self:getloc()
		-- metalua format [args]
		-- but then with a proper function body, none of this single-expression python lambda bullshit
		if self:canbe('[', 'symbol') then

			-- see if there's a : arg to hack in a 'self'
			local args, selffirst
			if self:canbe(':', 'symbol') then
				selffirst = true
				if self:canbe(',', 'symbol') then
					args = self:parse_parlist()
				end
			else
				args = self:parse_parlist()
			end
			args = args or table()
			if selffirst then
				args:insert(1, ast._var'self')
			end

			local lastArg = args:last()
			local functionType = lastArg and lastArg.type == 'vararg' and 'function-vararg' or 'function'
			self:mustbe(']', 'symbol')

			self.functionStack:insert(functionType)

			local block
			if self:canbe('do', 'keyword') then
				block = self:parse_block(functionType)
				self:mustbe('end', 'keyword')
			else
				-- TODO will I run into blockStack issues here, since I'm not pushing/popping it?

				-- maybe I'll say "if there's an initial ( then expect a mult-ret"
				-- so `[x](x+1,x+2,x+3)` is a single-expression that returns 3
				-- however default Lua behavior is that extra () will truncate mult-ret
				-- i.e. `return (x+1, x+2, x+3)` will just return x+1
				-- that would mean, with () for single-expression lambda with multiple-return,
				-- to truncate another multiple-return, you'd have to wrap in *two* sets of (())'s
				-- i.e. `[...]((...))` would return just the first argument of ...
				-- while `[...](...)` would mult-ret all arguments
				-- and `[...](1, ...)` would mult-ret `1` concatenated to all arguments
				-- and `[...]1, ...` would just return `1` and that 2nd `...` would belong to the scope outside the lambda.
				if self:canbe('(', 'symbol') then
					local explist = self:parse_explist()
					assert(explist, "expected expression")
					block = {ast._return(table.unpack(explist))}
					self:mustbe(')', 'symbol')
				else
					-- implicit return of single-expression
					-- should this allow return-single or return-multiple?
					-- i.e. should commas precedence be to include in the expression or should they become outside the function?
					-- outside I think for ambiguity.
					-- though inside would be more flexible ... [x,y,z]x,y,z returns 3 args ...
					--[[ will require parentehsis to wrap
					local exp = self:parse_prefixexp()
					assert(exp, "expected expression")
					block = {ast._return(exp)}
					--]]
					-- [[ won't require parenthesis to wrap
					local exp = self:parse_exp()
					assert(exp, "expected expression")
					block = {ast._return(exp)}
					--]]
					--[[ mult-ret, doesn't require () to wrap the return,
					-- but successive ,'s after will get lumped into the mult-ret
					--  such that it can only be separted from them by wrapping the whole lambda in ()'s
					-- i.e. `[x]x, [x]x` is a single-lambda that returns x and [x]x
					-- (instead of two separate comma-separated expressions)
					local explist = self:parse_explist()
					assert(explist, "expected expression")
					block = {ast._return(table.unpack(explist))}
					--]]
				end
			end

			asserteq(self.functionStack:remove(), functionType)

			return self:makeFunction(nil, args, table.unpack(block))
				:setspan{from = from, to = self:getloc()}
		end
		return LuaFixedParser.super.parse_functiondef(self)
	end

	require 'ext.load'(env).xforms:insert(function(data, source)
		local parser, tree, result
		assert(xpcall(function()
			parser = LuaFixedParser()
			parser:setData(data, source)
			tree = parser.tree
			result = tree:toLua()
--DEBUG:print('\n'..source..'\n'..showcode(result)..'\n')
		end, function(err)
			return '\n'
				--..(source or ('['..data:sub(1,10)..'...]'))..'\n'		-- ext.load already handles this
				--..(data and ('data:\n'..showcode(data)..'\n') or '')	-- ext.load already handles this
	--			..(not tree and parser and parser.t and (' at '..parser.t:getpos()..'\n') or '')	-- parser.base.parser:setData already handles this
	--			..(result and ('result:\n'..showcode(result)..'\n') or '')
				..err..'\n'
				..debug.traceback()
		end))
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
