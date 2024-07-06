--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
return function(env)
	local table = require 'ext.table'
	local asserteq = require 'ext.assert'.eq
	local assertindex = require 'ext.assert'.index
	local showcode = require 'template.showcode'
	local LuaParser = require 'parser.lua.parser'
	local LuaTokenizer = require 'parser.lua.tokenizer'


	-- set globals here
	env = env or _G
	env.ffi = require 'ffi'

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
		{'idivto', '//='},
		{'modto', '%='},
		{'powto', '^='},
		{'bandto', '&=', 'bit.band(%1, %2)'},
		{'borto', '|=', 'bit.bor(%1, %2)'},
		-- oh wait, that's not-equals ... hmm someone didn't think that choice through ...
		-- coincidentally, xor is sometimes denoted as the not-equivalent symbol, because that's basically what it means, but how to distinguish between boolean and bitwise ...
		-- I would like an xor-equals ... but I also like ^ as power operator ... and I don't like ~= as not-equals, but to change that breaks Lua compatability ...
		{'bxorto', '~~=', 'bit.bxor(%1, %2)'},	-- tempting to use ⊕= and just use ⊕ for xor ...
		{'shlto', '<<=', 'bit.lshift(%1, %2)'},
		{'shrto', '>>=', 'bit.rshift(%1, %2)'},
		{'ashrto', '>>>=', 'bit.arshift(%1, %2)'},
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

	local function intptrcode(arg)
		return "ffi.cast('intptr_t',"..arg..')'
	end

	if not load'x=y//z' then
		function ast._idiv:serialzie(apply)
			-- [[ as integers, but requires ffi access ...
			-- parenthesis required?
			-- I think I got away with not using () to wrap generated-code because of the fact that always the code was generated from sources with correct precedence as it was parsed
			-- so by the fact that the language was specified correctly, so was the AST represented correctly, and so the regenerated code was also correct.
			local args = table.mapi(self, apply):mapi(intptrcode)
			return '('..args:concat'/'..')'
			--]]
			--[[ as floats but with floor ... ?  needs a math.sign or math.trunc function, how easy is that to write without generating anonymous lambdas or temp variables?
			return '((function()'
				..' local x = '..args:concat'/'
				..' return x < 0 and math.ceil(x) or math.floor(x)'
				..' end)())'
			--]]
		end
	end

	-- don't override these if we're running in pure-Lua (except arshift, which will need its own implementation somewhere ... TODO)
	if not load'x=y|z' then
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
				args = args:mapi(intptrcode)
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
			local a, b, c = table.unpack(self, 1, 3)
			return [[
(function(t)
	if t then
		return ]]..(b and commasep(b, apply) or 't')..[[
	else
		return ]]..(c and commasep(c, apply) or '')..[[
	end
end)(]]..apply(a)..[[)
]]
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
		return [[
(function(t, k)
	if t == nil then
		return nil
	end
	local v = t[k]
]]..(self.optassign and ([[
	if v == nil then
		v = ]]..self.optassign..[[
		t[k] = v
	end
]]) or '')..[[
	return v
end)(]]..apply(self.expr)..','..apply(self.key)..')'
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
		return [[
(function(t, k)
	if t == nil then
		return nil
	end
	local v = t[k]
]]..(self.optassign and ([[
	if v == nil then
		v = ]]..self.optassign..[[
		t[k] = v
	end
]]) or '')..[[
	return v
end)(]]..apply(self.expr)..','..ast._string(self.key)..')'
	end

	-- subclass the original _call, not our new one ...
	ast._optcall = ast._call:subclass()
	ast._optcall.type = 'optcall'
	-- TODO args are evaluated even if short-circuit fails (so it's not a short-ciruit, just an error avoidance)
	function ast._optcall:serialize(apply)
		local func = self.func
		if ast._optindexself:isa(func) then
			-- optcall optindexself
			return [[
(function(t, k, ...)
	if t == nil then
		return nil
	end
	local v = t[k]
	if v == nil then
]]..(func.optassign and ([[
		v = ]]..func.optassign..[[
		t[k] = v
	]]) or 'return nil')..[[
	end
	return v(t, ...)
end)(]]..table{func.expr, ast._string(func.key)}:append(self.args):mapi(apply):concat','..')'
			-- indexself key is a Lua string so ... lazy I know
		else
			-- optcall anything else
			-- can optassign go here? does it mean anything?
			return [[
(function(v, ...)
	if v == nil then
		return nil
	end
	return v(...)
end)(]]..table{func}:append(self.args):mapi(apply):concat','..')'
		end
	end

	-- and for optindexself to work, now I have to add exceptions to call...
	local _call = ast._call:subclass()
	ast._call = _call
	function _call:serialize(apply)
		local func = self.func
		if ast._optindexself:isa(func) then
			return [[
(function(t, k, ...)
	if t == nil then
		return nil
	end
	local v = t[k]
	if v == nil then
]]..(func.optassign and ([[
		v = ]]..func.optassign..[[
		t[k] = v
	]]) or '')	-- TODO if v is nil then ... bail out early?
..[[
	end
	return v(t, ...)
end)(]]..table{func.expr, ast._string(func.key)}:append(self.args):mapi(apply):concat','..')'
		else
			return _call.super.serialize(self, apply)
		end
	end

	function LuaFixedTokenizer:initSymbolsAndKeywords(...)
		LuaFixedTokenizer.super.initSymbolsAndKeywords(self, ...)

		self.symbols:insert(ast._ashr.op)
		for _,cl in ipairs(assignops) do
			self.symbols:insert(cl.op)
		end

		self.symbols:insert'?'	-- safe-navigation token, pairs with ?. ?: ?[ ?(
		self.symbols:insert'??'	-- I'm using this for ternary / elvis / null-coalescence
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
			local opt = self:canbe('?', 'symbol')
			if self:canbe('[', 'symbol') then
				local cl = opt and ast._optindex or ast._index
				prefixexp = cl(prefixexp, assert(self:parse_exp()))
				self:mustbe(']', 'symbol')
				prefixexp:setspan{from = from, to = self:getloc()}
			elseif self:canbe('.', 'symbol') then
				local cl = opt and ast._optindex or ast._index
				local sfrom = self:getloc()
				prefixexp = cl(
					prefixexp,
					ast._string(self:mustbe(nil, 'name'))
						:setspan{from = sfrom, to = self:getloc()}
				)
				:setspan{from = from, to = self:getloc()}
			elseif self:canbe(':', 'symbol') then
				local cl = opt and ast._optindexself or ast._indexself
				prefixexp = cl(
					prefixexp,
					self:mustbe(nil, 'name')
				):setspan{from = from, to = self:getloc()}

				local clcall = self:canbe('?', 'symbol') and ast._optcall or ast._call
				local args = self:parse_args()
				if not args then error"function arguments expected" end
				prefixexp = clcall(prefixexp, table.unpack(args))
					:setspan{from = from, to = self:getloc()}
			else
				local args = self:parse_args()
				if not args then
					if opt then
						error("expected . [ : or () after ? safe-navigator")
						-- TODO rewind one token and keep looking, to parse it as a ternary operator?
						-- or is rewinding a dangerous thing?
						--return prefixexp
					end
					break
				end

				local clcall = opt and ast._optcall or ast._call
				prefixexp = clcall(prefixexp, table.unpack(args))
					:setspan{from = from, to = self:getloc()}
			end

-- [[ safe-navigation suffix `:` for optional-assignment to the key if it doesn't exist
-- if you just want optional value, use ternary / null-coalescence `??:`
			if opt and self:canbe(':', 'symbol') then
				if ast._optcall:isa(prefixexp) then
					error("safe-navigation-assign only works after indexes, not calls")
				end
				local exp = self:parse_exp()
				assert(exp, "safe-navigation-assignment ? : expected an expression")
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
		--if self:canbe('?', 'symbol') then
		-- TODO can't use ? or it messes with safe-navigation ... or I could change safe-navigation ...
		if self:canbe('??', 'symbol') then

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
					c = self:parse_exp_or()
					assert(c, msg)
					c = table{c}
				end
				return c
			end

			local b, c
			if self:canbe(':', 'symbol') then
				-- skip 'b':
				c = parseOneOrMany"expected a ??: c"
			else
				--local b = self:parse_exp_or()
				b = parseOneOrMany"expected a ?? b : c or a ??: c"

				-- should I allow the ternary to not provide an 'else', and it default to nil?
				if self:canbe(':', 'symbol') then
					c = parseOneOrMany"expected a ?? b : c or a ??: c"
				end
			end

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
end
