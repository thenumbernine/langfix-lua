--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
local table = require 'ext.table'
local asserteq = require 'ext.assert'.eq
local assertindex = require 'ext.assert'.index
local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'
local LuaTokenizer = require 'parser.lua.tokenizer'

local LuaFixedTokenizer = LuaTokenizer:subclass()

local LuaFixedParser = LuaParser:subclass()

LuaFixedParser.ast = table(LuaFixedParser.ast)
local ast = LuaFixedParser.ast

-- I could insert >>> into the symbols and map it to luajit arshift ...
ast._ashr = ast._op:subclass{type='ashr', op='>>>'}

local optoinfos = {
	{'concatto', '..='},
	{'addto', '+='},
	{'subto', '-='},
	{'multo', '*='},
	{'divto', '/='},
	{'idivto', '//='},
	{'modto', '%='},
	{'powto', '^='},
	{'bandto', '&='},
	{'borto', '|='},
	{'shlto', '<<='},
	{'shrto', '>>='},
	{'ashrto', '>>>='},
}

for _,info in ipairs(optoinfos) do
	local name, op = table.unpack(info)
	local cl = ast._assign:subclass{type=name, op=op}
	ast['_'..name] = cl
	function cl:serialize(apply)
		local vars = table.mapi(self.vars, apply)
		local exprs = table.mapi(self.exprs, apply)
		return vars:concat','..' = '..vars:mapi(function(v,i)
			return '('..v..' '..op:sub(1,#op-1)..' '..exprs[i]..')'
		end):concat','
	end
end

-- Make a new 'ast' namespace and subclass all former classes into it so that we don't mess with anyone using the base-class
ast._idiv = ast._idiv:subclass()
ast._band = ast._band:subclass()
ast._bxor = ast._bxor:subclass()
ast._bor = ast._bor:subclass()
ast._shl = ast._shl:subclass()
ast._shr = ast._shr:subclass()

local function intptrcode(arg)
	return "require'ffi'.cast('intptr_t',"..arg..')'
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


--[[
how should a?.b be implemented?
already Lua supports nil-as-undefined, so a.b is indistinguishable from a?.b
(except in LuaJIT with cdata ... which throws exceptions when indexes are missing ... frustrating)
the real operation of a?.b is to stop chaining for *successive* indexes, i.e. a?.b?.c

... as an expression ...
a.b.c
becomes
index(index(var'a', string'b'), string'c')
becomes
a.b.c

so
a?.b?.c
becomes
optindex(optindex(var'a', string'b'), string'c')
becomes ...

(function(x)
	if x == nil then return nil end
	return (function(x)
		if x == nil then return nil end
		return x.c
	end)(x.b)
end)(a)

a.b.c()
becomes
call(index(index(var'a', string'b'), string'c'))

so
a?.b?.c()
becomes
call(optindex(optindex(var'a', string'b'), string'c'))
... but then the parent 'call' code needs know about its child ...
... or does it, since this is a call irregardless ... is the bailout per-expression?
that means generating a statement needs to search through the stmt tree to find any opts
and if they exist, wrap the expr in (function() end)()

(function(x)							-- a?.
	if x == nil then return nil end		-- a?.

	return (function(x)					-- b?.
		if x == nil then return nil end	-- b?.

		return x.c()					-- c() ... inner-most expression is a index-then-call operator, not-optional
										-- this can error if c is nil

	end)(x.b)							-- b?.

end)(a)									-- a?.

waiit
what is being short-circuited here?
the single . index operator? or the entire expression?
just the single . operator i.g. otherwise `a?.b + a.c` could short-circuit the whole thing upon fail ...
... or is that a good thing?

nahh i think i need an optcall to go along with my optindex

a?.b?()
becomes
(function(x)
	if x == nil then return nil end
	(function(x)
		if x == nil then return nil end
		return x()
	end)(x.b)
end)(a)


ok this is only for chains of call+indexself+index
so yeah I do need optcall+optindexself+optindex
and when generating the code, I have to assert that all these nodes have only 1 child, and I need to generate the code in opposite order of any hierarchies with opt nodes in them.


I need to look for ?'s before ['s and .'s in parse_prefixexp ... optindex
I need to look for ? before : in parse_prefixexp also ...  optindexself
... and before parse_args in parse_prefixexp .... in which case, return optcall

... and all of this on rhs only?
... or shhould it eval on lhs as well as full-on stmt-short-circuit?  a?.b = c?.d bails fully if a is nil -- no errors?
--]]
ast._optcall = ast._call:subclass()
ast._optcall.type = 'optcall'

ast._optindex = ast._index:subclass()
ast._optindex.type = 'optindex'

ast._optindexself = ast._indexself:subclass()
ast._optindexself.type = 'optindexself'

-- TODO serializatio of these


function LuaFixedTokenizer:initSymbolsAndKeywords(...)
	LuaFixedTokenizer.super.initSymbolsAndKeywords(self, ...)

	self.symbols:insert(ast._ashr.op)
	for _,info in ipairs(optoinfos) do
		local name, op = table.unpack(info)
		self.symbols:insert(op)
	end

	self.symbols:insert'?'	-- optional token, pairs with ?. ?: ?[ ?(
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
	if self:canbe('..=', 'symbol') then
		return ast._concatto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('+=', 'symbol') then
		return ast._addto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('-=', 'symbol') then
		return ast._subto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('-=', 'symbol') then
		return ast._subto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('*=', 'symbol') then
		return ast._multo(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('/=', 'symbol') then
		return ast._divto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('//=', 'symbol') then
		return ast._idivto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('%=', 'symbol') then
		return ast._modto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('^=', 'symbol') then
		return ast._powto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('&=', 'symbol') then
		return ast._bandto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('|=', 'symbol') then
		return ast._borto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('<<=', 'symbol') then
		return ast._shlto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('>>=', 'symbol') then
		return ast._shrto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('>>>=', 'symbol') then
		return ast._ashrto(vars, assert(self:parse_explist()))
			:setspan{from = from, to = self:getloc()}
	else
		return LuaFixedParser.super.parse_assign(self, vars, from, ...)
	end
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
			if not args then break end

			local clcall = opt and ast._optcall or ast._call
			prefixexp = clcall(prefixexp, table.unpack(args))
				:setspan{from = from, to = self:getloc()}
		end
	end

	return prefixexp
end

-- lambdas
function LuaFixedParser:parse_functiondef()
	local from = self:getloc()
	-- metalua format |args|
	-- but then with a proper function body, none of this single-expression python lambda bullshit
	if self:canbe('|', 'symbol') then

		local args = self:parse_parlist() or table()
		local lastArg = args:last()
		local functionType = lastArg and lastArg.type == 'vararg' and 'function-vararg' or 'function'
		self:mustbe('|', 'symbol')

		local block
		if self:canbe('do', 'keyword') then
			self.functionStack:insert(functionType)
			block = self:parse_block(functionType)
			asserteq(self.functionStack:remove(), functionType)
			self:mustbe('end', 'keyword')
		else
			-- implicit return of single-expression
			-- should this allow return-single or return-multiple?
			-- i.e. should commas precedence be to include in the expression or should they become outside the function?
			-- outside I think for ambiguity.
			-- though inside would be more flexible ... |x,y,z|x,y,z returns 3 args ...
			--[[ will require parentehsis to wrap
			local exp = self:parse_prefixexp()
			assert(exp, "expected expression")
			block = {ast._return(exp)}
			--]]
			--[[ won't require parenthesis to wrap
			local exp = self:parse_exp()
			assert(exp, "expected expression")
			block = {ast._return(exp)}
			--]]
			-- [[
			local explist = self:parse_explist()			-- won't require
			assert(explist, "expected expression")
			block = {ast._return(table.unpack(explist))}
			--]]
		end

		return self:makeFunction(nil, args, table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	end
	return LuaFixedParser.super.parse_functiondef(self)
end

require 'ext.load'.xforms:insert(function(data, source)
	local result
	assert(xpcall(function()
		local tree = LuaFixedParser.parse(data, source)
		result = tree:toLua()
--DEBUG: print('\n'..source..'\n'..showcode(result)..'\n')
	end, function(err)
		return (source or '[]')..'\n'
			..(data and ('data:\n'..showcode(data)..'\n') or '')
			..(result and ('result:\n'..showcode(result)..'\n') or '')
			..err..'\n'
			..debug.traceback()
	end))
	return result
end)
