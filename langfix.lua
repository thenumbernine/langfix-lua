--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
local table = require 'ext.table'
local asserteq = require 'ext.assert'.eq
local assertindex = require 'ext.assert'.index
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


function LuaFixedTokenizer:initSymbolsAndKeywords(...)
	LuaFixedTokenizer.super.initSymbolsAndKeywords(self, ...)
	
	self.symbols:insert(ast._ashr.op)
	for _,info in ipairs(optoinfos) do
		local name, op = table.unpack(info)
		self.symbols:insert(op)
	end
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

--[[ TODO make a new 'ast' namespace and subclass all former classes into it
local luaast = LuaFixedParser.ast
local ast = {}
for k,v in pairs(luaast) do ast[k] = v end
LuaFixedParser.ast = ast

ast._idiv = ast._idiv:subclass()
ast._band = ast._band:subclass()
ast._bxor = ast._bxor:subclass()
ast._bor = ast._bor:subclass()
ast._shl = ast._shl:subclass()
ast._shr = ast._shr:subclass()

local _idiv = ast._idiv:subclass()
local _band = ast._band:subclass()
local _bxor = ast._bxor:subclass()
local _bor = ast._bor:subclass()
local _shl = ast._shl:subclass()
local _shr = ast._shr:subclass()
--]]
-- [[ just modify the original
LuaFixedParser.ast = ast

local _idiv = ast._idiv
local _band = ast._band
local _bxor = ast._bxor
local _bor = ast._bor
local _shl = ast._shl
local _shr = ast._shr
--]]

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

-- lambdas?
-- might have to reorganize syntax for this ...
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

local function intptrcode(arg)
	return "require'ffi'.cast('intptr_t',"..arg..')'
end

function _idiv:serialzie(apply)
	-- [[ as integers, but requires ffi access ...
	-- parenthesis required?  
	-- I think I got away with not using () to wrap generated-code because of the fact that always the code was generated from sources with correct precedence as it was parsed
	-- so by the fact that the language was specified correctly, so was the AST represented correctly, and so the regenerated code was also correct.
	local args = self.args:mapi(apply):mapi(intptrcode)
	return '('..args:concat'/'..')'
	--]]
	--[[ as floats but with floor ... ?  needs a math.sign or math.trunc function, how easy is that to write without generating anonymous lambdas or temp variables?
	return '((function()'
		..' local x = '..table.mapi(self.args, apply):concat'/'
		..' return x < 0 and math.ceil(x) or math.floor(x)'
		..' end)())'
	--]]
end

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
		local args = {}
		for i=1,#self.args do
			args[i] = apply(self.args[i])
		end
		--[[ ffi.arch bitness, or at least intptr_t's bitness, whatever that is (usu 64-bit for ffi.arch == x64)
		-- upside: always 64-bit, even when luajit bit.band would be 32-bit for Lua-numbers
		-- downside: always 64-bit, even when luajit bit.band would be 32-bit for int32_t's
		args = args:mapi(intptrcode)
		--]] -- or don't and just use luajit builtin bit lib bitness as is (usu 32-bit regardless of ffi.arch for Lua numbers, or 64-bit for boxed types):
		return '(bit.'..func..'('..table.concat(args, ',')..'))'
	end
end

require 'ext.load'.xforms:insert(function(data, source)
	local tree = LuaFixedParser.parse(data, source)
	return tostring(tree)
end)
