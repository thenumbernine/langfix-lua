--[[
I wanted to make my grammar-parser-generaor first, but meh
--]]
local showcode = require 'template.showcode'
local LuaParser = require 'parser.lua.parser'

local unpack = _G.unpack or table.unpack

local LuaFixedParser = LuaParser:subclass()

-- parse *all* bitwise operators, and for LuaJIT make sure to replace them with bit.xxx function calls.

function LuaFixedParser.parse(...)
	return LuaFixedParser(...).tree
end

function LuaFixedParser:init(data, source)
	-- 5.4 means we're going to include 5.2 symbols: ?? ~ & | << >>
	LuaFixedParser.super.init(self, data, 'Lua 5.4', source)
	
	-- TODO HERE I could insert <<< into the symbols and map it to luajit arshift ...
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
local ast = LuaFixedParser.ast
LuaFixedParser.ast = ast

local _idiv = ast._idiv
local _band = ast._band
local _bxor = ast._bxor
local _bor = ast._bor
local _shl = ast._shl
local _shr = ast._shr
--]]



-- lambdas?
-- might have to reorganize syntax for this ...
local asserteq = require 'ext.assert'.eq
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
			--local exp = self:parse_prefixexp()	-- will require parentehsis to wrap 
			local exp = self:parse_exp()			-- won't require
			assert(exp, "expected expression")
			block = {ast._return(exp)}
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
} do
	local func = info.func or info.type
	
	-- looks like the luajit bitness of 'bit' using Lua-numbers is 32-bit, even for 64-bit arch builds ...
	-- ... unless the input is a (U)LL-number-literal / (u)int64_t-cast-type
	-- ... in which case, 'rshift' is the Lua-equiv zero-padding, and 'arshift' fills with the lhs-most-bit to preserve sign
	local cl = ast['_'..info.type]
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

-- does xpcall forward args correctly?  not in vanilla lua 5.1, and in luajit without 5.2 compat
local xpcallfwdargs = select(2, xpcall(function(x) return x end, function() end, true))

local oldload = (_VERSION == 'Lua 5.1' and not _G.jit) and loadstring or load

-- ok here's my modified load behavior
-- it's going to parse the lua 5.4 code and spit out the luajit code
local function newload(data, ...)
	-- 5.1 behavior: load(func, name) versus loadstring(data, name)
	-- 5.2..5.4 behavior: load(chunk, name, mode, env)
	-- TODO mind you the formatting on re-converting it will be off ...
	-- errors won't match up ...
	-- so I'll re-insert the generated code
	-- TODO would be nice to save whitespace and re-insert that ... hmm maybe long into the future ...
	-- TODO xpcall behavior testing for when we are allowed to forward the args ... maybe that compat behavior belongs in ext ?
	local source = ...
	if xpcallfwdargs then
		local success, result = xpcall(function(...)
			local tree = LuaFixedParser.parse(data, source)
			local code = tostring(tree)
			return oldload(code, ...)
		end, function(err)
			return --showcode(code)..'\n'..
				err..'\n'
				..debug.traceback()
		end, ...)
		if not success then return nil, result end
		return result
	else
		local args = {...}
		args.n = select('#', ...)
		local success, result = xpcall(function()
			local tree = LuaFixedParser.parse(data, source)
			local code = tostring(tree)
			return oldload(code, unpack(args, 1, args.n))
		end, function(err)
			return showcode(code)..'\n'
				..err..'\n'
				..debug.traceback()
		end)
		if not success then return nil, result end
		return result
	end
end

-- override global load() function, and maybe loadfile() if it's present too
-- (maybe loadstring() too ?)
if _G.loadstring ~= nil then _G.loadstring = newload end
-- TODO if we're in luajit (_VERSION=Lua 5.1) then load() will handle strings, but if we're in lua 5.1 then it will only handle functions (according to docs?) right?
_G.load = newload

local function newloadfile(filename, ...)
	local f, err = io.open(filename, 'r')
	if not f then return nil, err end
	local data, err = f:read'*a'
	f:close()
	if err then return nil, err end

	return newload(data, filename, ...)
end
_G.loadfile = newloadfile

-- next TODO here , same as ext.debug (consider making modular)
-- ... wedge in new package.seachers[2]/package.loaders[2] behavior to use my modified load()
local searchers = assert(package.searchers or package.loaders, "couldn't find searchers")
local oldsearchfile = searchers[2]
local function newsearchfile(req, ...)
	local filename, err = package.searchpath(req, package.path)
	if not filename then return err end

	local f, err = io.open(filename, 'r')
	if not f then return err end
	local d, err = f:read'*a'
	f:close()
	if err then return err end

	local f, err = newload(d, filename)
	return f or err
end
searchers[2] = newsearchfile
