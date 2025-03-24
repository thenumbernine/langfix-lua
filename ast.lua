local assert = require 'ext.assert'
local table = require 'ext.table'
local template = require 'template'
local LuaParser = require 'parser.lua.parser'

local ast = table(LuaParser.ast)
local LuaAST = ast.node

-- subclass all our AST classes so our modifications don't mess with the original LuaParser
for _,k in ipairs(table.keys(ast)) do
	local cl = ast[k]
	if LuaAST:isa(cl) then
		ast[k] = cl:subclass()
	end
end

-- weakness to this design ...i need to always keep specifying the above toLang() wrapper, or I have to make a seprate member function...
for k,cl in pairs(ast) do
	if LuaAST:isa(cl) then
		function cl:toLuaFixed_recursive(consume)
			return self:serialize(consume)
		end
		function cl:toLuaFixed()
			return self:serializeRecursiveMember'toLuaFixed_recursive'
		end
	end
end

-- I could insert >>> into the symbols and map it to luajit arshift ...
ast._ashr = ast._op:subclass{type='ashr', op='>>>'}

local function consumeforname(name)
	return function(consume,a,b)
		consume(name)
		consume'('
		consume(a)
		consume', '
		consume(b)
		consume')'
	end
end

local assignops = table{
	{'concatto', '..='},
	{'addto', '+='},
	{'subto', '-='},
	{'multo', '*='},
	{'divto', '/='},
	{'idivto', '//=', not native_idiv and consumeforname'langfix.idiv' or nil},
	{'modto', '%='},
	{'powto', '^='},
	{'bandto', '&=', not native_bitops and consumeforname'bit.band' or nil},
	{'borto', '|=', not native_bitops and consumeforname'bit.bor' or nil},
	-- oh wait, that's not-equals ... hmm someone didn't think that choice through ...
	-- coincidentally, xor is sometimes denoted as the not-equivalent symbol, because that's basically what it means, but how to distinguish between boolean and bitwise ...
	-- I would like an xor-equals ... but I also like ^ as power operator ... and I don't like ~= as not-equals, but to change that breaks Lua compatability ...
	{'bxorto', '~~=', not native_bitops and consumeforname'bit.bxor' or '%1 ~ %2'},	-- tempting to use ⊕= and just use ⊕ for xor ...
	{'shlto', '<<=', not native_bitops and consumeforname'bit.lshift' or nil},
	{'shrto', '>>=', not native_bitops and consumeforname'bit.rshift' or nil},
	{'ashrto', '>>>=', not native_bitops and consumeforname'bit.arshift' or nil},
	-- TODO what about and= or= not= ?
}:mapi(function(info)
	local name, op, binopexpr = table.unpack(info)
	binopexpr = binopexpr or function(consume,a,b)
		consume'(('
		consume(a)
		consume(') '..op:sub(1, -2)..' (')
		consume(b)
		consume'))'
	end
	local cl = ast._assign:subclass{type=name, op=op, binopexpr=binopexpr}
	info[3] = cl
	ast['_'..name] = cl
	function cl:serialize(consume)
		for i,v in ipairs(self.vars) do
			consume(v)
			if i < #self.vars then consume',' end
		end
		consume' = '
		for i,v in ipairs(self.vars) do
			consume'('
			binopexpr(consume, v, self.exprs[i])
			consume')'
			if i < #self.vars then consume',' end
		end
	end
	function cl:toLuaFixed_recursive(consume)
		for i,v in ipairs(self.vars) do
			consume(v)
			if i < #self.vars then consume',' end
		end
		consume(' '..op..' ')
		for i,v in ipairs(self.exprs) do
			consume(v)
			if i < #self.exprs then consume',' end
		end
	end

	return cl
end)
ast.assignops = assignops

-- Make a new 'ast' namespace and subclass all former classes into it so that we don't mess with anyone using the base-class
ast._idiv = ast._idiv:subclass()
ast._band = ast._band:subclass()
ast._bxor = ast._bxor:subclass()
ast._bor = ast._bor:subclass()
ast._shl = ast._shl:subclass()
ast._shr = ast._shr:subclass()

if not native_idiv then
	function ast._idiv:serialize(consume)
		consume'langfix.idiv('
		consume(self[1])
		consume','
		consume(self[2])
		consume')'
	end
	function ast._idiv:toLuaFixed_recursive(consume)
		consume(self[1])
		consume' // '
		consume(self[2])
	end
end

-- TODO this detection is done both here and in env.lua
-- I should do the detect in one place
-- but then I should also put the conditional ast class construction in one place too ...
local native_bitops = load'x=y|z'
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
		local cl = assert.index(ast, key)
		--cl = cl:subclass()	-- TODO fixme
		ast[key] = cl

		-- in this case the original serialize, which is for native bitops, is the LuaFixed serialize
		cl.toLuaFixed_recursive = cl.serialize

		-- funny how in lua `function a.b.c:d()` works but `function a.b['c']:d()` doesn't ...
		function cl:serialize(consume)
			--[[ ffi.arch bitness, or at least intptr_t's bitness, whatever that is (usu 64-bit for ffi.arch == x64)
			-- upside: always 64-bit, even when luajit bit.band would be 32-bit for Lua-numbers
			-- downside: always 64-bit, even when luajit bit.band would be 32-bit for int32_t's
			args = args:mapi(intptr_t)
			--]] -- or don't and just use luajit builtin bit lib bitness as is (usu 32-bit regardless of ffi.arch for Lua numbers, or 64-bit for boxed types):
			consume('(bit.'..func..'(')
			for i,arg in ipairs(self) do
				consume(arg)
				if i < #self then consume',' end
			end
			consume'))'
		end
	end
end

local function commasep(exprs, consume)
	for i,arg in ipairs(exprs) do
		consume(arg)
		if i < #exprs then consume',' end
	end
end

local _indexselfscope = ast._indexself:subclass()
_indexselfscope.type = 'indexselfscope'
ast._indexselfscope = _indexselfscope
function _indexselfscope:serialize(consume)
	-- TODO this serialization is for producing names
	-- you also need an extra rule in function-code-generation (for Lua code only, not LangFix code) for inserting the new _ENV=self / setfenv(1,self)
	consume(self.expr)
	consume':'
	consume(self.key)
end
function _indexselfscope:toLuaFixed_recursive(consume)
	-- ... then in LangFix code we spit out :: in the name again
	consume(self.expr)
	consume'::'
	consume(self.key)
end

-- add support for indexselfscope to function definitions ...
ast._function = ast._function:subclass()
function ast._function:serialize(consume)
	consume'function'
	if self.name then
		consume(self.name)
	end
	consume'('
	commasep(self.args, consume)
	consume')'
	if ast._indexselfscope:isa(self.name) then
		if setfenv then
			consume' setfenv(1, self) '
		else
			consume' local _ENV=self '
		end
	end
	for i,x in ipairs(self) do
		consume(x)
	end
	consume'end'
end

local _ternary = ast._op:subclass()
_ternary.type = 'ternary'
ast._ternary = _ternary
function _ternary:serialize(consume)
	consume'langfix.ternary('
	consume(self[1])
	consume','
	consume' function() return '
	commasep(self[2], consume)
	consume' end '
	consume','
	consume' function() return '
	commasep(self[3], consume)
	consume' end '
	consume')'
end
function _ternary:toLuaFixed_recursive(consume)
	local function serializeOneOrMany(exprs)
		if exprs == nil then return end
		consume'('
		for i,x in ipairs(exprs) do
			consume(x)
			if i < #exprs then consume',' end
		end
		consume')'
	end
	consume(self[1])
	consume' ? '
	serializeOneOrMany(self[2])
	consume' : '
	serializeOneOrMany(self[3])
end

local _nilcoalescing = ast._op:subclass()
_nilcoalescing.type = 'nilcoalescing'
ast._nilcoalescing = _nilcoalescing
function _nilcoalescing:serialize(consume)
	consume'langfix.nilcoalescing('
	consume(self[1])
	consume','
	consume' function() return '
	commasep(self[2], consume)
	consume' end '
	consume')'
end
function _nilcoalescing:toLuaFixed_recursive(consume)
	local function serializeOneOrMany(exprs)
		if exprs == nil then return end
		consume'('
		for i,x in ipairs(exprs) do
			consume(x)
			if i < #exprs then consume',' end
		end
		consume')'
	end
	consume(self[1])
	consume' ?? '
	serializeOneOrMany(self[2])
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
function ast._optindex:serialize(consume)
	consume'langfix.optindex('
	consume(self.expr)
	consume','
	consume(self.key)
	consume', '
	if self.optassign then
		consume' function() return '
		consume(self.optassign)
		consume' end '
	else
		consume' nil '
	end
	consume')'
end
function ast._optindex:toLuaFixed_recursive(consume)
	consume(self.expr)
	consume'?'
	if ast.keyIsName(self.key, self.parser) then
		consume('.'..self.key.value)
	else
		consume'['
		consume(self.key)
		consume']'
	end
	if self.optassign then
		consume' = '
		consume(self.optassign)
	end
end

-- ok here's where I start to break things
-- indexself was only valid when a child of call, so call(indexself(t,k)) turned into t:k(), which was language shorthand for t.k(t)
-- call(optindexself(t,'k'), v) is going to turn into 't?:k(v)', turns into 'function(t,k, ...) if t==nil then return end return t[k](t, ...) end)(t, 'k', v)
ast._optindexself = ast._indexself:subclass()
ast._optindexself.type = 'optindexself'
function ast._optindexself:serialize(consume)
	-- this should only ever be placed under a call or optcall, which will handle it themselves
	error('here with parent '..tostring(self.parent.type))
	-- indexself key is a Lua string so don't consume()
	consume'langfix.optindex('
	consume(self.expr)
	consume', '
	consume(self.parser:node('_string', self.key))
	consume', '
	if self.optassign then
		consume' function() return '
		consume(self.optassign)
		consume' end '
	else
		consume' nil '
	end
	consume')'
end
function ast._optindexself:toLuaFixed_recursive(consume)
	consume(self.expr)
	consume'?:'
	consume(self.key)
	if self.optassign then
		consume' = '
		consume(self.optassign)
	end
end

-- subclass the original _call, not our new one ...
ast._optcall = ast._call:subclass()
ast._optcall.type = 'optcall'
-- TODO args are evaluated even if short-circuit fails (so it's not a short-ciruit, just an error avoidance)
function ast._optcall:serialize(consume)
	local func = self.func
	if ast._optindexself:isa(func) then
		-- optcall optindexself
		consume'langfix.optcallself('
		consume(func.expr)
		consume','
		consume(self.parser:node('_string', func.key))
		consume','
		if func.optassign then
			consume' function() return '
			consume(func.optassign)
			consume' end '
		else
			consume' nil '
		end
		consume' '
		for i,arg in ipairs(self.args) do
			consume', '
			consume(arg)
		end
		consume')'
		-- indexself key is a Lua string so ... lazy I know
	else
		-- optcall anything else
		-- can optassign go here? does it mean anything?
		consume'langfix.optcall('
		consume(func)
		for i,arg in ipairs(self.args) do
			consume','
			consume(arg)
		end
		consume')'
	end
end
function ast._optcall:toLuaFixed_recursive(consume)
	local func = self.func
	consume(self.func)
	consume'?('
	for i,arg in ipairs(self.args) do
		consume(arg)
		if i < #self.args then consume',' end
	end
	consume')'
end

-- and for optindexself to work, now I have to add exceptions to call...
local _call = ast._call:subclass()
ast._call = _call
_call.toLuaFixed_recursive = _call.serialize	-- old serialize = langfix grammar
function _call:serialize(consume)
	local func = self.func
	if ast._optindexself:isa(func) then
		consume'langfix.optcallself('
		consume(func.expr)
		consume', '
		consume(self.parser:node('_string', func.key))
		consume', '
		if func.optassign then
			consume' function() return '
			consume(func.optassign)
			consume' end '
		else
			consume' nil '
		end
		consume' '
		for i,arg in ipairs(self.args) do
			consume', '
			consume(arg)
		end
		consume')'
	else
		return _call.super.serialize(self, consume)
	end
end

--[[
if function is a single-return-statement we can use []expr
if it's a mult-ret single-expression we have to use [](exprs)
if it's a call then there's certain situations (like if it's nested in a ternary operator) that we'll still have to wrap in ( )
if it originally returned wrapped in () to truncate args then yeah definitely we'll have to wrap an additional ()

if the function is multiple-stmts then we can use []do ... end
--]]
function ast._function:toLuaFixed_recursive(consume)
	local args = table(self.args)
	local closeplz
	if self.name then
		local name
		if ast._indexself:isa(self.name) then
			name = ast._index(self.name.expr, self.name.key)
			args:insert(1, ast._var':')
		else
			name = self.name
		end
		consume(name)
		consume' = '
	else
		-- if the parent is a table ctor then shorthand lambdas can break the grammar ... unless they are wrapped with parenthesis ...
		consume'('
		closeplz = true
	end
	consume'['
	for i,arg in ipairs(args) do
		consume(arg)
		if i < #args then consume',' end
	end
	consume']'
	if #self == 1
	and ast._return:isa(self[1])
	then
		local ret = self[1]
		-- TODO when is this necessary due to mult-ret?
		-- when is this necessary due to parent wrapping being ternary or single-expr lambda or something?
		consume'('
		for i,expr in ipairs(ret.exprs) do
			consume(expr)
			if i < #ret.exprs then consume',' end
		end
		consume')'
	else
		consume'do '
		for i,expr in ipairs(self) do
			consume(expr)
			if i < #self then consume' ' end
		end
		consume' end'
	end
	if closeplz then
		consume')'
	end
	return s
end

return ast
