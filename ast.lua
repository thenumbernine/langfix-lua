local assertindex = require 'ext.assert'.index
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

local function toLuaFixed(x)
	if x.toLuaFixed then return x:toLuaFixed() end
	return x:serialize(toLuaFixed)
end

-- weakness to this design ...i need to always keep specifying the above toLang() wrapper, or I have to make a seprate member function...
for k,cl in pairs(ast) do
	if LuaAST:isa(cl) then
		function cl:toLuaFixed_recursive(apply)
			return self:serialize(apply)
		end
		function cl:toLuaFixed()
			return self:toLuaFixed_recursive(toLuaFixed)
		end
	end
end



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
	function cl:toLuaFixed_recursive(apply)
		return table.mapi(self.vars, apply):concat','
			..' '..op..' '
			..table.mapi(self.exprs, apply):concat','
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
	function ast._idiv:serialize(apply)
		return 'langfix.idiv('..table.mapi(self, apply):concat','..')'
	end
	function ast._idiv:toLuaFixed_recursive(apply)
		return table.mapi(self, apply):concat' // '
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
		local cl = assertindex(ast, key)
		--cl = cl:subclass()	-- TODO fixme
		ast[key] = cl

		-- in this case the original serialize, which is for native bitops, is the LuaFixed serialize
		cl.toLuaFixed_recursive = cl.serialize

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
langfix.ternary(<?=apply(self[1])
?>,<?
if self[2] then
?> function() return <?=commasep(self[2], apply)?> end <?
else
?> nil <?
end
?>,<?
if self[3] then
?> function() return <?=commasep(self[3], apply)?> end <?
else
?> nil <?
end
?>)]],
		{
			self = self,
			apply = apply,
			commasep = commasep,
		})
	end
	function _ternary:toLuaFixed_recursive(apply)
		local function serializeOneOrMany(exprs)
			if exprs == nil then return '' end
			return '('..table.mapi(exprs, apply):concat','..')'
		end
		if self[2] == nil then
			return apply(self[1])
				..' ?? '..serializeOneOrMany(self[3])
		else
			return apply(self[1])
				..' ? '..serializeOneOrMany(self[2])
				..' : '..serializeOneOrMany(self[3])
		end
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
langfix.optindex(<?=
apply(self.expr)
?>,<?=
apply(self.key)
?>, <?
if self.optassign then
?> function() return <?=apply(self.optassign)?> end <?
else
?> nil <?
end ?>)]],
	{
		self = self,
		apply = apply,
	})
end
function ast._optindex:toLuaFixed_recursive(apply)
	return apply(self.expr)
		..'?'
		..(ast.keyIsName(self.key, self.parser)
			and '.'..self.key.value
			or '['..apply(self.key)..']'
		)..(self.optassign and ' = '..apply(self.optassign) or '')
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
langfix.optindex(<?=
apply(self.expr)
?>, <?=
apply(self.parser:node('_string', self.key))
?>, <?
if self.optassign then
?> function() return <?=apply(self.optassign)?> end <?
else
?> nil <?
end
?>)]],
	{
		self = self,
		ast = ast,
		apply = apply,
	})
end
function ast._optindexself:toLuaFixed_recursive(apply)
	return apply(self.expr)..'?:'..self.key
		..(self.optassign and ' = '..apply(self.optassign) or '')
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
langfix.optcallself(<?=
apply(func.expr)
?>,<?=
apply(self.parser:node('_string', func.key))
?>,<?
if func.optassign then
?> function() return <?=apply(func.optassign)?> end <?
else
?> nil <?
end ?> <?=
table.mapi(self.args, function(arg) return ', '..apply(arg) end):concat()
?>)]],
		{
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
function ast._optcall:toLuaFixed_recursive(apply)
	local func = self.func
	return apply(self.func)
		..'?('..table.mapi(self.args, apply):concat','..')'
end

-- and for optindexself to work, now I have to add exceptions to call...
local _call = ast._call:subclass()
ast._call = _call
_call.toLuaFixed_recursive = _call.serialize	-- old serialize = langfix grammar
function _call:serialize(apply)
	local func = self.func
	if ast._optindexself:isa(func) then
		return template([[
langfix.optcallself(<?=
apply(func.expr)
?>, <?=
apply(self.parser:node('_string', func.key))
?>, <?
if func.optassign then
?> function() return <?=apply(func.optassign)?> end <?
else
?> nil <?
end
?> <?=
table.mapi(self.args, function(arg) return ', '..apply(arg) end):concat()
?>)]],
		{
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

--[[
if function is a single-return-statement we can use []expr
if it's a mult-ret single-expression we have to use [](exprs)
if it's a call then there's certain situations (like if it's nested in a ternary operator) that we'll still have to wrap in ( )
if it originally returned wrapped in () to truncate args then yeah definitely we'll have to wrap an additional ()

if the function is multiple-stmts then we can use []do ... end
--]]
function ast._function:toLuaFixed_recursive(apply)
	local s = ''
	if self.name then s = apply(self.name)..' = '..s end
	s = s .. '['..table.mapi(self.args, apply):concat','..']'
	if #self == 1
	and ast._return:isa(self[1])
	then
		local ret = self[1]
		-- TODO when is this necessary due to mult-ret?
		-- when is this necessary due to parent wrapping being ternary or single-expr lambda or something?
		s = s .. '('
		s = s .. table.mapi(ret.exprs, apply):concat','
		s = s .. ')'
	else
		s = s .. 'do '
		s = s .. table.mapi(self, apply):concat' '
		s = s ..' end'
	end
	return s
end

return ast
