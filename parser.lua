local assert = require 'ext.assert'
local table = require 'ext.table'
local LuaParser = require 'parser.lua.parser'
local LuaFixedTokenizer = require 'langfix.tokenizer'

local LuaFixedParser = LuaParser:subclass()

LuaFixedParser.ast = require 'langfix.ast'

-- parse *all* bitwise operators, and for LuaJIT make sure to replace them with bit.xxx function calls.

function LuaFixedParser.parse(...)
	return LuaFixedParser(...).tree
end

function LuaFixedParser:init(data, source)
	self.version = 'Lua 5.4'
	self.useluajit = not not _G.jit

	-- 5.4 means we're going to include 5.2 symbols: ?? ~ & | << >>
	LuaFixedParser.super.init(self, data, self.version, source, self.useluajit)

	local shiftIndex, shift = self.parseExprPrecedenceRulesAndClassNames:find(nil, function(level) return level.name == 'shift' end)
	table.insert(shift.rules, {token='>>>', className='_ashr'})
end

function LuaFixedParser:buildTokenizer(data)
	return LuaFixedTokenizer(data, self.version, self.useluajit)
end

-- add op= parsing
function LuaFixedParser:parse_assign(vars, from, ...)
	local ast = self.ast
	for _,cl in ipairs(ast.assignops) do
		if self:canbe(cl.op, 'symbol') then
			return cl(vars, (assert(self:parse_explist(), {msg='expected expr list'})))
				:setspan{from = from, to = self:getloc()}
		end
	end

	return LuaFixedParser.super.parse_assign(self, vars, from, ...)
end

--[[ I thought I could use the superclass function ...
function LuaParser:parse_funcname()
	local from = self:getloc()
	local name = LuaFixedParser.super.parse_funcname(self)
	if not name then return end

	-- if we already got a ':' then don't check for '::'
	if not ast._indexself:isa(name) then
		if self:canbe('::', 'symbol') then
			name = self:node('_indexselfscope', name, self:mustbe(nil, 'name'))
				:setspan{from = from, to = self:getloc()}
		end
	end

	return name
end
--]]
-- [[
function LuaFixedParser:parse_funcname()
	if not self:canbe(nil, 'name') then return end
	local from = self:getloc()
	local name = self:node('_var', self.lasttoken)
		:setspan{from = from, to = self:getloc()}
	while self:canbe('.', 'symbol') do
		local sfrom = self.t:getloc()
		name = self:node('_index',
			name,
			self:node('_string', self:mustbe(nil, 'name'))
				:setspan{from = sfrom, to = self:getloc()}
		):setspan{from = from, to = self:getloc()}
	end
	if self:canbe(':', 'symbol') then
		name = self:node('_indexself', name, self:mustbe(nil, 'name'))
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe('::', 'symbol') then
		name = self:node('_indexselfscope', name, self:mustbe(nil, 'name'))
			:setspan{from = from, to = self:getloc()}
	end
	return name
end
--]]

-- copy of LuaParser:parse_prefixexp()
-- but with ?'s inserted for optcall, optindex, optindexself
function LuaFixedParser:parse_prefixexp()
	local ast = self.ast
	local prefixexp
	local from = self:getloc()

	if self:canbe('(', 'symbol') then
		local exp = assert(self:parse_exp(), {msg='expected expr'})
		self:mustbe(')', 'symbol')
		prefixexp = self:node('_par', exp)
			:setspan{from = from, to = self:getloc()}
	elseif self:canbe(nil, 'name') then
		prefixexp = self:node('_var', self.lasttoken)
			:setspan{from = from, to = self:getloc()}
	else
		return
	end

	while true do
		local opt = self:canbe('?[', 'symbol')
			or self:canbe('![', 'symbol')
		if opt or self:canbe('[', 'symbol') then
			local classname =
				opt == '?[' and '_optindex'
				or opt == '![' and '_assertindex'
				or '_index'
			prefixexp = self:node(classname, prefixexp, (assert(self:parse_exp(), {msg='expected expr'})))
			self:mustbe(']', 'symbol')
			prefixexp:setspan{from = from, to = self:getloc()}
		else
			opt = self:canbe('?.', 'symbol')
				or self:canbe('!.', 'symbol')
			if opt or self:canbe('.', 'symbol') then
				local classname =
					opt == '?.' and '_optindex'
					or opt == '!.' and '_assertindex'
					or '_index'
				local sfrom = self:getloc()
				prefixexp = self:node(
					classname,
					prefixexp,
					self:node('_string', self:mustbe(nil, 'name'))
						:setspan{from = sfrom, to = self:getloc()}
				)
				:setspan{from = from, to = self:getloc()}
			else
				opt = self:canbe('?:', 'symbol')
					or self:canbe('!:', 'symbol')
				if opt or self:canbe(':', 'symbol') then
					local classname =
						opt == '?:' and '_optindexself'
						or opt == '!:' and '_assertindexself'
						or '_indexself'
					prefixexp = self:node(
						classname,
						prefixexp,
						self:mustbe(nil, 'name')
					):setspan{from = from, to = self:getloc()}

					-- it'd be nice to handle f?'strings' or f?{tables} just like we can do without ?'s
					-- but if I do that then I have to handle ? as a separate symbol to the indexes
					-- and in doing so it makes index and ternary mix up
					-- (another fix i had for this was changing ternary, but that's pretty established...)
					local args, callClassName
					if self:canbe('?(', 'symbol') then
						-- no implicit () with string or table when using safe-navigation
						args = self:parse_explist() or {}
						self:mustbe(')', 'symbol')
						callClassName = '_optcall'
					elseif self:canbe('!(', 'symbol') then
						args = self:parse_explist() or {}
						self:mustbe(')', 'symbol')
						callClassName = '_assertcall'
					else
						args = self:parse_args()
						callClassName = '_call'
					end

					assert(args, {msg="function arguments expected"})
					prefixexp = self:node(callClassName, prefixexp, table.unpack(args))
						:setspan{from = from, to = self:getloc()}
				else
					local args, callClassName
					if self:canbe('?(', 'symbol') then
						-- no implicit () with string or table when using safe-navigation
						args = self:parse_explist() or {}
						self:mustbe(')', 'symbol')
						callClassName = '_optcall'
					elseif self:canbe('!(', 'symbol') then
						-- no implicit () with string or table when using safe-navigation
						args = self:parse_explist() or {}
						self:mustbe(')', 'symbol')
						callClassName = '_assertcall'
					else
						args = self:parse_args()
						if not args then break end
						callClassName = '_call'
					end

					prefixexp = self:node(callClassName, prefixexp, table.unpack(args))
						:setspan{from = from, to = self:getloc()}
				end
			end
		end

-- [[ safe-navigation suffix `=` for optional-assignment to the key if it doesn't exist
-- if you just want optional value, use ternary `? :` / null-coalescence `??`
		if opt
		and self:canbe('=', 'symbol')
		then
			if opt:find'^%?' then	-- if it starts with ?, i.e. its an optional-index node
				if ast._optcall:isa(prefixexp) then
					error{msg="safe-navigation-assign only works after indexes, not calls"}
				end
				local exp = self:parse_exp()
				assert(exp, {msg="safe-navigation-assignment ?. = expected an expression"})
				prefixexp.optassign = exp
			elseif opt:find'^!' then
				if ast._assertcall:isa(prefixexp) then
					error{msg="non-nil-assert-assign only works after indexes, not calls"}
				end
				local exp = self:parse_exp()
				assert(exp, {msg="non-nil-assert-assignment !. = expected an expression"})
				prefixexp.assertassign = exp
			else
				error'FIXME'
			end
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
	local from = self:getloc()
	local a = self:parse_expr_precedenceTable(1)
	if not a then return end
	a:setspan{from = from, to = self:getloc()}

	-- if we get a ( then handle many and expect a )
	-- if we don't then just expect one
	-- same logic as with single-expression lambdas
	local function parseOneOrMany(msg)
		local c
		if self:canbe('(', 'symbol') then
			c = self:parse_explist()
			assert(c, {msg=msg})
			self:mustbe(')', 'symbol')
		else
			--c = self:parse_prefixexp()
			--c = self:parse_expr_precedenceTable(1)	-- parsing the next-precedence (exp_or) here means you'll have to wrap chained ?:'s in ()'s or else it'll mess up the parsing
			c = self:parse_exp_ternary()
			assert(c, {msg=msg})
			c = table{c}
		end
		return c
	end

	if self:canbe('?', 'symbol') then
		local b = parseOneOrMany"expected a ? b : c"
		self:mustbe(':', 'symbol')
		local c = parseOneOrMany"expected a ? b : c"

		a = self:node('_ternary', a, b, c)
			:setspan{from = a.span.from, to = self:getloc()}
	elseif self:canbe('??', 'symbol') then
		local b = parseOneOrMany"expected a ?? b"
		a = self:node('_nilcoalescing', a, b)
			:setspan{from = a.span.from, to = self:getloc()}
	end
	return a
end
--]=]

-- lambdas
function LuaFixedParser:parse_functiondef()
	local from = self:getloc()
	-- metalua format |args|
	-- but then with a proper function body, none of this single-expression python lambda bullshit
	if self:canbe('|', 'symbol') then

		-- see if there's a : arg to hack in a 'self'
		local args, selffirst
		if self:canbe(':', 'symbol') then
			selffirst = 'indexself'
			if self:canbe(',', 'symbol') then
				args = self:parse_parlist()
			end
		elseif self:canbe('::', 'symbol') then
			selffirst = 'indexselfscope'
			if self:canbe(',', 'symbol') then
				args = self:parse_parlist()
			end
		else
			args = self:parse_parlist()
		end
		args = args or table()
		if selffirst then
			args:insert(1, self:node('_var', 'self'))
		end

		local lastArg = args:last()
		local functionType = lastArg and lastArg.type == 'vararg' and 'function-vararg' or 'function'
		self:mustbe('|', 'symbol')

		self.functionStack:insert(functionType)

		local block
		if self:canbe('do', 'keyword') then
			block = self:parse_block(functionType)
			self:mustbe('end', 'keyword', 'do', from)
		else
			-- TODO will I run into blockStack issues here, since I'm not pushing/popping it?

			-- maybe I'll say "if there's an initial ( then expect a mult-ret"
			-- so `|x|(x+1,x+2,x+3)` is a single-expression that returns 3
			-- however default Lua behavior is that extra () will truncate mult-ret
			-- i.e. `return (x+1, x+2, x+3)` will just return x+1
			-- that would mean, with () for single-expression lambda with multiple-return,
			-- to truncate another multiple-return, you'd have to wrap in *two* sets of (())'s
			-- i.e. `|...|((...))` would return just the first argument of ...
			-- while `|...|(...)` would mult-ret all arguments
			-- and `|...|(1, ...)` would mult-ret `1` concatenated to all arguments
			-- and `|...|1, ...` would just return `1` and that 2nd `...` would belong to the scope outside the lambda.
			if self:canbe('(', 'symbol') then
				local explist = self:parse_explist()
				assert(explist, {msg="expected expression"})
				block = {self:node('_return', table.unpack(explist))}
				self:mustbe(')', 'symbol')
			else
				-- implicit return of single-expression
				-- should this allow return-single or return-multiple?
				-- i.e. should commas precedence be to include in the expression or should they become outside the function?
				-- outside I think for ambiguity.
				-- though inside would be more flexible ... |x,y,z|x,y,z returns 3 args ...
				--[[ will require parentehsis to wrap
				local exp = self:parse_prefixexp()
				assert(exp, {msg="expected expression"})
				block = {self:node('_return', exp)}
				--]]
				-- [[ won't require parenthesis to wrap
				local exp = self:parse_exp()
				assert(exp, {msg="expected expression"})
				block = {self:node('_return', exp)}
				--]]
				--[[ mult-ret, doesn't require () to wrap the return,
				-- but successive ,'s after will get lumped into the mult-ret
				--  such that it can only be separted from them by wrapping the whole lambda in ()'s
				-- i.e. `|x|x, |x|x` is a single-lambda that returns x and |x|x
				-- (instead of two separate comma-separated expressions)
				local explist = self:parse_explist()
				assert(explist, {msg="expected expression"})
				block = {self:node('_return', table.unpack(explist))}
				--]]
			end
		end

		assert.eq(self.functionStack:remove(), functionType)

		-- TODO FIXME this will generate index-self-scope Lua code corretly, but it won't allow for regenerating langfix code ...
		-- how to get around this?  other than a subclassed _function and subclassed makeFunction ...
		if selffirst == 'indexselfscope' then
			if setfenv then
				table.insert(block, 1,
					self:node('_call',
						self:node('_var', 'setfenv'),
						self:node('_number', '1'),
						self:node('_var', 'self')
					)
				)
			else
				table.insert(block, 1,
					self:node('_local', {
						self:node('_assign',
							{self:node('_var', '_ENV')},
							{self:node('_var', 'self')}
						)
					})
				)
			end
		end

		return self:makeFunction(nil, args, table.unpack(block))
			:setspan{from = from, to = self:getloc()}
	end
	return LuaFixedParser.super.parse_functiondef(self)
end

--[[ the original LuaParser:parse_field function works fine, except that lambdas will get confused with key-expressions unless you wrap them in parentehsis
-- to get around this ...
function LuaFixedParser:parse_field()
	local from = self:getloc()
	if self:canbe('[', 'symbol') then

		-- TODO to handle both lambdas or expression-keys
		-- this will have to be either parse_exp or parse_parlist or also can have : or :: shorthands ...

		-- TODO here look for : or :: ...
		-- if you get it then we're doing a lambda
		-- otherwise ... look for parse_exp()
		-- if the expression that comes back is a single var name then we could be doing either lambda or key-exp
		-- if, after the expression, there's a comma then we must be doing a lambda (and the first expression better be a name / first entry in parse_parlist()
		-- after the expression/parlist, expect ]
		-- then if we find an = then it's a key-exp (and assert there was no : :: or parlist)
		-- and if we didn't find an = then it's a lambda (and assert that if we did parse an expression as the key that it was just a single name)

		local keyexp = assert(self:parse_exp(), {msg='unexpected symbol'})
		self:mustbe(']', 'symbol')

		if self:canbe('=', 'symbol') then
			local valexp = self:parse_exp()
			if not valexp then error{msg="expected expression but found "..tostring(self.t.token)} end
			return self:node('_assign', {keyexp}, {valexp})
				:setspan{from = from, to = self:getloc()}
		else
			-- not an equals?  maybe it's a lambda
		end
	end

	local exp = self:parse_exp()
	if not exp then return end

	if self.ast._var:isa(exp) and self:canbe('=', 'symbol') then
		return self:node('_assign',
			{
				self:node('_string', exp.name):setspan(exp.span)
			}, {
				(assert(self:parse_exp(), {msg='unexpected symbol'}))
			}
		):setspan{from = from, to = self:getloc()}
	else
		return exp
	end
end
--]]

return LuaFixedParser
