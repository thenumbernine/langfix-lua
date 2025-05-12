local LuaTokenizer = require 'parser.lua.tokenizer'

local LuaFixedTokenizer = LuaTokenizer:subclass()

-- TODO just give the tokenizer the ast, make the child tokens enumerable, and enum and put them all in self.symbols
function LuaFixedTokenizer:initSymbolsAndKeywords(...)
	LuaFixedTokenizer.super.initSymbolsAndKeywords(self, ...)

	-- TODO if anyone subclasses this then ast will be wrong
	-- I might as well have parser.base.tokenizer ctor with a pointer to the parser
	-- and then here point to self.parser.ast
	--local ast = self.parser.ast
	local ast = require 'langfix.ast'

	self.symbols:insert(ast._ashr.op)
	for _,cl in ipairs(ast.assignops) do
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

	-- non-nil assertion
	self.symbols:insert'!.'
	self.symbols:insert'!['
	self.symbols:insert'!:'
	self.symbols:insert'!('
end

return LuaFixedTokenizer 
