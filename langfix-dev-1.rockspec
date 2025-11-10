package = "langfix"
version = "dev-1"
source = {
	url = "git+https://github.com/thenumbernine/langfix-lua"
}
description = {
	summary = "Pure-Lua fix for some things in the language, especially missing in LuaJIT",
	detailed = [[Pure-Lua fix for some things in the language, especially missing in LuaJIT]],
	homepage = "https://github.com/thenumbernine/langfix-lua",
	license = "MIT"
}
dependencies = {
	"lua >= 5.1"
}
build = {
	type = "builtin",
	modules = {
		["langfix.ast"] = "ast.lua",
		["langfix.env"] = "env.lua",
		["langfix"] = "langfix.lua",
		["langfix.parser"] = "parser.lua",
		["langfix.run"] = "run.lua",
		["langfix.tokenizer"] = "tokenizer.lua"
	},
	copy_directories = {
		"tests"
	}
}
