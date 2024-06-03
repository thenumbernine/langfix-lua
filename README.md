Transpiled language because I'm lazy.
Transpile to LuaJIT and not C or anything compiled because I still want load() access. (Sorry NeLua)

Biggest pain points of Lua?
- one-based tables.
- global scope by default.
- (LuaJIT) no bit operators.

Features:
- bit operators that get implicitly converted to `bit.*` calls.
	Metamethods for this? but I suspect that will take too much runtime-changes, like testing each argument for a metamethod field, optionally calling, etc, and it would ruin performance.
- shorthand lambdas like in MetaLua or JavaScript.
	`|x,y| do z=x+y return z end`.  The `do` is only there to appease my vim syntax highlighter.
	maybe later I'll add `|x,y| x+y`

Complementing Features (in other libraries):
- https://github.com/thenumbernine/lua-ext `luajit -lext`: default operators for functions, coroutines, etc.
- `luajit -lext.debug` syntax for running things in debug-mode, or maybe even more of this, like types and type-checking upon-load()
- `luajit -lext.ctypes`: C types at global scope. this is an easy optional `require` to vanilla LuaJIT.  I put this in 
- https://github.com/thenumbernine/lua-local-default `luajit -llocal-default` local-by-default, global-by-keyword.  But this just wedges the env-setting into every function, so why not just do that with a transpiler-parser as well? In fact TODO for this is right now it only operates via `require()`.  Would be nice for it to operate via `load()` as well.  Might have to standardize my shim layer of the `require` and `load` functions which are used in this, `ext.debug`, `local-default`, `profiler`, etc.

TODO
- If you replace the globals `load`/`loadstring` then will `require` use your overridden functions, or still use the builtin one? 
- stretch goal: zero-based indexing.  list literals shift their integer keys to initialize to be zero-based.  likewise they invoke a wrapper for a zero-based metamethod object.  `select` also shifts its indexes by 1.
- Maybe some shorthand for array/ptr construction based on the type? i.e. `char` is equivalent to `ffi.typeof'char'` and char:ptr() makes `'char*'` type, and `char:ar(10)` makes `char[10]` type.
	In pure Lua this would mean changing the ctype metatable, which LuaJIT goes way out of their way to mess with (having metatable() return strings, so you have to use debug.metatable() .... why?!?!?!)
