Transpiled language because I'm lazy.

Transpile to LuaJIT and not C or anything compiled because I still want load() access. (Sorry NeLua)

So this is basically NeLua or MetaLua, but everything is pure Lua -- no external compiling required.
PRO:
- Now you can use `load()`.  You can't in NeLua last I checked.
CON:
- Because it is centered around LuaJIT, which has not yet been ported to WASM last I checked.

Biggest pain points of Lua?
- one-based tables.
- global scope by default.
- (LuaJIT) no bit operators.

Features:
- bit operators that get implicitly converted to `bit.*` calls.
- shorthand single-expression: `|x,y| x+y`.
- shorthand multi-statement: `|x,y| do return x+y end`.

Complementing Features (in other libraries):
- https://github.com/thenumbernine/lua-ext `luajit -lext`: default operators for functions, coroutines, etc.
- `luajit -lext.debug` syntax for running things in debug-mode, or maybe even more of this, like types and type-checking upon-load()
- `luajit -lext.ctypes`: C types at global scope. this is an easy optional `require` to vanilla LuaJIT.  I put this in 
- https://github.com/thenumbernine/lua-local-default `luajit -llocal-default` local-by-default, global-by-keyword.  But this just wedges the env-setting into every function, so why not just do that with a transpiler-parser as well? In fact TODO for this is right now it only operates via `require()`.  Would be nice for it to operate via `load()` as well.  Might have to standardize my shim layer of the `require` and `load` functions which are used in this, `ext.debug`, `local-default`, `profiler`, `fullcallstack`, etc.

TODO
- Make each feature optional.  Bit-operators, single-expression-lambads, multi-expression-lambdas, `lua-ext` metatables, local-by-default, etc.   And maybe make that specifyable at runtime (for code modularity).
- Metamethods for this? but I suspect that will take too much runtime-changes, like testing each argument for a metamethod field, optionally calling, etc, and it would ruin performance.
- Better shim layer / modular shim layer.  Same shim used in `local-default`, `ext-debut`, `profile`, and here.  Would be nice to just provide a single interface for patching `load()`.
	- Maybe put it in ext.load, let that override the global load and loadfile, and then let it expose a table for modifying load content transformations.
	- Maybe also part of that load() being an AST parse and then callbacks for modifying the AST ...
- Somehow get this to work with the interpreter and with `-e` support.
- Stretch goal: zero-based indexing.  list literals shift their integer keys to initialize to be zero-based.  likewise they invoke a wrapper for a zero-based metamethod object.  `select` also shifts its indexes by 1.
- Maybe some shorthand for array/ptr construction based on the type? i.e. `char` is equivalent to `ffi.typeof'char'` and char:ptr() makes `'char*'` type, and `char:ar(10)` makes `char[10]` type.
	In pure Lua this would mean changing the ctype metatable, which LuaJIT goes way out of their way to mess with (having metatable() return strings, so you have to use debug.metatable() .... why?!?!?!)
- Better coroutine iteration for ranges, something more like luafun, or just make this whole thing compatible with luafun.
