Transpiled language because I'm lazy.

Transpile to LuaJIT and not C or anything compiled because I still want `load()` access. (Sorry NeLua)

So this is basically NeLua or MetaLua, but everything is pure Lua -- no external compiling required.

PRO:
- Now you can use `load()`.  You can't in NeLua last I checked.

CON:
- It is centered around LuaJIT, which has not yet been ported to WASM last I checked.

Biggest pain points of Lua?
- Missing some convenience operators.
- One-based tables.
- Global scope by default.
- (LuaJIT) no bit operators.

Features:
- bit operators that get implicitly converted to `bit.*` calls: `& | << >> >>>`.  They don't work with metatmethods (yet?).
- Assign-to operators: `..= += -= *= /= //= %= ^= &= |= <<= >>= >>>=`.  Works with vararg assignment too: `a,b,c += 1,2,3`.
- shorthand single-expression: `|x,y| x+y`.
- shorthand multi-statement: `|x,y| do return x+y end`.
- "safe-navigation operator" `a?.b`, `a?['b']`, `a?()`, `a?.b()`, `a?:b()`, `a.b?()`, `a?.b?()`, `a:b?()`, `a?:b?()` etc ... to bailout evaluation of indexes and calls early.

Complementing Features (in other libraries):
- https://github.com/thenumbernine/lua-ext `luajit -lext`: default operators for functions, coroutines, etc.
- `luajit -lext.debug` syntax for running things in debug-mode, or maybe even more of this, like types and type-checking upon-load()
- `luajit -lext.ctypes`: C types at global scope. this is an easy optional `require` to vanilla LuaJIT.  I put this in 
- https://github.com/thenumbernine/lua-local-default `luajit -llocal-default` local-by-default, global-by-keyword.  But this just wedges the env-setting into every function.  It might be better to replace new-assigns with locals and a new `global` keyword with non-locals.

TODO
- `const` to substitute for `local<const>` ... if LuaJIT ever adopted attributes...
- Legit ternary operator: `a ? b : c` but safe for boolean types?
- Support for `function a['b']:c() end` to work just like `function a.b:c()` does.
- Make each feature optional.  Bit-operators, single-expression-lambads, multi-expression-lambdas, `lua-ext` metatables, local-by-default, etc.   And maybe make that specifyable at runtime (for code modularity).
	- Maybe a first-line-comment for something like `use strict`, to specify what features should be on or off, as an exception to whatever default setting.
- Metamethods for bit operators? but I suspect that will take too much runtime-changes, like testing each argument for a metamethod field, optionally calling, etc, and it would ruin performance.
- Somehow get this to work with the interpreter and with `-e` support.
- Zero-based indexing.  list literals shift their integer keys to initialize to be zero-based.  likewise they invoke a wrapper for a zero-based metamethod object.  `select` also shifts its indexes by 1.
- Maybe some shorthand for ctype array/ptr construction based on the type? i.e. `char` is equivalent to `ffi.typeof'char'` and char:ptr() makes `'char*'` type, and `char:ar(10)` makes `char[10]` type.
	In pure Lua this would mean changing the ctype metatable, which LuaJIT goes way out of their way to mess with (having metatable() return strings, so you have to use debug.metatable() .... why?!?!?!)
- Better coroutine iteration for ranges, something more like luafun, or just make this whole thing compatible with luafun.
- Think of a new extension to use?
- How about ++ etc operators?  But for the latter I'd have to change the single-line comments `--` ...  maybe go as far as Python did and just do `+=` 's ?
