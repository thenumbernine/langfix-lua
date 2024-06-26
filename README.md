# Lua Language Fix

### What it does?
This Lua/JIT library adds new language features to Lua.  It is completely pure Lua and requires no external compilation.
It's backwards-compatible with old Lua(/JIT) code.

### How to use it?

You can use

``` sh
lua -llangfix $filename
```

or

``` sh
lua -llangfix -e $code
```
and immediately start using the language features.

Alternatively, within a Lua file, you can call

``` Lua
require 'langfix'
```
and then from that point on, all subsequent `require`'d and `load`'d code will be able to use the new language features.

### How it does it?
It operates using my [`lua-ext`](https://github.com/thenumbernine/lua-ext)'s `ext.load` shim layer to modify the `load`, `loadfile`, `loadstring` (if present), and `require` functions.
With these overloaded, it uses my [`lua-parser`](https://github.com/thenumbernine/lua-parser) library to transpile the new language operators into old language operators.  Your code loading might take a small performance hit, but at runtime fingers crossed you should not notice any performance change.

### New Language Features:
- Bit operators `&`, `|`, `<<`, `>>`, `>>>`.  These get implicitly converted to `bit.*` calls: .  They don't work with metatmethods (yet?).
- Assign-to operators: `..=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `&=`, `|=`, `<<=`, `>>=`, `>>>=`.  Works with vararg assignment too: `a,b,c += 1,2,3`.
- Lambdas as multiple-statements: `[x,y] do return x+y end`.
- Lambdas as single-expressions: `[x,y] x+y`.
- Lambdas as single-expressions with multiple-returns need to have their expression-list wrapped in parenthesis to avoid ambiguity: `[x,y](x+y,x-y)`.
	This means the Lua truncate-multiple-arguments operation of wrapping with extra `()` will take a second set of parenthesis.
	- A lambda that returns vararg will look like `[...]...`.
	- A lambda that returns an extra value prepended to a vararg will look like `[...]('x', ...)`.
	- A lambda that truncates to the first value of a vararg will look like `[...]((...))`.
- Lambdas with a first argument of `:` is replaced with `self`: `[:]self` is equivalent to `[self]self`.
- "Safe-navigation operator": `a?.b`, `a?['b']`, `a?()`, `a?.b()`, `a?:b()`, `a.b?()`, `a?.b?()`, `a:b?()`, `a?:b?()` etc ... to bailout evaluation of indexes and calls early.

### TODO
- `const` to substitute for `local<const>` ... if LuaJIT ever adopted attributes...
- Support for `function a['b']:c() end` to work just like `function a.b:c()` does.
- Make each feature optional.  Bit-operators, single-expression-lambads, multi-expression-lambdas, `lua-ext` metatables, local-by-default, etc.   And maybe make that specifyable at runtime (for code modularity).
	- Maybe a first-line-comment for something like `use strict`, to specify what features should be on or off, as an exception to whatever default setting.
- Metamethods for bit operators? but I suspect that will take too much runtime-changes, like testing each argument for a metamethod field, optionally calling, etc, and it would ruin performance.
- Somehow get this to work with the interpreter and with `-e` support.
- Zero-based indexing.  list literals shift their integer keys to initialize to be zero-based.  likewise they invoke a wrapper for a zero-based metamethod object.  `select` also shifts its indexes by 1.
- Maybe some shorthand for ctype array/ptr construction based on the type? i.e. `char` is equivalent to `ffi.typeof'char'` and char:ptr() makes `'char*'` type, and `char:ar(10)` makes `char[10]` type.
	In pure Lua this would mean changing the ctype metatable, which LuaJIT goes way out of their way to mess with (having metatable() return strings, so you have to use debug.metatable() .... why?!?!?!)
- Better coroutine iteration for ranges, something more like luafun, or just make this whole thing compatible with luafun.
- Think of a new file extension to use?
- How about a legit ternary operator: `a ? b : c` but safe for boolean types?
- How about `++` etc operators?  But for the latter I'd have to change the single-line comments `--` ...  maybe go as far as Python did and just do `+=` 's ?
- I disagree so strongly with LuaJIT's default ctype struct index behavior of throwing errors if fields are missing, which breaks typical Lua convention of just returning nil, that I'm half-tempted to wrap all indexing operations in my `lua-ext`'s `op.safeindex` function, just to restore the original functionality, just to prove a point, even though I know it'll slow everything down incredibly.
- I'm still thinking how to handle lambdas that are single-expression multiple-return-value.
	If I allow them as `[x]x+1,x+2` then when parsing lambdas in arguments, all successive arguments get lumped into the lambda's multiple-return.
		This gets avoided if I wrap the lambda in ()'s, but then I have to wrap all lambdas being passed into function arguments with ()'s.
	Maybe I would require an extra () around the multiple-return arguments of a single-expression multiple-return lambda, but then that would cause a truncated-multiple-return (i.e. `(...)` evaluates to just the first argument of `...`) to be wrapped in *two* parenthesis instead of just one as with Vanilla Lua.
	(I.e. `[x](x+1, x+2)` would return two values, but then `[x]((assert(x, 'truncate')))` would be proper syntax to truncate the `'truncate'` string upon returning).

### Complementing Features In Other Libraries:
- [`lua-ext`](https://github.com/thenumbernine/lua-ext):
	- `luajit -lext`: default operators for functions, coroutines, etc.
	- `luajit -lext.debug` syntax for running things in debug-mode, or maybe even more of this, like types and type-checking upon-load()
	- `luajit -lext.ctypes`: C types at global scope. this is an easy optional `require` to vanilla LuaJIT.  I put this in
- [`lua-local-default`](https://github.com/thenumbernine/lua-local-default):
	- `luajit -llocal-default`: local-by-default, global-by-keyword.  But this just wedges the env-setting into every function.  It might be better to replace new-assigns with locals and a new `global` keyword with non-locals.
