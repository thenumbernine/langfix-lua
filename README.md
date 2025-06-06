# Lua Language Fix

# What it does?
This Lua/JIT library adds new language features to Lua.  It is completely pure Lua and requires no external compilation.
It's backwards-compatible with old Lua(/JIT) code.

# How to use it?

Put the file `rua` in your bin directory:

``` sh
#!/bin/sh
rlwrap /path/to/langfix/run.lua "$@"
```

You can also use

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

# How it does it?
It operates using my [`lua-ext`](https://github.com/thenumbernine/lua-ext)'s `ext.load` shim layer to modify the `load`, `loadfile`, `loadstring` (if present), and `require` functions.
With these overloaded, it uses my [`lua-parser`](https://github.com/thenumbernine/lua-parser) library to transpile the new language operators into old language operators.  Your code loading might take a small performance hit, but at runtime fingers crossed you should not notice any performance change.

# New Language Features:

## Bit operators

`&`, `|`, `<<`, `>>`, `>>>`.  These get implicitly converted to `bit.*` calls: .  They don't work with metatmethods (yet?).

## Assign-to operators

`..=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `^=`, `&=`, `|=`, `<<=`, `>>=`, `>>>=`, `??=`.

Works with vararg assignment too: `a,b,c += 1,2,3`.

Assign-to-xor's symbol `~=` was already taken as not-equals, so I switched the assign-to-xor symbol to `~~=`.  Coincidentally, in math notation, xor is sometimes represented as a not-equals symbol.

## Shorthand Lambdas

Lambdas as multiple-statements: `|x,y| do return x+y end`.

Lambdas as single-expressions can omit the `do return end`, i.e.: `|x,y| x+y`.

Lambdas as single-expressions with multiple-returns need to have their expression-list wrapped in parenthesis to avoid ambiguity: `|x,y|(x+y,x-y)`.
This means the Lua truncate-multiple-arguments operation of wrapping with extra `()` will take a second set of parenthesis.

Lambdas with a first argument of `:` is replaced with `self`: `|:|self` is equivalent to `|self|self`.

Lambdas with a first argument of `::` is replaced with `self` and the function scope environment is set to `self`, akin to `_ENV=self` or `setfenv(1,self)`.

Ex: A lambda that returns vararg will look like `|...|...`.

Ex: A lambda that returns an extra value prepended to a vararg will look like `|...|('x', ...)`.

Ex: A lambda that truncates to the first value of a vararg will look like `|...|((...))`.

## Safe-Navigation Operator:

`a?.b`, `a?['b']`, `a?()`, `a?.b()`, `a?:b()`, `a.b?()`, `a?.b?()`, `a:b?()`, `a?:b?()` etc ... to bailout evaluation of indexes and calls early.

"safe-navigation-assign operator": `a?.b=c` means "if a doesn't exist then bail out early.  if b doesn't exist then assign it c.  return b."

## Non-Nil Assertion Operator:

`a!.b`, `a!['b']`, `a!()`, `a!.b()`, `a!:b()`, `a.b!()`, `a!.b?()`, `a:b!()`, `a!:b!()` etc ... to error when an indexed field is `nil`.

## Nil-Coalescing Operator:

`a ?? b` returns `a` if it is not nil, `b` otherwise.

`a ??= b` assigns `a` to `b` if `b` is not nil.

## Ternary operator

`a ? b : c` works as expected with false `b` values, unlike the equivalent expression `a and b or c`.

Ternary with 2nd args ending in a call `()` will need to be wrapped in `()`, due to the fact that `a():b()` could be a ternary 2nd and 3rd, and could be a valid prefix-expression self-call of `a()`'s result's member-function `b()`.  If I changed the ternary 2nd symbol from `:` to something else then I could avoid this.

Ternary with either args beginning in a `(` ... if it is not a wrapping `()` ... then you will need an extra wrapping `()`, due to the ternary-return-multiple-values requiring (and looking for) extra wrapping `()`'s.  If I removed mult-ret from ternary then I could avoid this.

So in general most your ternary operators will have their 2nd and 3rd arguments wrapped in `()`.  I can fix this if I change the ternary symbols, since `:` and `?:` is used elsewhere, in prefix-expressions and in safe-navigation.

Ternary handles multiple-returns just like single-expression lambdas do: wrap it in parenthesis as to not confuse a tailing comma with a new expression-list entry: `a ?? (b,c) : (d,e)`.  Yup, same language issue applies as single-expression-lambdas: if you want to truncate a multiple-return then now you need to wrap it in two parenthesis. Maybe this risks being problematic if you combine single-expression-lambdas, ternary, and multiple-expression-returns.

## Self-Scope

`function k::v(...)` function definitions for C++-style `self`-scope via `setfenv` or `_ENV`.

`k.v=|::,...|` works the same in lambda-shorthand form.

Things to note about self-scope:
- Once it is set, you no longer have access to any other scope including global scope unless you set proper measures i.e. your object's `__index` points back to `_G`.

# TODO

- Safe-navigation doesn't work as statements, only as expressions in the rhs of assignments.
- How about octal number support?  0777 == 512 .  Binary too?
- `const` to substitute for `local<const>` ... if LuaJIT ever adopted attributes...
- Support for `function a['b']:c() end` to work just like `function a.b:c()` does.
- Make each feature optional.  Bit-operators, single-expression-lambads, multi-expression-lambdas, `lua-ext` metatables, local-by-default, etc.   And maybe make that specifyable at runtime (for code modularity).
	- Maybe a first-line-comment for something like `use strict`, to specify what features should be on or off, as an exception to whatever default setting.
- Metamethods for bit operators? but I suspect that will take too much runtime-changes, like testing each argument for a metamethod field, optionally calling, etc, and it would ruin performance.
- Zero-based indexing.  list literals shift their integer keys to initialize to be zero-based.  likewise they invoke a wrapper for a zero-based metamethod object.  `select` also shifts its indexes by 1.
- Maybe some shorthand for ctype array/ptr construction based on the type? i.e. `char` is equivalent to `ffi.typeof'char'` and `char:ptr()` makes `'char*'` type, and `char:ar(10)` makes `char[10]` type.
	In pure Lua this would mean changing the ctype metatable, which LuaJIT goes way out of their way to mess with (having metatable() return strings, so you have to use debug.metatable() .... why?!?!?!)
- Better coroutine iteration for ranges, something more like luafun, or just make this whole thing compatible with luafun.
- Think of a new file extension to use?
- How about `++` etc operators?  But for the latter I'd have to change the single-line comments `--` ...  maybe go as far as Python did and just do `+=` 's ?
- I disagree so strongly with LuaJIT's default ctype struct index behavior of throwing errors if fields are missing, which breaks typical Lua convention of just returning nil, that I'm half-tempted to wrap all indexing operations in my `lua-ext`'s `op.safeindex` function, just to restore the original functionality, just to prove a point, even though I know it'll slow everything down incredibly.
- Should assign-to include the boolean operators `and=` `or=` ?
- Between shorthand lambda multiple-return needing something to distinguish its commas from any commas that would separate the lambda expression from other args, and the same exact problem in the ternary problem, maybe I should introduce syntax to both for wrapping multiple-expressions?  In both cases I require extra parenthesis, but these can get confusing with the fact that parenthesis-around-multiple-expressions is already supposed to represent truncating multiple-expressions down to one expression. 

# Complementing Features In Other Libraries:

- [`lua-ext`](https://github.com/thenumbernine/lua-ext):
	- `luajit -lext`: default operators for functions, coroutines, etc.
	- `luajit -lext.debug` syntax for running things in debug-mode, or maybe even more of this, like types and type-checking upon-load()
	- `luajit -lext.ctypes`: C types at global scope. this is an easy optional `require` to vanilla LuaJIT.  I put this in
- [`lua-local-default`](https://github.com/thenumbernine/lua-local-default):
	- `luajit -llocal-default`: local-by-default, global-by-keyword.  But this just wedges the env-setting into every function.  It might be better to replace new-assigns with locals and a new `global` keyword with non-locals.
- [`leftargs-lua`](https://github.com/thenumbernine/leftargs-lua):
	- It turns out code reads left-to-right in evaluation order and in instruction order, with less skips back and forth, if you put the call function arguments on the left hand side.
