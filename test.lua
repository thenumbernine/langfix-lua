#!/usr/bin/env ./run.sh

-- single-statement lambdas
asserteq( (||1)(), 1)
asserteq( (|x|x+1)(1), 2)

-- single-statement lambda with multiple-return ... not supported yet
-- assert(load[[ asserteq( (|x|x+1,x+1)(1)) ]])()

-- multiple-statement lamda
asserteq( (|n| do local sum=0 for i=1,n do sum = sum + i end return sum end)(10), 55)

-- function metamethods (in ext)
local g = (function(x) return x+1 end) * (function(x) return x-1 end)
-- function g(x) = (x+1)*(x-1)
asserteq(g(2), (2+1)*(2-1))
asserteq(g(3), (3+1)*(3-1))
-- tempting to make a :cartesian :product :outer or somethign like that which combines args and multiplies results ...

-- bit operators
asserteq(2 << 2, 8)

--[=[ bit operator metamethods ... not supported yet
local g = (function(x) return x+1 end) << (function(x) return x-1 end)
-- function g(x) = (x+1)*(x-1)
asserteq(g(2), (2+1) << (2-1))
asserteq(g(3), (3+1) << (3-1))
--]=]

-- global-scope boxed ctypes
asserteq(int(7)/int(2), int(3))

-- integer division TODO has errors 
--print(7//2, 3)

-- lambda + function-operator
asserteq(((|x|x+1) * (|x|x-1))(3), 8)

-- function operators
asserteq((|x,y|x+y):bind(2)(3), 5)

--[=[ TODO test locals-as-default
-- but that should be shimming using require(), not load(), so I do need to move its scope to load() as well ...
do
	x = 1
end
asserteq(x, nil)
--]=]

-- tests:
-- multi line lambdas
-- bit operators
-- :bind(), :co() function metatable
-- :resume() thread metatable
-- int ctype global
asserttableieq({(|x,y| do coroutine.yield(x << y) end):bind(int(3)):co():resume(int(4))}, {true, 48})
