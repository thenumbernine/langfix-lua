-- tests:
-- multi line lambdas
-- bit operators
-- :bind(), :co() function metatable
-- :resume() thread metatable
-- int ctype global
print((|x,y| do coroutine.yield(x << y) end):bind(int(3)):co():resume(int(4)))
