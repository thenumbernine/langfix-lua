#!/usr/bin/env luajit
-- using this with nginx I'm getting errors about yielding across C-call boundaries, even though I'm not seeing such a yield in the call stack.
-- meanwhile nginx has *HARDCODED* 2k limit to error log messages *JUST BECAUSE* (smh rtarded)
-- I wonder why . . . here's me trying to reproduce it . . .
require 'ext'
require 'langfix'

local th = (function()
	assert(xpcall(function()
		assert(load[[

assert((||do
	print(||x)

	assert(xpcall(||do

		assert(load[=[
			print(||x)
		]=])()

	end))

end):co():resume())
		

local th = (||do
	assert(xpcall(||do
		print(||x)
	end))
end):co()

local th2 = (||do
	assert(th:resume())
end):co()

assert(th2:resume())

		]])()
	end))
end):co()

local th2 = (function()
	assert(xpcall(function()
		assert(th:resume())
	end))
end):co()

assert(th2:resume())
