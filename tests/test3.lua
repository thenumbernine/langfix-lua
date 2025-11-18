#!/usr/bin/env luajit
-- found a bad edge case ... annnd it was just  me forgetting to assert() wrap a coroutine resume
require 'ext'
require 'langfix'

assert((function()
	assert(xpcall(function()
		assert(load[[

assert((||do
	assert(xpcall(||do
		print(||x)
	end))
end):co():resume())
		]])()
	end))
end):co():resume())
