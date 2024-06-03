-- run with luajit -llangfix test.lua
print((|x,y|do return x << y end)(3, 4))
