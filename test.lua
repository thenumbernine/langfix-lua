-- run with luajit -llangfix test.lua
print((|x,y| return x << y end)(3, 4))
