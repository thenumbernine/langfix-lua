#!/bin/sh
luajit -lext -lext.ctypes -llangfix run.lua "$@"
#-llocal-default ... but this uses a shim parser, which langfix does too, so hmm
