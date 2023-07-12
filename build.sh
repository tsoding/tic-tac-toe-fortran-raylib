#!/bin/sh

set -xe

FFLAGS="-std=f2003 -fno-range-check"
LIBS="`pkg-config --libs raylib` -lglfw -ldl -lpthread"
SRC="src/game.f03 src/ai.f03 src/raymath.f03 src/raylib.f03 src/ui.f03 src/main.f03"

mkdir -p build/
gfortran $FFLAGS -J build/ -o build/ttt $SRC $LIBS
