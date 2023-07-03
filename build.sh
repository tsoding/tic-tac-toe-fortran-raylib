#!/bin/sh

set -xe

FFLAGS=-cpp #-fno-range-check
LIBS="`pkg-config --libs raylib` -lglfw -ldl -lpthread"
SRC="src/game.f90 src/ai.f90 src/raylib.f90 src/main.f90"

mkdir -p build/
gfortran $FFLAGS -J build/ -o build/ttt $SRC $LIBS
