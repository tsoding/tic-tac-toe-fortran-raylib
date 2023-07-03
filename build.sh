#!/bin/sh

set -xe

FFLAGS=-cpp #-fno-range-check

gfortran $FFLAGS -o main game.f90 raylib.f90 main.f90 `pkg-config --libs raylib` -lglfw -ldl -lpthread
