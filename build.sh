#!/bin/sh

set -xe

FFLAGS=-cpp #-fno-range-check

gfortran $FFLAGS -o main raylib.f90 main.f90 `pkg-config --libs raylib` -lglfw -ldl -lpthread
