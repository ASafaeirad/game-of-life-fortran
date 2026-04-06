#!/usr/bin/env just --justfile

set quiet
src    := "src/kinds_mod.f90 src/main.f90"
build  := "build"
target := build + "/game_of_life"
build:
  mkdir -p {{build}}
  mkdir -p {{build}}/mod
  gfortran -Wall -g -o {{target}} -J {{build}}/mod {{src}}

[default]
run:
  just build
  ./{{target}}

watch:
  just build
  find src -name "*.f90" | entr -c just run

clean:
  rm -rf {{build}}
