#!/usr/bin/env just --justfile

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

clean:
  rm -rf {{build}}
