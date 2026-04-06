# Game of Life in Fortran

A small terminal implementation of Conway's Game of Life written in Fortran.

## Requirements

- `gfortran`
- `just`
- `entr` (optional)

## Run

```sh
just build
just run
```

`just run` builds the executable and runs 50 generations by default.

To run a different number of generations:

```sh
./build/game_of_life <options>
```

**Options:**
- `-g`: generations (default `100`)
- `-f`: file (default: `input`)
- `-s`: speed (default `1`)

## Input format

To define the starting grid:

- `#` = live cell
- `.` = dead cell

**Glider input:**
```
.....................
.....................
.....................
......#..............
.....#...............
.....###.............
.....................
.....................
.....................
.....................
.....................
.....................
.....................
```

