# aoc-2k21
Solutions of Advent of Code 2021 in Haskell.

First, place input files under `inputs/<day_number>.txt` in the root folder. Input files must be named in the given format to be recognized correctly.

To run all solutions:
```sh
stack build
stack exec aoc-2k21-exe
```

To run solutions for a subset of days:
```sh
stack build
stack exec aoc-2k21-exe 1 2 10 # Runs solutions for days 1, 2 and 10
```
