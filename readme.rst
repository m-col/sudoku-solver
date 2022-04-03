sudoku-solver
=============

This small Haskell script solves sudoku puzzles. I wrote this as part of a
programming exercise.

The puzzle in ``puzzle_1.txt`` is in the public domain, and is taken from
Wikipedia.

Usage
-----

The script uses ``runhaskell`` as an interpreter but doesn't use any third
party libraries.

./sudoku.hs puzzle_1.txt
./sudoku.hs  # Defaults to puzzle_1.text

Where the file passed contains a sudoku puzzle encoded in spaces and digits,
ignoring all other characters including newlines etc. The script extracts the
digits and spaces and maps those onto the puzzle grid.
