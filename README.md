# 🎄 AOC 2024 🌟

## Local setup

Definitely going to regret this 2 years later when I'm no longer on nix...

    nix-shell -p ghc

## Commands

Compiling a binary

    ghc Day01

Running test case

    ghc Day01 && ./Day01 1 "tests/1.1.in" | diff "tests/1.1.out" -

Solving puzzle input

    ghc Day01 && ./Day01 1 "input/1.1.in"
