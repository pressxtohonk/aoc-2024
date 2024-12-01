# 🎄 AOC 2024 🌟

## Local setup

Development environment bundled by Nix: https://nix.dev/install-nix.html

    nix develop

## Commands

Compiling a binary

    ghc Day01

Running test case

    ghc Day01 && ./Day01 1 "tests/1.1.in" | diff "tests/1.1.out" -

Solving puzzle input

    ghc Day01 && ./Day01 1 "input/1.1.in"
