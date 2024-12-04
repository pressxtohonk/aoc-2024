# 🎄 AOC 2024 🌟

## Environment

Development environment bundled by Nix flakes: https://nix.dev/install-nix.html

    nix develop

## Development

This project uses cabal to build and run executables. Build targets are configured in `aoc2024.cabal`.

For example, to configure a target `example` with source directory `src/` and entrypoint `src/Example.hs`:

    executable example
        hs-source-dirs:       src
        main-is:              Example.hs
        build-depends:        base

 The executable is then built and run with:

    cabal run example

## Examples

Build, test, and solve:

    ./run [DAY] [PART]

Building executable:

    cabal build day01

Running test cases:

    cabal run day01 1 "resources/tests/1.1.in" | diff "resources/tests/1.1.out" -

Solving puzzle input:

    cabal run day01 1 "resources/input/1.1.in"
