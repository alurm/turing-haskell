# Interpreter for Turing machines in Haskell (WIP)

(These instructions may be outdated by now.)

```text
turing-haskell ) nix run .
|
1|
|10
|
1|
11|
111|
1111|
|
1|
11|
111|
1111|
11111|
111111|
1111111|
11111111|
111111111|
|10
0|0
|00
1|0
|10
0|0
````

## Development

Use `nix develop` (or `direnv`). There's a VS Codium with the Haskell extension (and direnv) available as `codium`.

To build, `cabal build`.

Most of the code is in `app/Turing.hs`.

Sample repl session:

```text
turing-haskell ) cabal repl
λ :load Turing 
[1 of 1] Compiling Turing           ( app/Turing.hs, interpreted )
Ok, one module loaded.
λ putStr $ Turing.logn 5 '0' Turing.exampleLoop Turing.emptyTape
|
1|
11|
111|
1111|
λ 
```
