# Backc
The Bytecode Compiler for Back   
Back is a Concurrent Forth implementation, have fun

as of now, Backc has been completed.
the only things i will be adding are more words (maybe), or make the compiler more efficient

# Building
The only dependency of Backc is the GHC (The Glasgow Haskell Compiler)

Backc doesn't need anything fancy, just run :
```shell
ghc backc.hs
```

# Design Notes
The whole Back Compiler, is consisted of special tailored fold-like functions, like `backlex` or `untilcmt` that return their accumulator once they have no more input.
see the source code for a better idea of how it works

# License
Backc is licensed under the `GPL-3.0` license
