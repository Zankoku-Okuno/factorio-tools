# Building

I'm using GHC 8.10, `cabal` and `record-dot-preprocessor`.
Unless you already have `record-dot-preprocessor` on your path, use the following command to install it.
Be sure to run it from somewhere that isn't a cabal project, or it'll sandbox the build.

```sh
cabal install record-dot-preprocessor --installdir="$HOME/.cabal/bin"
```

Hopefully this will get easier with GHC 8.12.
