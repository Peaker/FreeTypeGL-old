# FreeTypeGL

## A free texture fonts library for OpenGL and Haskell.

This library makes it easy to use freetype fonts with Haskell and OpenGL on all platforms.

The goal is to be installable where-ever the Haskell platform is
installed with a simple "cabal install" command.

Until a minor change in a dependency (freetype2) is merged, you will
have to manually install freetype2 via:

```shell
  git clone https://github.com/Peaker/freetype2
  cd freetype2
  cabal install
```

Then, you will be able to install FreeTypeGL with a simple cabal install command.
