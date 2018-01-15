# traycer-hs

A ray-tracer written in Haskell.

## Building

Install [haskell stack url](https://docs.haskellstack.org/en/stable/README/ "Haskell Stack"). `cd` to the project directory and

`stack build`

Make sure you have `opt`([llvm url](http://llvm.org "LLVM")) in your `$PATH`. If you don't want to build with `LLVM`, remove the `-fllvm` and `-optlo-O3` flags from `package.yaml`.

## Running

Define the world by making a config file (like `examples/Example.yaml`), and run the program as follows

`stack exec traycer-hs -- PATH_TO_CONFIG PATH_TO_IMAGE.png`

### Example

![ray tracing example](examples/image.png?raw=True "Ray Tracing Example")
