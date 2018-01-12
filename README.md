# traycer-hs

A ray-tracer written in Haskell.

## How to use

Define the world by making an object of the `Config` class. Call `config2Image` on it with a file name in the `IO` monad. It will produce a `.bmp`. An example is given in `app/Main.hs`.

### Example

![ray tracing example](image.bmp?raw=True "Ray Tracing Example")