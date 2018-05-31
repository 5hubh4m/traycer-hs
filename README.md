# traycer-hs

A ray-tracer written in Haskell.

### Current Progress

#### Shapes

It currently has support for Spheres, Planes, Disks, and Triangle meshes. There are cconvenience functions to create Cuboids and Rectangles using mesh.

#### Textures

[Phong Illumination Model](https://en.wikipedia.org/wiki/Phong_reflection_model) and supports reflection and refraction. However, because this still very basic, there is no support for caustics (i.e shadows of transparent objects are as if they were opaque).

It has 6 parameters for controlling objects surface

- albedo: Color (Color of the surface)
- kDiffuse: Diffuse coefficient (Controls the diffuse reflection)
- kSpecular: Specular coefficient (Controls the specular reflection)
- exponent: Specular exponent (Controls the quality of specular highlights)
- kAmbient: Ambience coefficient (Controls the amount of ambient light)
- reflectance: Reflectance of the surface (Controls the light reflected off the surface)
- transparency: Transparency of the surface (Controls the light transmitted)
- mu: Refractive index (Controls refraction)

![Phong Ilumination Example](https://upload.wikimedia.org/wikipedia/commons/thumb/6/6b/Phong_components_version_4.png/655px-Phong_components_version_4.png "Phong Illumination Example")

#### Anti-aliasing and Depth of Field

It supports uniformly spread Anti-aliasing as well as Depth of field. Their quality can be controlled by the config (higher is better). Depth of field should only be used if the scene is spread out in the depth dimension, otherwise the effect is minimal.

#### Transformations

It supports transformations. Currently only translation and rotation are supported. They can be applied to both individual objects and globally (to emulate view transform).

#### Lights

It suports point-lights that can be colored and positioned arbitrarily in the space. Soft-shadows are currently not supported.

#### Camera

Camera can be configured by the size of the view window in the scene world, and the number of pixels for rendering quality. The eye can be positioned arbitrarity in the world. View transforms can be performed using global transforms in config.

#### Configuration

The ray-tracer is configured using a `yaml` config file. For an example, view the `examples/example.yaml` file. The details of parsing are in the file `src/Traycer/Parser.hs`.

#### Performance

It can render the example scene (three spheres, one cube, two disk mirrors, one transparent mirrors) with 16x Anti-aliasing and Depth of field turned off at resolution of 1200 x 800 in about 16 seconds.

### Future

#### Features

I was hoping to add at least Area-lights and Soft-shadows. Adding caustics will involve changing the entire algorithm to perform both backward and forward ray tracing, or more even more difficult, going physically based. Still haven't decided.

#### Performance Improvements

The ray-tracer hasn't been optimised for performance as it is mostly an excercise in learning Haskell. However, I keep coming back to it so I may work on improving performance in the future. The avenues may be:

- Using vectors instead of lists whereever appropriate
- Removing some redundant computation by caching their results
- Using more advanced ray-world intersection algorithms (Octree/Kd-Tree)

## Building

Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/). `cd` to the project directory and

`stack build`

Make sure you have `opt`([LLVM](http://llvm.org)) in your `$PATH`. If you don't want to build with `LLVM`, remove the `-fllvm` and `-optlo-O3` flags from `package.yaml`.

## Running

Define the world by making a config file (like `examples/Example.yaml`), and run the program as follows

`stack exec traycer-hs -- PATH_TO_CONFIG.yaml PATH_TO_IMAGE.png`

### Example

This example shows most major features of the ray-tracer.

![Traycer Example](examples/image.png?raw=True "Traycer Example")
