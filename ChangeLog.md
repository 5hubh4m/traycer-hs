# Changelog for traycer-hs

#### 0.1

Initial release

#### 0.2

Added better camera config. Now the camera can be positioned arbitrarily and in any orientation. See `examples/Example.yaml`.

#### 0.3

Added transforms, but removed the camera positioning config as it can be emulated using transforms. Currently translation and rotation are supported. There are two kinds of transforms: object level and world level. See `examples/Example.yaml`.

Also added polyhedrons. It can now ray trace arbitrary convex polyhedrons. There is a convenience function for construction cuboids.

#### 0.3.1

Polyhedron intersection is now accelerated using bounding boxes. The ray is first checked for interection with these bounding boxes before performing actual intersection test.

Fixed a bug where lights were not being transformed with the whole scene.

#### 0.3.2

Minor refactor. Added rectangles to lessen the overhead of using cuboids to emulate ractangles, as the former has 12 triangles while the latter has only 2.
