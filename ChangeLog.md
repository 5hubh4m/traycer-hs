# Changelog for traycer-hs

#### 0.1

Initial release

#### 0.2

Added better camera config. Now the camera can be positioned arbitrarily and in any orientation. See `examples/Example.yaml`.

#### 0.3

Added transforms, but removed the camera positioning config as it can be emulated using transforms. Currently translation and rotation are supported. There are two kinds of transforms: object level and world level. See `examples/Example.yaml`.

Also added polyhedrons. It can now ray trace arbitrary convex polyhedrons. There is a convenience function for construction cuboids.
