module Main where

import Traycer

config :: Config Double Int
config = mkConfig [ Body { _solid = mkSphere (V3 0 (-0.5) 7) 0.5
                         , _texture = mkDiffuse _red 0.7 0.3 50
                         }
                  , Body { _solid = mkSphere (V3 0.3 0 2) 0.4
                         , _texture = mkTransparent _blue 0 0 0 0 0.9 1.5
                         }
                  , Body { _solid = mkSphere (V3 1 (-0.3) 6.7) 0.3
                         , _texture = mkDiffuse _blue 0.6 0.4 10
                         }
                  , Body { _solid = mkDisk (V3 (-1) 0 7) (V3 1 0 (-1)) 1
                         , _texture = mkReflective _white 0 0 0 0.99
                         }
                  , Body { _solid = mkDisk (V3 2 0 7) (V3 (-1) 0 (-1)) 1
                         , _texture = mkReflective _white 0 0 0 0.99
                         }
                  , Body { _solid = mkDisk (V3 0 (-1) 7) (V3 0 1 0) 3
                         , _texture = mkDiffuse _green 0.5 0.1 10 
                         }
                  ]
                  [ Light { _intensity = _white
                          , _position = V3 0 10 (-2)
                          }
                  , Light { _intensity = _white
                          , _position = V3 (-10) 5 (-5)
                          }
                  ]
                  (fromRGB 135 206 250)                               -- ^ Ambient
                  (mkCamera (V3 0 0 (-10)) (V2 4 3) (V2 1600 1200) 4) -- ^ Camera
                  10                                                  -- ^ Depth
                  4                                                   -- ^ AA samples
                  25                                                  -- ^ Dof samples
           
main :: IO ()
main = config2Image config "image.png"
