{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Geometry.Solid
  ( Solid()
  , Collision()
  , mkDisk
  , mkPlane
  , mkSphere
  , mkPoly
  , mkPolyFromVertices
  , mkCuboid
  , mkRectangle
  , changePoints
  , changeDirections
  , center
  , normal
  , radius
  , triangles
  , boundMin
  , boundMax
  , scalar
  , collNormal
  , collPoint
  , hit
  , normalAt
  , reflected
  , refracted
  ) where

import Control.Lens
import Data.Foldable
import GHC.Generics
import Linear.Epsilon
import Linear.Metric
import Linear.V3
import Traycer.Geometry.Ray
import Traycer.Math
import Traycer.Util

data Triangle a = Triangle { _p1 :: !(V3 a)
                           , _p2 :: !(V3 a)
                           , _p3 :: !(V3 a)
                           , _triNormal :: !(V3 a)
                           }
                deriving (Show, Read, Eq, Generic)

data Solid a = Plane { _center :: !(V3 a)  -- ^ Any point that passes through the plane
                     , _normal :: !(V3 a) -- ^ The normal `vector` should always be normalised
                     }
             | Sphere { _center :: !(V3 a)
                      , _radius :: !a
                      }
             | Disk { _center :: !(V3 a) -- ^ The center of the disk
                    , _normal :: !(V3 a) -- ^ The normal `vector` should always be normalised
                    , _radius :: !a      -- ^ Radius of the disk
                    }
             | Polyhedron { _triangles :: ![Triangle a]
                          , _boundMin :: !(V3 a)
                          , _boundMax :: !(V3 a)
                          }
             deriving (Show, Read, Eq, Generic)

data Collision a = Collision { _scalar :: !a
                             , _collNormal :: !(V3 a)
                             , _collPoint :: !(V3 a)
                             }
                 deriving (Show, Read, Eq)

makeLenses ''Solid
makeLenses ''Collision

instance (Ord a) => Ord (Collision a) where
  compare c1 c2 = compare (c1^.scalar) (c2^.scalar)

-- Contructors
mkSphere :: (Num a, Ord a) => a -> Solid a
mkSphere !r
  | r < 0     = error "Negative radius for sphere."
  | otherwise = Sphere 0 r
{-# INLINE mkSphere #-}

mkPlane :: (Floating a, Epsilon a) => V3 a -> Solid a
mkPlane !n
  | nearZero n = error "Normal can't be zero."
  | otherwise  = Plane 0 $ normalize n
{-# INLINE mkPlane #-}

mkDisk :: (Floating a, Epsilon a, Ord a) => V3 a -> a -> Solid a
mkDisk !n !r
  | r < 0      = error "Negative radius for disk."
  | nearZero n = error "Normal can't be zero."
  | otherwise  = Disk 0 (normalize n) r
{-# INLINE mkDisk #-}

mkTriangle :: (Floating a, Epsilon a, Ord a) => V3 a -> V3 a -> V3 a -> Triangle a
mkTriangle !a !b !c = Triangle a b c $ normalize $ cross (b - a) (c - a)
{-# INLINE mkTriangle #-}

mkPoly :: (Ord a, Foldable f) => f (Triangle a) -> Solid a
mkPoly !ts = Polyhedron (toList ts) (V3 xMin yMin zMin) (V3 xMax yMax zMax)
  where
    vs = concatMap (\(Triangle !a !b !c _) -> [a, b, c]) ts
    xVals = map (^._x) vs
    yVals = map (^._y) vs
    zVals = map (^._z) vs
    xMin = minimum xVals
    xMax = maximum xVals
    yMin = minimum yVals
    yMax = maximum yVals
    zMin = minimum zVals
    zMax = maximum zVals 
{-# INLINE mkPoly #-}

mkPolyFromVertices :: (Floating a, Epsilon a, Ord a) => [V3 a] -> Solid a
mkPolyFromVertices !vs = mkPoly $ poly vs
  where
    poly [f, g, h] = [mkTriangle f g h]
    poly (f:g:h:rs) = mkTriangle f g h:poly rs
    poly _ = error "There should be a multiple of 3 vertices."     
{-# INLINE mkPolyFromVertices #-}

-- | Make a cuboid centered at origin
mkCuboid :: (Floating a, Epsilon a, Ord a) => V3 a -> Solid a
mkCuboid (V3 !xw !yw !zw)
  | xw < 0 || yw < 0 || zw < 0 = error "Cuboid dimensions can't be negative."
  | otherwise = mkPolyFromVertices [ p1, p4, p3
                                   , p3, p2, p1
                                   , p1, p2, p7
                                   , p7, p6, p1
                                   , p5, p1, p6
                                   , p5, p4, p1
                                   , p3, p4, p5
                                   , p3, p5, p8
                                   , p8, p1, p6
                                   , p8, p6, p7
                                   , p2, p8, p7
                                   , p2, p3, p8
                                   ]
  where
    p1 = V3 (xw / 2) (yw / 2) (zw / 2)
    p2 = V3 (-xw / 2) (yw / 2) (zw / 2)
    p3 = V3 (-xw / 2) (yw / 2) (-zw / 2)
    p4 = V3 (xw / 2) (yw / 2) (-zw / 2)
    p5 = V3 (xw / 2) (-yw / 2) (-zw / 2)
    p6 = V3 (xw / 2) (-yw / 2) (zw / 2)
    p7 = V3 (-xw / 2) (-yw / 2) (zw / 2)
    p8 = V3 (-xw / 2) (-yw / 2) (-zw / 2)
{-# INLINE mkCuboid #-}

-- | Make a rectangle centered at origin
mkRectangle :: (Floating a, Epsilon a, Ord a) => V3 a -> Solid a
mkRectangle (V3 !x !y !z)
  | x < 0 || y < 0 || z < 0 = error "Rectangle dimensions must be positive."
  | otherwise = mkPolyFromVertices [ p1, p2, p3
                                   , p3, p4, p1
                                   ]
  where
    getPoints 0 yw zw = ( V3 0 (yw / 2) (zw / 2),
                           V3 0 (yw / 2) (-zw / 2),
                           V3 0 (yw / 2) (-zw / 2),
                           V3 0 (-yw / 2) (zw / 2)
                         )
    getPoints xw 0 zw = ( V3 (xw / 2) 0 (zw / 2),
                           V3 (xw / 2) 0 (-zw / 2),
                           V3 (-xw / 2) 0 (-zw / 2),
                           V3 (-xw / 2) 0 (zw / 2)
                         )
    getPoints xw yw 0 = ( V3 (xw / 2) (yw / 2) 0,
                           V3 (xw / 2) (-yw / 2) 0,
                           V3 (-xw / 2) (-yw / 2) 0,
                           V3 (-xw / 2) (yw / 2) 0
                         )
    getPoints _ _ _ = error "Rectangle must have one 0 dimension and 2 positive dimensions."
    (p1, p2, p3, p4) = getPoints x y z
{-# INLINE mkRectangle #-}

-- | Solid type helper functions for
--   collision etcetra

-- | Change all the points in the solid by the given function
changePoints :: (Ord a) => Solid a -> (V3 a -> V3 a) -> Solid a
changePoints s@Sphere{} !f = s&center %~ f
changePoints s@Plane{} !f = s&center %~ f
changePoints s@Disk{} !f = s&center %~ f
changePoints (Polyhedron !ts _ _) !f = mkPoly $ map changeTriPoints ts
  where
    changeTriPoints (Triangle !a !b !c !n) = Triangle (f a) (f b) (f c) n
{-# INLINE changePoints #-}

-- | Change all the directions in the solid by the given function
changeDirections :: (Epsilon a, Floating a, Ord a)
                 => Solid a -> (V3 a -> V3 a) -> Solid a
changeDirections s@Sphere{} _ = s
changeDirections s@Plane{} !f = s&normal %~ normalize . f
changeDirections s@Disk{} !f = s&normal %~ normalize . f
changeDirections (Polyhedron !ts _ _) !f = mkPoly $ map changeTriDir ts
  where
    changeTriDir (Triangle !a !b !c !n) = Triangle a b c $ normalize $ f n
{-# INLINE changeDirections #-}

-- | Test for collision with a solid
--   If collision is successful, return the ray
--   coordinate for the point of intersection
hit :: (Epsilon a, Floating a, Ord a, Eq a)
    => Solid a                    -- ^ The 'Solid' to test collision with
    -> Ray a                      -- ^ 'Ray' to hit
    -> Maybe (Collision a)        -- ^ Scalar which gives the intersection point
hit (Plane !c !n) !ray
  | nearZero denom || t <= 0 = Nothing
  | otherwise                = Just $ Collision t 0 0 
  where
    (denom, t) = hitFlatSurface c n ray
hit (Disk !c !n !r) !ray
  | nearZero denom || t <= 0 || (norm (c - p) > r) = Nothing
  | otherwise                                      = Just $ Collision t 0 0
   where
    (denom, t) = hitFlatSurface c n ray
    p = ray *-> t
hit (Polyhedron !ts !b1 !b2) !ray = if hitBox b1 b2 ray
                                    then foldl step Nothing ts
                                    else Nothing
  where
    step x (Triangle a b c n) = maybeMin x x'
      where
        (denom, t') = hitFlatSurface a n ray
        p = ray *-> t'
        x' = if nearZero denom || t' < 0 || not (insideTriangle a b c n p)
             then Nothing
             else Just $ Collision t' n a 
hit (Sphere !o !r) !ray = case quadratic a b c of
  Nothing -> Nothing
  Just (t0, t1)
    | t0 <= 0 && t1 <= 0 -> Nothing
    | t0 <= 0 && t1 > 0  -> Just $ Collision t1 0 0
    | otherwise          -> Just $ Collision (min t0 t1) 0 0
  where
    l = ray^.origin - o
    a = quadrance $ ray^.direction
    b = 2 * dot (ray^.direction) l
    c = quadrance l - r * r
{-# INLINE hit #-}

-- | Get the normal at the point of intersection
normalAt :: (Epsilon a, Floating a, Ord a, Eq a)
       => Solid a
       -> Ray a
       -> Collision a
       -> V3 a                    -- ^ Normal vector at the intersection point, normalised
normalAt (Plane !c !n) !ray _ = if dot n (ray^.origin - c) >= 0
                                then n
                                else -n
normalAt (Disk !c !n _) !ray _ = if dot n (ray^.origin - c) >= 0
                                 then n
                                 else -n
normalAt Polyhedron{} !ray (Collision _ !n !c) = if dot n (ray^.origin - c) >= 0
                                                 then n
                                                 else -n
normalAt (Sphere !o !r) !ray !coll = if dot n (ray^.origin) >= r
                                     then n
                                     else -n
  where
    n = normalize $ c - o
    c = ray *-> (coll^.scalar)
{-# INLINE normalAt #-}

-- | Get the reflected ray from the intersection
--   of ray with solid
reflected :: (Epsilon a, Floating a, Ord a, Eq a)
          => Solid a
          -> Ray a
          -> Collision a
          -> Ray a                -- ^ Reflected ray at intersection point
reflected !s !r !c = mkRay p (reflect (r^.direction) n) (r^.medium)
  where
    p = r *-> (c^.scalar)
    n = normalAt s r c
{-# INLINE reflected #-}

-- | Get the refracted ray from the intersection
--  of ray with the solid
refracted :: (Epsilon a, Floating a, Ord a, Eq a)
          => Solid a
          -> Ray a
          -> Collision a
          -> (a -> Maybe (Ray a)) -- | returns a function that takes
                                  --   mu and maybe returns a refracted ray
refracted Plane{} !r _ _ = Just r
refracted Disk{} !r _ _ = Just r
refracted s@(Polyhedron !ts _ _) !r !c !mu
  | length ts < 4 = Just r
  | otherwise = (\x -> mkRay p x mu) <$>
                refract (r^.direction) n (r^.medium) mu
  where
    p = r *-> (c^.scalar)
    n = normalAt s r c
refracted !s !r !c !mu = (\x -> mkRay p x mu) <$>
                         refract (r^.direction) n (r^.medium) mu
  where
    p = r *-> (c^.scalar)
    n = normalAt s r c
{-# INLINE refracted #-}

-- ======================
-- Temporary functions

hitBox :: (Fractional a, Ord a) => V3 a -> V3 a -> Ray a -> Bool
hitBox b1 b2 r = not $ txMin > tyMax  || tyMin > txMax  || txyMin > tzMax || tzMin > txyMax
  where
    invDir = 1 / r^.direction
    selectB (V3 a b c) = V3 (if a then b2^._x else b1 ^._x)
                            (if b then b2^._y else b1^._y)
                            (if c then b2^._z else b1^._z)
    s = (< 0) <$> invDir
    s' = not <$> s
    (V3 txMin tyMin tzMin) = (selectB s - (r^.origin)) * invDir
    (V3 txMax tyMax tzMax) = (selectB s' - (r^.origin)) * invDir
    txyMin = max txMin tyMin
    txyMax = min txMax tyMax
{-# INLINE hitBox #-}

insideTriangle :: (Num a, Ord a) => V3 a -> V3 a -> V3 a -> V3 a -> V3 a -> Bool
insideTriangle !f !g !h !n !p = sameSign (dot n $ cross e1 c1)
                                         (dot n $ cross e2 c2)
                                         (dot n $ cross e3 c3)
  where
    e1 = g - f
    e2 = h - g
    e3 = f - h
    c1 = p - f
    c2 = p - g
    c3 = p - h
    sameSign x y z = x * y >= 0 && y * z >= 0
{-# INLINE insideTriangle #-}

hitFlatSurface :: (Num a, Fractional a) => V3 a -> V3 a -> Ray a -> (a, a)
hitFlatSurface !c !n !ray = (denom, t)
  where
    denom = dot (ray^.direction) n
    numer = dot (c - ray^.origin) n
    t = numer / denom
{-# INLINE hitFlatSurface #-}

-- ======================
