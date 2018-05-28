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
  , mkCuboid
  , points
  , directions
  , changePoints
  , changeDirections
  , center
  , normal
  , radius
  , triangles
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
import Data.Function
import GHC.Generics
import Linear.Epsilon
import Linear.Metric
import Linear.V3
import Traycer.Geometry.Ray
import Traycer.Math


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
             | Polyhedron { _triangles :: [Triangle a] }
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

mkPoly :: (Floating a, Epsilon a, Ord a) => [V3 a] -> Solid a
mkPoly = Polyhedron . poly
  where
    poly [f, g, h] = [mkTriangle f g h]
    poly (f:g:h:rs) = mkTriangle f g h:poly rs
    poly _ = error "There should be a multiple of 3 vertices."
{-# INLINE mkPoly #-}

-- | Make a cuboid centered at origin
mkCuboid :: (Floating a, Epsilon a, Ord a) => a -> a -> a -> Solid a
mkCuboid xw yw zw
  | xw < 0 || yw < 0 || zw < 0 = error "Cuboid dimensions can't be < 0."
  | otherwise = Polyhedron [ mkTriangle p1 p4 p3
                           , mkTriangle p3 p2 p1
                           , mkTriangle p1 p2 p7
                           , mkTriangle p7 p6 p1
                           , mkTriangle p5 p1 p6
                           , mkTriangle p5 p4 p1
                           , mkTriangle p3 p4 p5
                           , mkTriangle p3 p5 p8
                           , mkTriangle p8 p1 p6
                           , mkTriangle p8 p6 p7
                           , mkTriangle p2 p8 p7
                           , mkTriangle p2 p3 p8
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
    
-- | Solid type helper functions for
--   collision etcetra

-- | Get all the points in the solid
points :: Solid a -> [V3 a]
points (Sphere !c _) = [c]
points (Plane !c _) = [c]
points (Disk !c _ _) = [c]
points (Polyhedron !ts) = concatMap (\(Triangle !a !b !c _) -> [a, b, c]) ts
{-# INLINE points #-}

-- | Get all the directios in the solid
directions :: Solid a -> [V3 a]
directions Sphere{} = []
directions (Plane _ !n) = [n]
directions (Disk _ !n _) = [n]
directions (Polyhedron !ts) = concatMap (\(Triangle _ _ _ !n) -> [n]) ts
{-# INLINE directions #-}

-- | Change all the points in the solid
changePoints :: Solid a -> (V3 a -> V3 a) -> Solid a
changePoints s@Sphere{} !f = s&center %~ f
changePoints s@Plane{} !f = s&center %~ f
changePoints s@Disk{} !f = s&center %~ f
changePoints s@Polyhedron{} !f = s&triangles %~ map changeTriPoints
  where
    changeTriPoints (Triangle !a !b !c !n) = Triangle (f a) (f b) (f c) n
{-# INLINE changePoints #-}

-- | Change all the directions in the solid
changeDirections :: (Epsilon a, Floating a)
                 => Solid a -> (V3 a -> V3 a) -> Solid a
changeDirections s@Sphere{} _ = s
changeDirections s@Plane{} !f = s&normal %~ normalize . f
changeDirections s@Disk{} !f = s&normal %~ normalize . f
changeDirections s@Polyhedron{} !f = s&triangles %~ map changeTriDir
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
hit (Polyhedron !ts) !ray
  | null validHits = Nothing
  | otherwise    = Just $ Collision t n a
  where
    hitList = zip ts $ map (\(Triangle x _ _ y) -> hitFlatSurface x y ray) ts
    validHits = filter (\(tr, (d, x)) -> not (nearZero d) && inTri tr x && x >= 0) hitList
    (Triangle !a _ _ !n, (_, t)) = minimumBy (compare `on` (snd . snd)) validHits
    inTri (Triangle !f !g !h !o) !x = sameSign (dot o $ cross e1 c1)
                                               (dot o $ cross e2 c2)
                                               (dot o $ cross e3 c3)
      where
        p = ray *-> x
        e1 = g - f
        e2 = h - g
        e3 = f - h
        c1 = p - f
        c2 = p - g
        c3 = p - h
    sameSign x y z = signum x == signum y &&
                     signum y == signum z
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
normalAt (Sphere !o !r) !ray !c = normalize $ if norm n >= r
                                              then n
                                              else -n
  where
    n = p - o
    p = ray *-> (c^.scalar)
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
refracted s@(Polyhedron !ts) !r !c !mu
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

hitFlatSurface :: (Num a, Fractional a) => V3 a -> V3 a -> Ray a -> (a, a)
hitFlatSurface !c !n !ray = (denom, t)
  where
    denom = dot (ray^.direction) n
    numer = dot (c - ray^.origin) n
    t = numer / denom
{-# INLINE hitFlatSurface #-}

-- ======================
