{-# LANGUAGE KindSignatures #-}
module Graphics.Solidhs where

import Data.VectorSpace

type R  = Float
type R3 = (R, R, R)
type R4 = (R, R, R, R)
type N  = Int
type N3  = (Int, Int, Int)

data CSG = Union  [CSG]
         | Diff   [CSG]
         | Intersection [CSG]
         | Sphere R   R3 -- size, center
         | Cube   R   R3 -- size, center

         | Cylinder        R R R R R3 N -- r, h, r1, r2, center, segments
         | Polyhedron      [R3] [N3] N  -- points, triangles, convexity
         -- | Hole', 'args': [], 'kwargs': []} ,
         -- | Part', 'args': [], 'kwargs': []} ,
         | Translate       R3 [CSG] -- vector, block
         | Scale           R3 [CSG] -- vector, block
         | Rotate          R  R3 [CSG] -- angle, vector, block
         | Mirror          R3 [CSG]    -- vector, block
--         | Multmatrix', 'args': ['m'], 'kwargs': []},
         | Color           R4
         | Minkowski       [CSG]
         | Hull            [CSG]
         | Render          R [CSG]
         | LinearExtrude   R R3 N         -- height, center, convexity, twist, slices
--         | RotateExtrude', 'args': [], 'kwargs': ['convexity', 'segments']} ,
--         | Dxf_linear_extrude', 'args': ['file'], 'kwargs': ['layer', 'height', 'center', 'convexity', 'twist', 'slices']} ,
--         | Projection', 'args': [], 'kwargs': ['cut']} ,
         | Surface         String R3 N -- file, center, convexity
         | ECSG
        deriving Show

data CsgM a = CsgM {lst :: [CSG], val :: a}

runCM :: CsgM a -> a
runCM m = val m

instance Monad (CsgM) where
  csgm >>= f = CsgM (lst csgm ++ [evl]) evl
    where evl = (val.f.val) csgm
  return x = CsgM [] x
-- \x -> a ++ [f a]

(+) (Union a) (Union b) = Union (b ++ a)
(+) (Union a)        b  = Union (b : a)
(+)        a  (Union b) = Union (a : b)
(+)        a         b  = Union [a, b]

(-) (Diff a) (Diff b)  = Diff (b ++ a)
(-) (Diff a)        b  = Diff (b : a)
(-)        a  (Diff b) = Diff (a : b)
(-)        a        b  = Diff [a, b]

(/) (Intersection a) (Intersection b) = Intersection (b ++ a)
(/) (Intersection a)               b  = Intersection (b : a)
(/)               a  (Intersection b) = Intersection (a : b)
(/)               a                b  = Intersection [a, b]

translate r3 = \csg -> Translate r3 csg
sphere r r3  = \csg -> Sphere r3 csg
cylinder r r3  = \csg -> Cylinder r3 csg
cube r r3  = \csg -> Cube r3 csg


--main = do
--  translate (1.0, 2.3, 0.0) $ runCM $ sphere 1.0 (0.1,0.3,0.5) >>= cube 0.4 (-4, 0.0, 2.4)

--  translate (1.0, 2.3, 0.0) $ do
--    sphere 1.0 (0.1,0.3,0.5)
--    cube 0.4 (-4, 0.0, 2.4)
--
