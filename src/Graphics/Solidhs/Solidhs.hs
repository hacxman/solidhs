module Graphics.Solidhs ( ) where

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
