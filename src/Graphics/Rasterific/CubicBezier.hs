{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Rasterific.CubicBezier
    ( cubicBezierCircle
    , cubicBezierFromPath
    , clipCubicBezier
    , decomposeCubicBeziers
    , sanitizeCubicBezier
    , offsetCubicBezier
    ) where

import Prelude hiding( or )
import Control.Applicative( Applicative
                          , (<$>)
                          , (<*>)
                          , pure
                          )
import Linear( V1( .. )
             , V2( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , dot
             , norm
             )
import Data.Monoid( Monoid, mempty, (<>) )
{-import Data.Foldable( Foldable, foldMap )-}
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types

import Debug.Trace
{-import Text.Printf-}

-- | Create a list of cubic bezier patch from a list of points.
--
-- > cubicBezierFromPath [a, b, c, d, e] = [CubicBezier a b c d]
-- > cubicBezierFromPath [a, b, c, d, e, f, g] =
-- >    [CubicBezier a b c d, CubicBezier d e f g]
--
cubicBezierFromPath :: [Point] -> [CubicBezier]
cubicBezierFromPath (a:b:c:rest@(d:_)) =
    CubicBezier a b c d : cubicBezierFromPath rest
cubicBezierFromPath _ = []

-- | Represent a circle of radius 1 centered on 0 of
-- a cubic bezier curve.
cubicBezierCircle :: [CubicBezier]
cubicBezierCircle =
    [ CubicBezier (V2 0 1) (V2 c 1) (V2 1 c) (V2 1 0)
    , CubicBezier (V2 1 0) (V2 1 (-c)) (V2 c (-1)) (V2 0 (-1))
    , CubicBezier (V2 0 (-1)) (V2 (-c) (-1)) (V2 (-1) (-c)) (V2 (-1) 0)
    , CubicBezier (V2 (-1) 0) (V2 (-1) c) (V2 (-c) 1) (V2 0 1)
    ]
  where c = 0.551915024494 -- magic constant? magic constant.

straightLine :: Point -> Point -> CubicBezier
straightLine a b = CubicBezier a p p b
  where p = a `midPoint` b

type BezierBasis = (Point, Point, Point, Point)

bezierBasis :: CubicBezier -> BezierBasis
bezierBasis (CubicBezier p0 p1 p2 p3) = (a, b, c, d)
  where a = negate p0 ^+^ (p1 ^-^ p2) ^* 3 + p3
        b = (p0 ^-^ p1 ^* 2 ^+^ p2) ^* 3
        c = (p1 ^-^ p0) ^* 3
        d = p0


--               3                    2            2                  3      
-- x(t) = (1 - t) ∙x     + 3∙t∙(1 - t) ∙x     + 3∙t ∙(1 - t)∙x     + t ∙x    
--                   0                    1                    2          3  
--
--               3                    2            2                  3      
-- y(t) = (1 - t) ∙y     + 3∙t∙(1 - t) ∙y     + 3∙t ∙(1 - t)∙y     + t ∙y    
--                   0                    1                    2          3  

cusp :: BezierBasis -> Float
cusp (V2 ax ay, V2 bx by, V2 cx cy, _) =
    - 0.5 * ( (ay * cx - ax * cy)
            / (ay * bx - ax * by) )

data Inflections
    = InflectionNone
    | InflectionOne Float
    | InflectionTwo Float Float
    deriving (Eq, Show)

inflectionPoints :: CubicBezier -> Inflections
inflectionPoints bez
    | val < 0     = InflectionNone
    | val < 0.001 = InflectionOne tCusp
    | otherwise   = InflectionTwo (tCusp - sqrt val) (tCusp + sqrt val)
  where
    basis@(V2 ax ay, V2 bx by, V2 cx cy, _) = bezierBasis bez
    val = (\a -> trace ("tCusp:" ++ show tCusp ++ "  val:" ++ show a) a) $ tCusp * tCusp * quadrature / 3
    tCusp = trace (show basis) $ cusp basis
    quadrature = (by * cx - bx * cy)
               / (ay * bx - ax * by)

offsetCurve :: (Applicative a, Monoid (a Primitive))
            => Float -> CubicBezier -> a Primitive
offsetCurve offset (CubicBezier a b c d)
    | u `dot` v >= 0.9 && u `dot` s >= 0.9 && s `dot` v >= 0.9 =
        pure . CubicBezierPrim $ CubicBezier shiftedA shiftedB shiftedC shiftedD
    | a /= b && b /= c && c /= d =
        offsetCurve offset  (CubicBezier a ab abbc abbcbccd) <>
            offsetCurve offset (CubicBezier abbcbccd bccd cd d)
    | otherwise = mempty
  where
    u = a `normal` b
    v = c `normal` d
    s = b `normal` c

    --                     BC
    --         B X----------X---------X C  
    --    ^     /      ___/   \___     \     ^
    --   u \   /   __X------X------X_   \   / v
    --      \ /___/ ABBC       BCCD  \___\ /
    --    AB X/                          \X CD
    --      /                              \
    --     /                                \
    --    /                                  \
    -- A X                                    X D
    ab = a `midPoint` b
    bc = b `midPoint` c
    cd = c `midPoint` d

    w = ab `normal` bc
    x = bc `normal` cd

    abbc = ab `midPoint` bc
    bccd = bc `midPoint` cd
    abbcbccd = abbc `midPoint` bccd

    shiftedA = a ^+^ (u ^* offset)
    shiftedD = d ^+^ (v ^* offset)

    {-shiftedABBCBCCD = abbcbccd ^+^ (w ^* offset)-}
    shiftedB = (b ^+^ (w ^* offset))
    shiftedC = (c ^+^ (x ^* offset))

offsetCubicBezier :: (Applicative a, Monoid (a Primitive))
                  => Float -> CubicBezier -> a Primitive
offsetCubicBezier offsetVal bezier = go . (\a -> trace (show a) a) $ inflectionPoints bezier
  where
    offset = offsetCurve offsetVal

    go InflectionNone = offset bezier
    go (InflectionOne f) = offset a <> offset b
        where (a, b) = cubicBezierBreakAt bezier f
    go (InflectionTwo f1 f2) = offset a <> offset b' <> offset c
        where (a, b) = cubicBezierBreakAt bezier f1
              (b', c) = cubicBezierBreakAt b $ (f2 - f1) / (1 - f1)

-- | Clamp the cubic bezier curve inside a rectangle
-- given in parameter.
clipCubicBezier
    :: Point   -- ^ Point representing the "minimal" point for cliping
    -> Point  -- ^ Point representing the "maximal" point for cliping
    -> CubicBezier -- ^ The cubic bezier curve to be clamped
    -> [Primitive]
clipCubicBezier mini maxi bezier@(CubicBezier a b c d)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure $ CubicBezierPrim bezier
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY =
        pure . CubicBezierPrim $ clampedA `straightLine` clampedD
    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise =
        recurse (CubicBezier m bccd cd d) <>
            recurse (CubicBezier a ab abbc m)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a . vmin b $ vmin c d
        bmax = vmax a . vmax b $ vmin c d

        recurse = clipCubicBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedD = clamper d

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        --                     BC
        --         B X----------X---------X C  
        --          /      ___/   \___     \   
        --         /   __X------X------X_   \  
        --        /___/ ABBC       BCCD  \___\ 
        --    AB X/                          \X CD
        --      /                              \
        --     /                                \
        --    /                                  \
        -- A X                                    X D
        ab = a `midPoint` b
        bc = b `midPoint` c
        cd = c `midPoint` d

        abbc = ab `midPoint` bc
        bccd = bc `midPoint` cd
        abbcbccd = abbc `midPoint` bccd

        edgeSeparator = vabs (abbcbccd ^-^ mini) ^<^ vabs (abbcbccd ^-^ maxi)
        edge = vpartition edgeSeparator mini maxi
        m = vpartition (vabs (abbcbccd ^-^ edge) ^< 0.1) edge abbc

-- | Will subdivide the bezier from 0 to coeff and coeff to 1
cubicBezierBreakAt :: CubicBezier -> Float -> (CubicBezier, CubicBezier)
cubicBezierBreakAt (CubicBezier a b c d) val =
    (CubicBezier a ab abbc abbcbccd, CubicBezier abbcbccd bccd cd d)
  where
    ab = lerpPoint a b val
    bc = lerpPoint b c val
    cd = lerpPoint c d val

    abbc = lerpPoint ab bc val
    bccd = lerpPoint bc cd val
    abbcbccd = lerpPoint abbc bccd val

decomposeCubicBeziers :: CubicBezier -> [EdgeSample]
decomposeCubicBeziers (CubicBezier a@(V2 ax ay) b c d@(V2 dx dy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise =
        recurse (CubicBezier m bccd cd d) <>
            recurse (CubicBezier a ab abbc m)
  where recurse = decomposeCubicBeziers
        floorA = vfloor a
        floorD = vfloor d
        V2 px py  = fromIntegral <$> vmin floorA floorD
        V1 w = (px + 1 -) <$>  (V1 dx `midPoint` V1 ax)
        h = dy - ay

        V2 insideX insideY =
            floorA ^==^ floorD ^||^ vceil a ^==^ vceil d

        --                     BC
        --         B X----------X---------X C  
        --          /      ___/   \___     \   
        --         /   __X------X------X_   \  
        --        /___/ ABBC       BCCD  \___\ 
        --    AB X/                          \X CD
        --      /                              \
        --     /                                \
        --    /                                  \
        -- A X                                    X D
        ab = a `midPoint` b
        bc = b `midPoint` c
        cd = c `midPoint` d
        abbc = ab `midPoint` bc
        bccd = bc `midPoint` cd

        abbcbccd = abbc `midPoint` bccd

        mini = fromIntegral <$> vfloor abbcbccd
        maxi = fromIntegral <$> vceil abbcbccd
        nearmin = vabs (abbcbccd ^-^ mini) ^< 0.1
        nearmax = vabs (abbcbccd ^-^ maxi) ^< 0.1

        minMaxing mi nearmi ma nearma p
          | nearmi = mi
          | nearma = ma
          | otherwise = p

        m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax
                      <*> abbcbccd

sanitizeCubicBezier :: (Applicative a, Monoid (a Primitive))
                    => CubicBezier -> a Primitive
sanitizeCubicBezier bezier@(CubicBezier a b c d)
  | norm (a ^-^ b) > 0.0001 &&
        norm (b ^-^ c) > 0.0001 && 
        norm (c ^-^ d) > 0.0001 =
       pure . CubicBezierPrim $ bezier
  | ac /= b && bd /= c =
      pure . CubicBezierPrim $ CubicBezier a ac bd d
  | ac /= b = 
      pure . CubicBezierPrim $ CubicBezier a ac c d
  | bd /= c = 
      pure . CubicBezierPrim $ CubicBezier a b bd d
  | otherwise = mempty
    where ac = a `midPoint` c
          bd = a `midPoint` d

