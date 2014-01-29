{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module handling math regarding the handling of quadratic
-- and cubic bezier curve.
module Graphics.Rasterific.QuadraticBezier
    ( -- * Helper functions
      straightLine
    , bezierFromPath
    , decomposeBeziers
    , clipBezier
    , sanitizeBezier
    , offsetBezier
    , flattenBezier
    , bezierBreakAt
    ) where

import Control.Applicative( (<$>)
                          , (<*>)
                          , Applicative
                          , pure )
import Linear( V2( .. )
             , V1( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , dot
             , norm
             )
import Data.Monoid( Monoid( mempty ), (<>) )
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types

-- | Create a list of bezier patch from a list of points,
--
-- > bezierFromPath [a, b, c, d, e] == [Bezier a b c, Bezier c d e]
-- > bezierFromPath [a, b, c, d, e, f] == [Bezier a b c, Bezier c d e]
-- > bezierFromPath [a, b, c, d, e, f, g] ==
-- >     [Bezier a b c, Bezier c d e, Bezier e f g]
--
bezierFromPath :: [Point] -> [Bezier]
bezierFromPath (a:b:rest@(c:_)) = Bezier a b c : bezierFromPath rest
bezierFromPath _ = []

decomposeBeziers :: Bezier -> [EdgeSample]
decomposeBeziers (Bezier a@(V2 ax ay) b c@(V2 cx cy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise = recurse (Bezier m bc c) <> recurse (Bezier a ab m)
  where floorA = vfloor a
        floorC = vfloor c
        V2 px py  = fromIntegral <$> vmin floorA floorC
        V1 w = (px + 1 -) <$>  (V1 cx `midPoint` V1 ax)
        h = cy - ay

        recurse = decomposeBeziers

        V2 insideX insideY =
            floorA ^==^ floorC ^||^ vceil a ^==^ vceil c

        ab = a `midPoint` b
        bc = b `midPoint` c
        abbc = ab `midPoint` bc

        mini = fromIntegral <$> vfloor abbc
        maxi = fromIntegral <$> vceil abbc
        nearmin = vabs (abbc ^-^ mini) ^< 0.1
        nearmax = vabs (abbc ^-^ maxi) ^< 0.1

        minMaxing mi nearmi ma nearma p
          | nearmi = mi
          | nearma = ma
          | otherwise = p

        m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax <*> abbc

-- | Create a quadratic bezier curve representing
-- a straight line.
straightLine :: Point -> Point -> Bezier
straightLine a c = Bezier a (a `midPoint` c) c

-- | Clamp the bezier curve inside a rectangle
-- given in parameter.
clipBezier :: Point     -- ^ Point representing the "minimal" point for cliping
           -> Point     -- ^ Point representing the "maximal" point for cliping
           -> Bezier    -- ^ The quadratic bezier curve to be clamped
           -> [Primitive]
clipBezier mini maxi bezier@(Bezier a b c)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure $ BezierPrim bezier
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY =
        pure . BezierPrim $ clampedA `straightLine` clampedC
    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise =
        recurse (Bezier m bc c) <> recurse (Bezier a ab m)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a $ vmin b c
        bmax = vmax a $ vmax b c

        recurse = clipBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedC = clamper c

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        --
        --         X B
        --        / \
        --       /   \
        --   ab X--X--X bc
        --     / abbc  \
        --    /         \
        -- A X           X C
        --
        ab = a `midPoint` b
        bc = b `midPoint` c
        abbc = ab `midPoint` bc

        --  mini
        --     +-------------+
        --     |             |
        --     |             |
        --     |             |
        --     +-------------+
        --                   maxi
        -- the edgeSeparator vector encode which edge
        -- is te nearest to the midpoint.
        -- if True then it's the 'min' edges which are
        -- the nearest, otherwise it's the maximum edge
        edgeSeparator =
            vabs (abbc ^-^ mini) ^<^ vabs (abbc ^-^ maxi)

        -- So here we 'solidify' the nearest edge position
        -- in an edge vector.
        edge = vpartition edgeSeparator mini maxi

        -- If we're near an edge, snap the component to the
        -- edge.
        m = vpartition (vabs (abbc ^-^ edge) ^< 0.1) edge abbc


-- | Rewrite the bezier curve to avoid degenerate cases.
sanitizeBezier :: (Applicative a, Monoid (a Primitive))
               => Bezier -> a Primitive
sanitizeBezier bezier@(Bezier a b c)
   -- If the two normals vector are far apart (cos nearly -1)
   --
   --       u           v
   -- <----------   ------------>
   -- because u dot v = ||u|| * ||v|| * cos(uv)
   --
   -- This imply that AB and BC are nearly parallel
   | u `dot` v < -0.9999 =
     -- divide in to halves with
     sanitizeBezier (Bezier abbc (abbc `midPoint` c) c) <>
        sanitizeBezier (Bezier a (a `midPoint` abbc) abbc)

   -- b is far enough of b and c, (it's not a point)
   | norm (a ^-^ b) > 0.0001 && norm (b ^-^ c) > 0.0001 =
       pure . BezierPrim $ bezier

   -- if b is to nearby a or c, take the midpoint as new reference.
   | ac /= b = sanitizeBezier (Bezier a ac c)
   | otherwise = mempty
  where u = a `normal` b
        v = b `normal` c
        ac = a `midPoint` c
        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)

bezierBreakAt :: Bezier -> Float -> (Bezier, Bezier)
bezierBreakAt (Bezier a b c) t = (Bezier a ab abbc, Bezier abbc bc c)
  where
    --         X B
    --        / \
    --       /   \
    --   ab X--X--X bc
    --     / abbc  \
    --    /         \
    -- A X           X C
    ab = lerpPoint a b t
    bc = lerpPoint b c t
    abbc = lerpPoint ab bc t

flattenBezier :: (Applicative a, Monoid (a Primitive))
              => Bezier -> a Primitive
flattenBezier bezier@(Bezier a b c)
    -- If the spline is not too curvy, just return the
    -- shifted component
    | u `dot` v >= 0.9 = pure $ BezierPrim bezier
    -- Otherwise, divide and conquer
    | a /= b && b /= c =
        flattenBezier (Bezier a ab abbc) <>
            flattenBezier (Bezier abbc bc c)
    | otherwise = mempty
  where --
        --         X B   
        --    ^   /^\   ^
        --   u \ /w| \ / v
        --      X-----X
        --     /       \
        --    /         \
        -- A X           X C
        --
        u = a `normal` b
        v = b `normal` c

        ab = (a `midPoint` b)
        bc = (b `midPoint` c)
        abbc = ab `midPoint` bc

-- | Move the bezier to a new position with an offset.
offsetBezier :: (Applicative a, Monoid (a Primitive))
             => Float -> Bezier -> a Primitive
offsetBezier offset (Bezier a b c)
    -- If the spline is not too curvy, just return the
    -- shifted component
    | u `dot` v >= 0.9 =
        pure . BezierPrim $ Bezier shiftedA mergedB shiftedC
    -- Otherwise, divide and conquer
    | a /= b && b /= c =
        offsetBezier offset (Bezier abbc bc c) <>
            offsetBezier offset (Bezier a ab abbc)
    | otherwise = mempty
  where --
        --         X B   
        --    ^   /^\   ^
        --   u \ /w| \ / v
        --      X-----X
        --     /       \
        --    /         \
        -- A X           X C
        --
        u = a `normal` b
        v = b `normal` c
        w = ab `normal` bc

        ab = (a `midPoint` b)
        bc = (b `midPoint` c)
        abbc = ab `midPoint` bc

        shiftedA = a ^+^ (u ^* offset)
        shiftedC = c ^+^ (v ^* offset)
        shiftedABBC = abbc ^+^ (w ^* offset)
        mergedB =
            (shiftedABBC ^* 2.0) ^-^ (shiftedA `midPoint` shiftedC)

