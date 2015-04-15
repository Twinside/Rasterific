{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
    , bezierLengthApproximation
    , isBezierPoint
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure )
import Data.Monoid( Monoid( mempty ) )
#endif
import Graphics.Rasterific.Linear
             ( V2( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , dot
             , norm
             , lerp
             )

import Data.Monoid( (<>) )
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

isBezierPoint :: Bezier -> Bool
isBezierPoint (Bezier a b c) =
  not $ a `isDistingableFrom` b || 
        b `isDistingableFrom` c

-- | Only work if the quadratic bezier curve
-- is nearly flat
bezierLengthApproximation :: Bezier -> Float
bezierLengthApproximation (Bezier a _ c) =
    norm $ c ^-^ a

decomposeBeziers :: Bezier -> Producer EdgeSample
decomposeBeziers (Bezier (V2 aRx aRy) (V2 bRx bRy) (V2 cRx cRy)) =
    go aRx aRy bRx bRy cRx cRy where
  go ax ay _bx _by cx cy cont
    | insideX && insideY =
      let !px = fromIntegral $ min floorAx floorCx
          !py = fromIntegral $ min floorAy floorCy
          !w = px + 1 - cx `middle` ax
          !h = cy - ay
      in
      EdgeSample (px + 0.5) (py + 0.5) (w * h) h : cont
      where
        floorAx, floorAy :: Int
        !floorAx = floor ax
        !floorAy = floor ay

        !floorCx = floor cx
        !floorCy = floor cy

        !insideX = floorAx == floorCx || ceiling ax == (ceiling cx :: Int)
        !insideY = floorAy == floorCy || ceiling ay == (ceiling cy :: Int)


  go !ax !ay !bx !by !cx !cy cont =
      go ax ay abx aby mx my $ go mx my bcx bcy cx cy cont
    where
      !abx = ax `middle` bx
      !aby = ay `middle` by

      !bcx = bx `middle` cx
      !bcy = by `middle` cy

      !abbcx = abx `middle` bcx
      !abbcy = aby `middle` bcy

      !mx | abs (abbcx - mini) < 0.1 = mini
          | abs (abbcx - maxi) < 0.1 = maxi
          | otherwise = abbcx
         where !mini = fromIntegral (floor abbcx :: Int)
               !maxi = fromIntegral (ceiling abbcx :: Int)

      !my | abs (abbcy - mini) < 0.1 = mini
          | abs (abbcy - maxi) < 0.1 = maxi
          | otherwise = abbcy
         where !mini = fromIntegral (floor abbcy :: Int)
               !maxi = fromIntegral (ceiling abbcy :: Int)


-- | Create a quadratic bezier curve representing
-- a straight line.
straightLine :: Point -> Point -> Bezier
straightLine a c = Bezier a (a `midPoint` c) c

-- | Clamp the bezier curve inside a rectangle
-- given in parameter.
clipBezier :: Point     -- ^ Point representing the "minimal" point for cliping
           -> Point     -- ^ Point representing the "maximal" point for cliping
           -> Bezier    -- ^ The quadratic bezier curve to be clamped
           -> Container Primitive
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
        recurse (Bezier a ab m) <>
            recurse (Bezier m bc c)
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
        (ab, bc, abbc) = splitBezier bezier

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
sanitizeBezier :: Bezier -> Container Primitive
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
    sanitizeBezier (Bezier a (a `midPoint` abbc) abbc) <>
        sanitizeBezier (Bezier abbc (abbc `midPoint` c) c)

   -- b is far enough of b and c, (it's not a point)
   | a `isDistingableFrom` b && b `isDistingableFrom` c =
       pure . BezierPrim $ bezier

   -- if b is to nearby a or c, take the midpoint as new reference.
   | ac `isDistingableFrom` b = sanitizeBezier (Bezier a ac c)
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
    ab = lerp t a b
    bc = lerp t b c
    abbc = lerp t ab bc

splitBezier :: Bezier -> (Point, Point, Point)
{-# INLINE splitBezier #-}
splitBezier (Bezier a b c) = (ab, bc, abbc)
  where
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

flattenBezier :: Bezier -> Container Primitive
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

        (ab, bc, abbc) = splitBezier bezier

-- | Move the bezier to a new position with an offset.
offsetBezier :: Float -> Bezier -> Container Primitive
offsetBezier offset bezier@(Bezier a b c)
    -- If the spline is not too curvy, just return the
    -- shifted component
    | u `dot` v >= 0.9 =
        pure . BezierPrim $ Bezier shiftedA mergedB shiftedC
    -- Otherwise, divide and conquer
    | a /= b && b /= c =
        offsetBezier offset (Bezier a ab abbc) <>
            offsetBezier offset (Bezier abbc bc c)
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

        (ab, bc, abbc) = splitBezier bezier

        shiftedA = a ^+^ (u ^* offset)
        shiftedC = c ^+^ (v ^* offset)
        shiftedABBC = abbc ^+^ (w ^* offset)
        mergedB =
            (shiftedABBC ^* 2.0) ^-^ (shiftedA `midPoint` shiftedC)

