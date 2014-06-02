{-# LANGUAGE BangPatterns #-}
-- | Handle straight lines polygon.
module Graphics.Rasterific.Line
    ( lineFromPath
    , decomposeLine
    , clipLine
    , sanitizeLine
    , lineBreakAt
    , flattenLine
    , lineLength
    , offsetLine
    ) where

import Control.Applicative( (<$>), (<*>), pure )
import Data.Monoid( (<>), mempty )
import Graphics.Rasterific.Linear
             ( V1( .. )
             , V2( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , lerp
             , norm )

import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types

-- | Transform a list a point to a list of lines
--
-- > lineFromPath [a, b, c, d] = [Line a b, Line b c, Line c d]
--
lineFromPath :: [Point] -> [Line]
lineFromPath [] = []
lineFromPath lst@(_:rest) =
    uncurry Line <$> zip lst rest

lineLength :: Line -> Float
lineLength (Line a b) = norm (b ^-^ a)

sanitizeLine :: Line -> Container Primitive
sanitizeLine l@(Line p1 p2)
  | p1 `isNearby` p2 = mempty
  | otherwise = pure $ LinePrim l

lineBreakAt :: Line -> Float -> (Line, Line)
lineBreakAt (Line a b) t = (Line a ab, Line ab b)
  where ab = lerp t a b

flattenLine :: Line -> Container Primitive
flattenLine = pure . LinePrim

offsetLine :: Float -> Line -> Container Primitive
offsetLine offset (Line a b) = pure . LinePrim $ Line shiftedA shiftedB
  where
   u = a `normal` b
   shiftedA = a ^+^ (u ^* offset)
   shiftedB = b ^+^ (u ^* offset)

-- | Clamp the bezier curve inside a rectangle
-- given in parameter.
clipLine :: Point     -- ^ Point representing the "minimal" point for cliping
         -> Point     -- ^ Point representing the "maximal" point for cliping
         -> Line      -- ^ The line
         -> Container Primitive
clipLine mini maxi poly@(Line a b)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure . LinePrim $ poly
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY = pure . LinePrim $ Line clampedA clampedB

    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise = recurse (Line a m) <> recurse (Line m b)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a b
        bmax = vmax a b

        recurse = clipLine mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedB = clamper b

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        -- A X-----X-----X B
        --        AB
        ab = (a `midPoint` b)

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
            vabs (ab ^-^ mini) ^<^ vabs (ab ^-^ maxi)

        -- So here we 'solidify' the nearest edge position
        -- in an edge vector.
        edge = vpartition edgeSeparator mini maxi

        -- If we're near an edge, snap the component to the
        -- edge.
        m = vpartition (vabs (ab ^-^ edge) ^< 0.1) edge ab

-- TODO: implement better algorithm for lines, should
-- be doable.
decomposeLine :: Line -> Container EdgeSample
decomposeLine (Line aRoot bRoot) = go aRoot bRoot where
  go !a@(V2 ax ay) !b@(V2 bx by)
    | insideX && insideY = pure $ EdgeSample (px + 0.5) (py + 0.5) (w * h) h
      where
        !floorA = vfloor a
        !floorB = vfloor b
        !(V2 insideX insideY) =
            floorA ^==^ floorB ^||^ vceil a ^==^ vceil b

        !(V2 px py)  = fromIntegral <$> vmin floorA floorB
        !(V1 w) = (px + 1 -) <$>  (V1 bx `midPoint` V1 ax)
        !h = by - ay

  go a b = go a m <> go m b
    where
      !ab = a `midPoint` b

      !mini = fromIntegral <$> vfloor ab
      !maxi = fromIntegral <$> vceil ab
      !nearmin = vabs (ab ^-^ mini) ^< 0.1
      !nearmax = vabs (ab ^-^ maxi) ^< 0.1

      minMaxing mi nearmi ma nearma p
        | nearmi = mi
        | nearma = ma
        | otherwise = p

      !m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax <*> ab

