{-# LANGUAGE TypeFamilies #-}
-- | Handle straight lines polygon.
module Graphics.Rasterific.Polygon
    ( Polygon( .. )
    , polygonFromPath
    ) where

import Control.Applicative( Applicative, (<$>), pure )
import Data.Monoid( Monoid, (<>) )
import Linear( V2( .. ), (^-^) )

import Graphics.Rasterific.Operators
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.Types

data Polygon = Polygon !Point !Point

polygonFromPath :: [Point] -> [Polygon]
polygonFromPath [] = []
polygonFromPath lst@(_:rest) =
    uncurry Polygon <$> zip lst rest

quadraticBezierOfPolygon :: Polygon -> Bezier
quadraticBezierOfPolygon (Polygon a b) = a `straightLine` b

instance Strokable Polygon where
  type RenderType Polygon = Bezier
  strokize width join cap =
    strokize width join cap . fmap quadraticBezierOfPolygon

instance Rasterizable Polygon where
    clip = clipPolygon
    decompose (Polygon a b) =
        decompose $ a `straightLine` b

-- | Clamp the bezier curve inside a rectangle
-- given in parameter.
clipPolygon :: (Applicative a, Monoid (a Polygon))
             => Point     -- ^ Point representing the "minimal" point for cliping
             -> Point     -- ^ Point representing the "maximal" point for cliping
             -> Polygon    -- ^ The line
             -> a Polygon
clipPolygon mini maxi poly@(Polygon a b)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure poly
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY = pure $ Polygon clampedA clampedB

    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise = recurse (Polygon m b) <> recurse (Polygon a m)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a b
        bmax = vmax a b

        recurse = clipPolygon mini maxi

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

