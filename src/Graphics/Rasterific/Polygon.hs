module Graphics.Rasterific.Polygon
    ( Polygon
    , strokizePolygon
    ) where

import Control.Applicative( (<$>) )
import Graphics.Rasterific.Bezier
import Graphics.Rasterific.Types

type Polygon = Bezier

polygonizePoints :: [Point] -> [Bezier]
polygonizePoints [] = []
polygonizePoints lst@(_:rest) =
    uncurry straightLine <$> zip lst rest

strokizePolygon :: Float -> Float -> (Caping, Caping) -> [Point]
                -> [Polygon]
strokizePolygon width l caping =
    strokizeBezierPath width l caping . polygonizePoints

