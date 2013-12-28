-- | Handle straight lines polygon.
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

strokizePolygon :: Float -> Join -> (Cap, Cap) -> [Point]
                -> [Polygon]
strokizePolygon width join caping =
    strokizeBezierPath width join caping . polygonizePoints

