module Graphics.Rasterific.Bezier( Bezier( .. ), clipBezier ) where

import Linear( V2( .. ), (^-^) )
import Data.Monoid( mappend )
import Graphics.Rasterific.Operators

data Bezier = Bezier !Point !Point !Point
  deriving Show

clipBezier :: Point -> Point -> Bezier -> [Bezier]
clipBezier mini maxi bezier@(Bezier a b c)
    | insideX && insideY = [bezier]
    | outsideX || outsideY =
        [Bezier clampedA (clampedA `midPoint` clampedC) clampedC]
    | otherwise =
        recurse (Bezier m (b `midPoint`c) c) `mappend`
            recurse (Bezier a (a `midPoint` b) m)
  where bmin = vmin a $ vmin b c
        bmax = vmax a $ vmax b c

        recurse = clipBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedC = clamper c

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)
        edgeSeparator = vabs (abbc ^-^ mini) ^<^ vabs (abbc ^-^ maxi)
        edge = vpartition edgeSeparator mini maxi
        m = vpartition (vabs (abbc ^-^ edge) ^< 0.1) edge abbc

