module Graphics.Rasterific.Rasterize
     where

import Control.Applicative( (<$>), (<*>) )
import Data.List( mapAccumL, sortBy )

import Linear( V2( .. ), V1( .. ), Additive( .. ), (^-^) )
import Graphics.Rasterific.Bezier
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types

decomposeBeziers :: Bezier -> [EdgeSample]
decomposeBeziers (Bezier a@(V2 ax ay) b c@(V2 cx cy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise =
        decomposeBeziers (Bezier m (b `midPoint` c) c) ++
            decomposeBeziers (Bezier a (a `midPoint` b) m)
  where floorA = vfloor a
        floorC = vfloor c
        V2 px py  = fromIntegral <$> vmin floorA floorC
        V1 w = (px + 1 -) <$>  (V1 cx `midPoint` V1 ax)
        h = cy - ay

        V2 insideX insideY =
            floorA ^==^ floorC ^||^ vceil a ^==^ vceil c

        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)
        mini = fromIntegral <$> vfloor abbc
        maxi = fromIntegral <$> vceil abbc
        nearmin = vabs (abbc ^-^ mini) ^< 0.1
        nearmax = vabs (abbc ^-^ maxi) ^< 0.1

        minMaxing mi nearmi ma nearma p
          | nearmi = mi
          | nearma = ma
          | otherwise = p

        m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax <*> abbc

data CoverageSpan = CoverageSpan
    { _coverageX      :: !Float
    , _coverageY      :: !Float
    , _coverageVal    :: !Float
    , _coverageLength :: !Float
    }
    deriving Show

combineEdgeSamples :: [EdgeSample] -> [CoverageSpan]
combineEdgeSamples = append . mapAccumL go (0, 0, 0, 0)
  where append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan x y (min 1 $ abs a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x', y', a + a', h + h'), [])
          | y == y' = ((x', y', h + a', h + h'), [p1, p2])
          | otherwise =
             ((x', y', a', h'), [CoverageSpan x y (min 1 $ abs a) 1])
               where p1 = CoverageSpan x y (min 1 $ abs a) 1
                     p2 = CoverageSpan (x + 1) y (min 1 $ abs h) (x' - x - 1)

rasterizeBezier :: [Bezier] -> [CoverageSpan]
rasterizeBezier = combineEdgeSamples . sortBy xy . concatMap decomposeBeziers
  where xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)

