module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    ) where

import Data.List( mapAccumL, sortBy )
import Graphics.Rasterific.Types

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

rasterize :: Rasterizable el => [el] -> [CoverageSpan]
rasterize = combineEdgeSamples . sortBy xy . concatMap decompose
  where xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)

