module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    ) where

import Data.Fixed( mod' )
import Data.Foldable( foldMap )
import Data.List( mapAccumL, sortBy )
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier

data CoverageSpan = CoverageSpan
    { _coverageX      :: {-# UNPACK #-} !Float
    , _coverageY      :: {-# UNPACK #-} !Float
    , _coverageVal    :: {-# UNPACK #-} !Float
    , _coverageLength :: {-# UNPACK #-} !Float
    }
    deriving Show

combineEdgeSamples :: (Float -> Float) -> [EdgeSample] -> [CoverageSpan]
{-# INLINE combineEdgeSamples #-}
combineEdgeSamples prepareCoverage = append . mapAccumL go (0, 0, 0, 0)
  where append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan x y (prepareCoverage a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x', y', a + a', h + h'), [])
          | y == y' = ((x', y', h + a', h + h'), [p1, p2])
          | otherwise =
             ((x', y', a', h'), [CoverageSpan x y (prepareCoverage a) 1])
               where p1 = CoverageSpan x y (prepareCoverage a) 1
                     p2 = CoverageSpan (x + 1) y (prepareCoverage h) (x' - x - 1)

decompose :: Primitive -> Container EdgeSample
decompose (LinePrim (Line x1 x2)) = decomposeBeziers $ straightLine x1 x2
decompose (BezierPrim b) = decomposeBeziers b
decompose (CubicBezierPrim c) = decomposeCubicBeziers c

rasterize :: FillMethod -> [Primitive] -> [CoverageSpan]
rasterize method = 
  case method of
    FillWinding -> combineEdgeSamples combineWinding 
                        . sortBy xy
                        . listOfContainer
                        . foldMap decompose
    FillEvenOdd -> combineEdgeSamples combineEvenOdd
                        . sortBy xy
                        . listOfContainer
                        . foldMap decompose
  where xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)
        combineWinding = min 1 . abs
        combineEvenOdd cov = abs $ abs (cov - 1) `mod'` 2 - 1

