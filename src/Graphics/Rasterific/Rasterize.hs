module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    ) where

import Data.Fixed( mod' )
import Data.List( mapAccumL, sortBy )
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier

data CoverageSpan = CoverageSpan
    { _coverageX      :: !Float
    , _coverageY      :: !Float
    , _coverageVal    :: !Float
    , _coverageLength :: !Float
    }
    deriving Show

combineEdgeSamples :: [EdgeSample] -> [CoverageSpan]
combineEdgeSamples = append . mapAccumL go (0, 0, 0, 0)
  where prepareCoverage = min 1 . abs
        append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan x y (prepareCoverage a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x', y', a + a', h + h'), [])
          | y == y' = ((x', y', h + a', h + h'), [p1, p2])
          | otherwise =
             ((x', y', a', h'), [CoverageSpan x y (prepareCoverage a) 1])
               where p1 = CoverageSpan x y (prepareCoverage a) 1
                     p2 = CoverageSpan (x + 1) y (prepareCoverage h) (x' - x - 1)

combineEdgeSamplesEvenOdd :: [EdgeSample] -> [CoverageSpan]
combineEdgeSamplesEvenOdd = append . mapAccumL go (0, 0, 0, 0) -- . (\a -> trace (unlines $ map show a) a)
  where prepareCoverage cov = abs $ cov `mod'` 2
        alphaCoverage = min 1 . abs

        append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan x y (alphaCoverage a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x', y', a + a', h + h'), [])
          | y == y' = ((x', y', h + a', h + h'), [p1, p2])
          | otherwise =
             ((x', y', a', h'), [CoverageSpan x y (alphaCoverage a) 1])
               where nextZoneCoverage = prepareCoverage h
                     prevZoneCoverage
                        | nextZoneCoverage == 0 && h /= 0 = alphaCoverage $ a `mod'` 2
                        | otherwise = alphaCoverage a
                     p1 = CoverageSpan x y prevZoneCoverage 1
                     p2 = CoverageSpan (x + 1) y nextZoneCoverage (x' - x - 1)

decompose :: Primitive -> [EdgeSample]
decompose (LinePrim (Line x1 x2)) = decomposeBeziers $ straightLine x1 x2
decompose (BezierPrim b) = decomposeBeziers b
decompose (CubicBezierPrim c) = decomposeCubicBeziers c

rasterize :: FillMethod -> [Primitive] -> [CoverageSpan]
rasterize method = 
  case method of
    FillWinding -> combineEdgeSamples . sortBy xy . concatMap decompose
    FillEvenOdd -> combineEdgeSamplesEvenOdd . sortBy xy . concatMap decompose
  where xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)

