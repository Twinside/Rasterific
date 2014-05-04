{-# LANGUAGE BangPatterns #-}
module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    ) where

import Control.Monad.ST( runST )
import Data.Fixed( mod' )
import Data.Foldable( foldMap )
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Line
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VS

data CoverageSpan = CoverageSpan
    { _coverageX      :: {-# UNPACK #-} !Float
    , _coverageY      :: {-# UNPACK #-} !Float
    , _coverageVal    :: {-# UNPACK #-} !Float
    , _coverageLength :: {-# UNPACK #-} !Float
    }
    deriving Show

combineEdgeSamples :: (Float -> Float) -> V.Vector EdgeSample
                   -> [CoverageSpan]
{-# INLINE combineEdgeSamples #-}
combineEdgeSamples prepareCoverage vec = go 0 0 0 0 0
  where
    !maxi = V.length vec
    go !ix !x !y !a !_h | ix >= maxi = [CoverageSpan x y (prepareCoverage a) 1]
    go !ix !x !y !a !h = sub (vec `V.unsafeIndex` ix) where
      sub (EdgeSample x' y' a' h')
        | y == y' && x == x' = go (ix + 1) x' y' (a + a') (h + h')
        | y == y' = p1 : p2 : go (ix + 1) x' y' (h + a') (h + h')
        | otherwise =
           CoverageSpan x y (prepareCoverage a) 1 : go (ix + 1) x' y' a' h'
             where p1 = CoverageSpan x y (prepareCoverage a) 1
                   p2 = CoverageSpan (x + 1) y (prepareCoverage h) (x' - x - 1)

decompose :: Primitive -> Container EdgeSample
decompose (LinePrim l) = decomposeLine l
decompose (BezierPrim b) = decomposeBeziers b
decompose (CubicBezierPrim c) = decomposeCubicBeziers c

sortEdgeSamples :: [EdgeSample] -> V.Vector EdgeSample
sortEdgeSamples samples = runST $ do
    mutableVector <- V.unsafeThaw $ V.fromList samples
    let xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)
    VS.sortBy xy mutableVector
    V.unsafeFreeze mutableVector

rasterize :: FillMethod -> Container Primitive -> [CoverageSpan]
rasterize method = 
  case method of
    FillWinding -> combineEdgeSamples combineWinding 
                        . sortEdgeSamples
                        . listOfContainer
                        . foldMap decompose
    FillEvenOdd -> combineEdgeSamples combineEvenOdd
                        . sortEdgeSamples
                        . listOfContainer
                        . foldMap decompose
  where combineWinding = min 1 . abs
        combineEvenOdd cov = abs $ abs (cov - 1) `mod'` 2 - 1

