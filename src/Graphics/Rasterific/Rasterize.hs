{-# LANGUAGE BangPatterns #-}
module Graphics.Rasterific.Rasterize
    ( CoverageSpan( .. )
    , rasterize
    , toOpaqueCoverage
    , clip
    ) where

import Control.Monad.ST( runST )
import Data.Fixed( mod' )
import Data.Monoid( Endo( Endo, appEndo ) )
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

toOpaqueCoverage :: CoverageSpan -> CoverageSpan
{-# INLINE toOpaqueCoverage #-}
toOpaqueCoverage coverage = coverage { _coverageVal = 1 }

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

-- | Clip the geometry to a rectangle.
clip :: Point     -- ^ Minimum point (corner upper left)
     -> Point     -- ^ Maximum point (corner bottom right)
     -> Primitive -- ^ Primitive to be clipped
     -> Container Primitive
clip mini maxi (LinePrim l) = clipLine mini maxi l
clip mini maxi (BezierPrim b) = clipBezier mini maxi b
clip mini maxi (CubicBezierPrim c) = clipCubicBezier mini maxi c

decompose :: Primitive -> Producer EdgeSample
decompose (LinePrim l) = decomposeLine l
decompose (BezierPrim b) = decomposeBeziers b
decompose (CubicBezierPrim c) =
    {-decomposeCubicBezierForwardDifference c-}
    decomposeCubicBeziers c

xyCompare :: EdgeSample -> EdgeSample -> Ordering
{-# INLINE xyCompare #-}
xyCompare !(EdgeSample { _sampleY = ay, _sampleX = ax })
          !(EdgeSample { _sampleY = by, _sampleX = bx }) =
  case compare ay by of
    EQ -> compare ax bx
    c -> c

sortEdgeSamples :: [EdgeSample] -> V.Vector EdgeSample
sortEdgeSamples samples = runST $ do
    -- Resist the urge to make this a storable vector,
    -- it is actually a pessimisation.
    mutableVector <- V.unsafeThaw $ V.fromList samples
    VS.sortBy xyCompare mutableVector
    V.unsafeFreeze mutableVector

rasterize :: FillMethod -> Container Primitive -> [CoverageSpan]
rasterize method = 
  case method of
    FillWinding -> combineEdgeSamples combineWinding 
                        . sortEdgeSamples
                        . (($ []) . appEndo)
                        . foldMap (Endo . decompose)
    FillEvenOdd -> combineEdgeSamples combineEvenOdd
                        . sortEdgeSamples
                        . (($ []) . appEndo)
                        . foldMap (Endo . decompose)
  where combineWinding = min 1 . abs
        combineEvenOdd cov = abs $ abs (cov - 1) `mod'` 2 - 1

