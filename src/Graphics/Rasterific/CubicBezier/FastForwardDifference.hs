{-# LANGUAGE BangPatterns #-}
module Graphics.Rasterific.CubicBezier.FastForwardDifference
    ( ForwardDifferenceCoefficient( .. )
    , bezierToForwardDifferenceCoeff
    , decomposeCubicBezierForwardDifference
    , estimateFDStepCount
    ) where

import Control.Applicative( pure )
import Data.Monoid( mempty, (<>) )
import Data.Bits( unsafeShiftL )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Linear ( V2( .. ), (^-^), (^+^), (^*), qd )

data ForwardDifferenceCoefficient = ForwardDifferenceCoefficient
    { _fdA :: {-# UNPACK #-} !Float
    , _fdB :: {-# UNPACK #-} !Float
    , _fdC :: {-# UNPACK #-} !Float
    }

-- | Given a cubic curve, return the initial step size and
-- the coefficient for the forward difference.
-- Initial step is assumed to be "1"
bezierToForwardDifferenceCoeff
    :: CubicBezier
    -> (ForwardDifferenceCoefficient, ForwardDifferenceCoefficient)
bezierToForwardDifferenceCoeff (CubicBezier x y z w) = (xCoeffs, yCoeffs)
  where
    xCoeffs = ForwardDifferenceCoefficient { _fdA = ax, _fdB = bx, _fdC = cx }
    yCoeffs = ForwardDifferenceCoefficient { _fdA = ay, _fdB = by, _fdC = cy }

    V2 ax ay = w ^-^ x
    V2 bx by = (w ^-^ z ^* 2 ^+^ y) ^* 6
    V2 cx cy = (w ^-^ z ^* 3 ^+^ y ^* 3 ^-^ x) ^* 6

{-
doubleFDCoefficients :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
doubleFDCoefficients (ForwardDifferenceCoefficient a b c) =
    ForwardDifferenceCoefficient { _fdA = a', _fdB = b', _fdC = c' }
  where
    a' = 8 * a
    b' = 4 * b + 4 * a
    c' = 2 * c + b
-- -}

halveFDCoefficients :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
halveFDCoefficients (ForwardDifferenceCoefficient a b c) =
    ForwardDifferenceCoefficient { _fdA = a', _fdB = b', _fdC = c' }
  where
    c' = c * 0.125
    b' = b * 0.25 - c'
    a' = (a - b') * 0.5

updateForwardDifferencing :: Float -> ForwardDifferenceCoefficient
                          -> (Float, ForwardDifferenceCoefficient)
updateForwardDifferencing v (ForwardDifferenceCoefficient a b c) =
    (v + a, ForwardDifferenceCoefficient (a + b) (b + c) c)

estimateFDStepCount :: CubicBezier -> Int
estimateFDStepCount (CubicBezier p0 p1 p2 p3) =
  toInt $ maximum [p0 `qd` p1, p2 `qd` p3, (p0 `qd` p2) / 4, (p1 `qd` p3) / 4]
  where
    toInt = scale . frexp . max 1 . (18 *)
    scale (_, r) = (r + 1) `div` 2


fixIter :: Int -> (a -> a) -> a -> a
fixIter count f = go count
  where
    go 0 a = a
    go n a = go (n-1) $ f a

{-
fractionalRest :: Float -> Float
fractionalRest v = v - fromIntegral (truncate v :: Int)
-- -}

decomposeCubicBezierForwardDifference :: CubicBezier -> Container EdgeSample
decomposeCubicBezierForwardDifference bez = pure initialSample <> lst where
  lst = go 0 xStartCoeff yStartCoeff integerStart initialVec initialIx
  initialSample = EdgeSample
    { _sampleX     = fromIntegral initialIx
    , _sampleY     = fromIntegral initialIy
    , _sampleAlpha = 1.0 -- For testing purposes only.
    , _sampleH     = 1.0
    }

  (xFull, yFull) = bezierToForwardDifferenceCoeff bez
  xStartCoeff = fixIter stepCount halveFDCoefficients xFull
  yStartCoeff = fixIter stepCount halveFDCoefficients yFull

  initialVec = _cBezierX0 bez
  integerStart@(V2 initialIx initialIy) = vfloor initialVec

  stepCount = estimateFDStepCount bez

  maxStepCount :: Int
  maxStepCount = 1 `unsafeShiftL` stepCount

  go currentStep _ _ (V2 px py) _ firstWrittenX
    | currentStep >= maxStepCount =
      if firstWrittenX == px then mempty
      else pure EdgeSample { _sampleX = fromIntegral px
                           , _sampleY = fromIntegral py
                           , _sampleAlpha = 1.0
                           , _sampleH = 1.0 }
  go !currentStep !xFdCoeff !yFdCoeff
     (V2 prevXpx prevYpx)
     (V2 xPrev yPrev)
     !lastWrittenX
    | prevYpx == integerNextY = rest lastWrittenX

    | prevXpx == lastWrittenX =
        pure nextSample <> rest integerNextX
    | otherwise =
        pure prevSample <> pure nextSample <> rest integerNextX
    where
      rest = go (currentStep + 1) xCoeff yCoeff nexti next

      next = V2 xNext yNext
      (xNext, xCoeff) = updateForwardDifferencing xPrev xFdCoeff
      (yNext, yCoeff) = updateForwardDifferencing yPrev yFdCoeff
      nexti@(V2 integerNextX integerNextY) = vfloor next :: V2 Int

      prevSample = EdgeSample
          { _sampleX     = fromIntegral prevXpx
          , _sampleY     = fromIntegral prevYpx
          , _sampleAlpha = 1.0 -- For testing purposes only.
          , _sampleH     = 1.0
          }

      nextSample = EdgeSample
          { _sampleX     = fromIntegral integerNextX
          , _sampleY     = fromIntegral integerNextY
          , _sampleAlpha = 1.0 -- For testing purposes only.
          , _sampleH     = 1.0
          }
      {-deltas@(V2 xDelta yDelta) = next ^-^ prev-}
      {-V2 xFrac yFrac = fractionalRest <$> deltas-}

frexp :: Float -> (Float, Int)
frexp x
   | isNaN x = error "NaN given to frexp"
   | isInfinite x = error "infinity given to frexp"
   | otherwise  = go x 0
  where
    go s e
      | s >= 1.0 = go (s / 2) (e + 1)
      | s < 0.5 = go (s * 2) (e - 1)
      | otherwise = (s, e)
