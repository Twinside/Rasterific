{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Rasterific.CubicBezier.FastForwardDifference
    ( ForwardDifferenceCoefficient( .. )
    , bezierToForwardDifferenceCoeff
    , estimateFDStepCount
    , rasterizerCubicBezier
    ) where

import Control.Monad.Primitive( PrimMonad )
import Control.Monad.State( lift, get )
import Control.Applicative( liftA2, pure )
import Data.Monoid( mempty, (<>)  )
import Data.Bits( shiftR, unsafeShiftL )

import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Types
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Linear
import Graphics.Rasterific.BiSampleable
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.Shading

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
    -> V2 ForwardDifferenceCoefficient
bezierToForwardDifferenceCoeff (CubicBezier x y z w) = V2 xCoeffs yCoeffs
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

stepSquare :: CubicBezier -> Float
stepSquare (CubicBezier a b c d) = maxi * 18 where
  maxi = max d1 . max d2 $ max d3 d4
  d1 = a `qd` b
  d2 = c `qd` d
  d3 = (a `qd` c) * 0.25
  d4 = (b `qd` d) * 0.25

rasterizerCubicBezier :: (PrimMonad m, ModulablePixel px, BiSampleable src px)
                      => src -> CubicBezier -> UV -> UV -> DrawContext m px ()
rasterizerCubicBezier source bez uvStart uvEnd = do
  canvas <- get
  let V2 xFull yFull = bezierToForwardDifferenceCoeff bez
      V2 xStart yStart = _cBezierX0 bez
      shiftCount = estimateFDStepCount bez

      maxStepCount :: Int
      maxStepCount = 1 `unsafeShiftL` shiftCount

      xFStart = fixIter shiftCount halveFDCoefficients xFull
      yFStart = fixIter shiftCount halveFDCoefficients yFull

      V2 du dv = (uvEnd ^-^ uvStart) ^/ fromIntegral maxStepCount
      
      
      go !currentStep _ _ _ _ _ _ 
        | currentStep >= maxStepCount = return ()
      go !currentStep !xFdCoeff !yFdCoeff !x !y !u !v = do
        let !(xNext, xCoeff) = updateForwardDifferencing x xFdCoeff
            !(yNext, yCoeff) = updateForwardDifferencing y yFdCoeff
            !color = interpolate source (V2 u v)
        plotPixel canvas color (floor x) (floor y)
        go (currentStep + 1) xCoeff yCoeff xNext yNext (u + du) (v + dv)

  lift $ go 0 xFStart yFStart xStart yStart du dv

rasterizeTensorPatch :: (PrimMonad m, ModulablePixel px, BiSampleable src px)
                     => TensorPatch src -> DrawContext m px ()
rasterizeTensorPatch TensorPatch { .. } = go maxStepCount basePoints ffCoeff uvStart uvEnd
  where
    uvStart = zero
    uvEnd = V2 0 1
    curves = V4 _curve0 _curve1 _curve2 _curve3
    shiftStep = maximum $ estimateFDStepCount <$> [_curve0 _curve1 _curve2 _curve3]
    
    dUV = pure $ 1 / fromIntegral maxStepCount
    
    basePoints = _cBezierX0 <$> curves
    ffCoeff =
      fmap (fixIter shiftCount halveFDCoefficients) <$> bezierToForwardDifferenceCoeff curves
    
    maxStepCount = 1 `unsafeShiftL` shiftStep
    
    go 0 _pts _coeffs _uvStart _uvEnd = return ()
    go i pts coeffs uvStart uvEnd = do
      let a :: _
          a = fmap updateForwardDifferencing <$> pts <*> coeffs
      rasterizerCubicBezier _tensorValues pts uvStart uvEnd
      go (i - 1) pts coeffs (uvStart ^+^ dUV) (uvEnd ^+^ dUV)

squareStepsToPow :: Float -> Int
squareStepsToPow = fix . snd . frexp . max 1 where
  fix v = (v + 1) `shiftR` 1


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
