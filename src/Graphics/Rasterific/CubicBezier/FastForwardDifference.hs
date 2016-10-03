{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Rasterific.CubicBezier.FastForwardDifference
    ( ForwardDifferenceCoefficient( .. )
    , bezierToForwardDifferenceCoeff
    , estimateFDStepCount
    , rasterizerCubicBezier
    , rasterizeTensorPatch
    ) where

import Control.Monad.Primitive( PrimMonad )
import Control.Monad.State( lift, get )
import Control.Monad.ST( ST )
import Data.Bits( unsafeShiftL )

import Codec.Picture( PixelRGBA8 )

import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Types
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

halveFDCoefficients :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
halveFDCoefficients (ForwardDifferenceCoefficient a b c) =
    ForwardDifferenceCoefficient { _fdA = a', _fdB = b', _fdC = c' }
  where
    c' = c * 0.125
    b' = b * 0.25 - c'
    a' = (a - b') * 0.5

updateForwardDifferencing :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
updateForwardDifferencing (ForwardDifferenceCoefficient a b c) =
  ForwardDifferenceCoefficient (a + b) (b + c) c

updatePointsAndCoeff :: (Applicative f', Applicative f, Additive f)
                     => f' (f Float) -> f' (f ForwardDifferenceCoefficient)
                     -> (f' (f Float), f' (f ForwardDifferenceCoefficient))
updatePointsAndCoeff pts coeffs =
    (advancePoint <$> pts <*> coeffs, fmap updateForwardDifferencing <$> coeffs)
  where
    fstOf (ForwardDifferenceCoefficient a _ _) = a
    advancePoint v c = v ^+^ (fstOf <$> c)


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

rasterizerCubicBezier :: (PrimMonad m, ModulablePixel px, BiSampleable src px)
                      => src -> CubicBezier
                      -> Float -> Float
                      -> Float -> Float
                      -> DrawContext m px ()
{-# SPECIALIZE INLINE
  rasterizerCubicBezier :: (ParametricValues PixelRGBA8) -> CubicBezier
                        -> Float -> Float
                        -> Float -> Float
                        -> DrawContext (ST s) PixelRGBA8 () #-}
rasterizerCubicBezier source bez uStart vStart uEnd vEnd = do
  canvas <- get
  let !baseFfd = bezierToForwardDifferenceCoeff bez
      !shiftCount = estimateFDStepCount bez
      maxStepCount :: Int
      maxStepCount = 1 `unsafeShiftL` shiftCount
      !(V2 (ForwardDifferenceCoefficient ax' bx' cx)
           (ForwardDifferenceCoefficient ay' by' cy)) =
               fixIter shiftCount halveFDCoefficients <$> baseFfd

      !(V2 du dv) = (V2 uEnd vEnd ^-^ V2 uStart vStart) ^/ fromIntegral maxStepCount
      !(V2 xStart yStart) = _cBezierX0 bez
      
      go !currentStep _ _ _ _ _ _ _ _ | currentStep >= maxStepCount = return ()
      go !currentStep !ax !bx !ay !by !x !y !u !v = do
        let !color = interpolate source u v
        plotPixel canvas color (floor x) (floor y)
        go (currentStep + 1)
            (ax + bx) (bx + cx)
            (ay + by) (by + cy)
            (x  + ax) (y  + ay)
            (u  + du) (v  + dv)

  lift $ go 0 ax' bx' ay' by' xStart yStart uStart vStart

rasterizeTensorPatch :: (PrimMonad m, ModulablePixel px, BiSampleable src px)
                     => TensorPatch src -> DrawContext m px ()
{-# SPECIALIZE rasterizeTensorPatch :: TensorPatch (ParametricValues PixelRGBA8)
                                    -> DrawContext (ST s) PixelRGBA8 () #-}
rasterizeTensorPatch TensorPatch { .. } =
    go maxStepCount basePoints ffCoeff 0 0 0 1
  where
    !curves = V4 _curve0 _curve1 _curve2 _curve3
    !shiftStep = maximum $ estimateFDStepCount <$> [_curve0, _curve1, _curve2, _curve3]
    
    !basePoints = _cBezierX0 <$> curves
    !ffCoeff =
      fmap (fixIter shiftStep halveFDCoefficients) . bezierToForwardDifferenceCoeff <$> curves
    
    maxStepCount :: Int
    !maxStepCount = 1 `unsafeShiftL` shiftStep

    !(V2 du dv) = (V2 1 0) ^/ fromIntegral maxStepCount
    
    toBezier (V4 a b c d) = CubicBezier a b c d
    
    go 0 _pts _coeffs _uvStart _ _uvEnd _ = return ()
    go i !pts !coeffs !ut !vt !ub !vb = do
      let (newPoints, newCoeff) = updatePointsAndCoeff pts coeffs
      rasterizerCubicBezier _tensorValues (toBezier pts) ut vt ub vb
      go (i - 1) newPoints newCoeff (ut + du) (vt + dv) (ub + du) (vb + dv)

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

