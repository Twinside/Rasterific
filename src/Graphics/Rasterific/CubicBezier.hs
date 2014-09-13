{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Rasterific.CubicBezier
    ( cubicBezierCircle
    , cubicBezierFromPath
    , cubicBezierBreakAt
    , clipCubicBezier
    , decomposeCubicBeziers
    , decomposeCubicBezierForwardDifference
    , sanitizeCubicBezier
    , offsetCubicBezier
    , flattenCubicBezier
    , cubicBezierLengthApproximation
    , cubicBezierBounds
    ) where

import Prelude hiding( or )
import Data.Bits( unsafeShiftL )
import Control.Applicative( liftA2
                          , (<$>)
                          , (<*>)
                          , pure
                          )
import Graphics.Rasterific.Linear
             ( V1( .. )
             , V2( .. )
             , (^-^)
             , (^+^)
             , (^*)
             , norm
             , lerp
             , qd
             )
import Data.List( nub )
import Data.Monoid( mempty, (<>) )
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticFormula
import Graphics.Rasterific.QuadraticBezier( sanitizeBezier )

import Debug.Trace
import Text.Printf

-- | Create a list of cubic bezier patch from a list of points.
--
-- > cubicBezierFromPath [a, b, c, d, e] = [CubicBezier a b c d]
-- > cubicBezierFromPath [a, b, c, d, e, f, g] =
-- >    [CubicBezier a b c d, CubicBezier d e f g]
--
cubicBezierFromPath :: [Point] -> [CubicBezier]
cubicBezierFromPath (a:b:c:rest@(d:_)) =
    CubicBezier a b c d : cubicBezierFromPath rest
cubicBezierFromPath _ = []

cubicBezierLengthApproximation :: CubicBezier -> Float
cubicBezierLengthApproximation (CubicBezier a _ _ d) =
    norm $ d ^-^ a

-- | Represent a circle of radius 1 centered on 0 of
-- a cubic bezier curve.
cubicBezierCircle :: [CubicBezier]
cubicBezierCircle =
    [ CubicBezier (V2 0 1) (V2 c 1) (V2 1 c) (V2 1 0)
    , CubicBezier (V2 1 0) (V2 1 (-c)) (V2 c (-1)) (V2 0 (-1))
    , CubicBezier (V2 0 (-1)) (V2 (-c) (-1)) (V2 (-1) (-c)) (V2 (-1) 0)
    , CubicBezier (V2 (-1) 0) (V2 (-1) c) (V2 (-c) 1) (V2 0 1)
    ]
  where c = 0.551915024494 -- magic constant? magic constant.

straightLine :: Point -> Point -> CubicBezier
straightLine a b = CubicBezier a p p b
  where p = a `midPoint` b

isSufficientlyFlat :: Float -- ^ Tolerance
                   -> CubicBezier
                   -> Bool
isSufficientlyFlat tol (CubicBezier a b c d) =
    x + y <= tolerance
  where u = (b ^* 3) ^-^ (a ^* 2) ^-^ d
        v = (c ^* 3) ^-^ (d ^* 2) ^-^ a
        (^*^) = liftA2 (*)
        V2 x y = vmax (u ^*^ u) (v ^*^ v)
        tolerance = 16 * tol * tol

flattenCubicBezier :: CubicBezier -> Container Primitive
flattenCubicBezier bezier@(CubicBezier a b c d)
    | isSufficientlyFlat 1 bezier = pure $ CubicBezierPrim bezier
    | otherwise =
        flattenCubicBezier (CubicBezier a ab abbc abbcbccd) <>
            flattenCubicBezier (CubicBezier abbcbccd bccd cd d)
  where
    --                     BC
    --         B X----------X---------X C
    --    ^     /      ___/   \___     \     ^
    --   u \   /   __X------X------X_   \   / v
    --      \ /___/ ABBC       BCCD  \___\ /
    --    AB X/                          \X CD
    --      /                              \
    --     /                                \
    --    /                                  \
    -- A X                                    X D
    ab = a `midPoint` b
    bc = b `midPoint` c
    cd = c `midPoint` d

    abbc = ab `midPoint` bc
    bccd = bc `midPoint` cd
    abbcbccd = abbc `midPoint` bccd


--               3                    2            2                  3
-- x(t) = (1 - t) ∙x     + 3∙t∙(1 - t) ∙x     + 3∙t ∙(1 - t)∙x     + t ∙x
--                   0                    1                    2          3
--
--               3                    2            2                  3
-- y(t) = (1 - t) ∙y     + 3∙t∙(1 - t) ∙y     + 3∙t ∙(1 - t)∙y     + t ∙y
--                   0                    1                    2          3

-- Other representation:
--                3                2        2              3
-- B(t) = x(1 - t)  + 3∙y∙t∙(1 - t)  + 3∙z∙t ∙(1 - t) + w∙t


-- | Represent the cubic bezier curve as a vector ready
-- for matrix multiplication
data CachedBezier = CachedBezier
    { _cachedA :: {-# UNPACK #-} !Float
    , _cachedB :: {-# UNPACK #-} !Float
    , _cachedC :: {-# UNPACK #-} !Float
    , _cachedD :: {-# UNPACK #-} !Float
    }

cacheBezier :: CubicBezier -> (CachedBezier, CachedBezier)
cacheBezier (CubicBezier p0@(V2 x0 y0) p1 p2 p3) =
    (CachedBezier x0 bX cX dX, CachedBezier y0 bY cY dY)
  where
   V2 bX bY = p1 ^* 3 ^-^ p0 ^* 3
   V2 cX cY = p2 ^* 3 ^-^ p1 ^* 6 + p0 ^* 3
   V2 dX dY = p3 ^-^ p2 ^* 3 ^+^ p1 ^* 3 ^-^ p0

cachedBezierAt :: CachedBezier -> Float -> Float
cachedBezierAt (CachedBezier a b c d) t =
    a + b * t + c * tSquare + tCube * d
  where
    tSquare = t * t
    tCube = tSquare * t

data ForwardDifferenceCoefficient = ForwardDifferenceCoefficient
    { _fdA :: {-# UNPACK #-} !Float
    , _fdB :: {-# UNPACK #-} !Float
    , _fdC :: {-# UNPACK #-} !Float
    }

{-
fdCoeffOfEqua :: Float -> CachedBezier -> ForwardDifferenceCoefficient
fdCoeffOfEqua stepSize (CachedBezier a b c _) = ForwardDifferenceCoefficient
  { _fdA = aHCube + bHSquare + c * stepSize
  , _fdB = 6 * aHCube + 2 * bHSquare
  , _fdC = 6 * aHCube
  }
  where
    stepSizeSquare = stepSize * stepSize
    aHCube = a * stepSizeSquare * stepSize
    bHSquare = b * stepSizeSquare
-- -}

-- | Given a cubic curve, return the initial step size and
-- the coefficient for the forward difference.
bezierToForwardDifferenceCoeff
    :: CubicBezier
    -> (Float, ForwardDifferenceCoefficient, ForwardDifferenceCoefficient)
bezierToForwardDifferenceCoeff (CubicBezier x y z w) = (1, xCoeffs, yCoeffs)
  where
    xCoeffs = ForwardDifferenceCoefficient { _fdA = ax, _fdB = bx, _fdC = cx }
    yCoeffs = ForwardDifferenceCoefficient { _fdA = ay, _fdB = by, _fdC = cy }

    V2 ax ay = (w ^-^ z ^* 3 ^+^ y ^* 3 ^-^ x) ^* 6
    V2 bx by = (w ^-^ z ^* 2 ^+^ y) ^* 6
    V2 cx cy = w ^-^ x

halveFDCoefficients :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
halveFDCoefficients (ForwardDifferenceCoefficient a b c) =
    ForwardDifferenceCoefficient { _fdA = a', _fdB = b', _fdC = c' }
  where
    a' = a * 0.125
    b' = b * 0.25 - a'
    c' = (c - b') * 0.5

{-
doubleFDCoefficients :: ForwardDifferenceCoefficient -> ForwardDifferenceCoefficient
doubleFDCoefficients (ForwardDifferenceCoefficient a b c) =
    ForwardDifferenceCoefficient { _fdA = a', _fdB = b', _fdC = c' }
  where
    a' = 8 * a
    b' = 4 * b + 4 * a
    c' = 2 * c + b
-- -}
--
updateForwardDifferencing :: Float -> ForwardDifferenceCoefficient
                          -> (Float, ForwardDifferenceCoefficient)
updateForwardDifferencing v (ForwardDifferenceCoefficient a b c) =
    (v + c, ForwardDifferenceCoefficient a (b + a) (c + b))


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

decomposeCubicBezierForwardDifference :: CubicBezier -> Container EdgeSample
decomposeCubicBezierForwardDifference bez =
    (trace $ printf "stepCount:%d maxStepCount:%d" stepCount maxStepCount) lst where
  lst = go 0 xStartCoeff yStartCoeff xStart yStart

  (_initialStep, xFull, yFull) = bezierToForwardDifferenceCoeff bez
  xStartCoeff = fixIter stepCount halveFDCoefficients xFull
  yStartCoeff = fixIter stepCount halveFDCoefficients yFull

  V2 xStart yStart = _cBezierX0 bez
  stepCount = estimateFDStepCount bez

  maxStepCount :: Int
  maxStepCount = 1 `unsafeShiftL` stepCount

  go !currentStep _xFdCoeff _yFdCoeff _xPrev !_yPrev
    | currentStep >= maxStepCount = trace "DONE" $ mempty
  go !currentStep !xFdCoeff !yFdCoeff !xPrev !yPrev =
     pure sample <> go (currentStep + 1) xCoeff yCoeff xNext yNext
    where
      sample = EdgeSample
          { _sampleX     = fromIntegral integerPrevX
          , _sampleY     = fromIntegral integerPrevY
          , _sampleAlpha = 1.0 -- For testing purposes only.
          , _sampleH     = 1.0
          }

      integerPrevX, integerPrevY :: Int
      integerPrevX = floor xPrev
      integerPrevY = floor yPrev

      {-yDelta = xNext - xPrev-}
      (xNext, xCoeff) = updateForwardDifferencing xPrev xFdCoeff
      (yNext, yCoeff) = updateForwardDifferencing yPrev yFdCoeff

cachedBezierDerivative :: CachedBezier -> QuadraticFormula Float
cachedBezierDerivative (CachedBezier _ b c d) =
    QuadraticFormula (3 * d) (2 * c) b

-- | Find the coefficient of the extremum points
extremums :: CachedBezier -> [Float]
extremums cached =
  [ root | root <- formulaRoots $ cachedBezierDerivative cached
         , 0 <= root && root <= 1.0 ]

extremumPoints :: (CachedBezier, CachedBezier) -> [Point]
extremumPoints (onX, onY) = toPoints <$> nub (extremums onX <> extremums onY)
  where toPoints at = V2 (cachedBezierAt onX at) (cachedBezierAt onY at)

cubicBezierBounds :: CubicBezier -> [Point]
cubicBezierBounds bez@(CubicBezier p0 _ _ p3) =
    p0 : p3 : extremumPoints (cacheBezier bez)

offsetCubicBezier :: Float -> CubicBezier -> Container Primitive
offsetCubicBezier offset bezier@(CubicBezier a b c d)
    | isSufficientlyFlat 1 bezier =
        pure . CubicBezierPrim $ CubicBezier shiftedA shiftedB shiftedC shiftedD
    | otherwise =
        recurse (CubicBezier a ab abbc abbcbccd) <>
            recurse (CubicBezier abbcbccd bccd cd d)
  where
    recurse = offsetCubicBezier offset

    u = a `normal` b
    v = c `normal` d

    --                     BC
    --         B X----------X---------X C
    --    ^     /      ___/   \___     \     ^
    --   u \   /   __X------X------X_   \   / v
    --      \ /___/ ABBC       BCCD  \___\ /
    --    AB X/                          \X CD
    --      /                              \
    --     /                                \
    --    /                                  \
    -- A X                                    X D
    ab = a `midPoint` b
    bc = b `midPoint` c
    cd = c `midPoint` d

    w = ab `normal` bc
    x = bc `normal` cd

    abbc = ab `midPoint` bc
    bccd = bc `midPoint` cd
    abbcbccd = abbc `midPoint` bccd

    shiftedA = a ^+^ (u ^* offset)
    shiftedD = d ^+^ (v ^* offset)

    {-shiftedABBCBCCD = abbcbccd ^+^ (w ^* offset)-}
    shiftedB = (b ^+^ (w ^* offset))
    shiftedC = (c ^+^ (x ^* offset))

-- | Clamp the cubic bezier curve inside a rectangle
-- given in parameter.
clipCubicBezier
    :: Point   -- ^ Point representing the "minimal" point for cliping
    -> Point  -- ^ Point representing the "maximal" point for cliping
    -> CubicBezier -- ^ The cubic bezier curve to be clamped
    -> Container Primitive
clipCubicBezier mini maxi bezier@(CubicBezier a b c d)
    -- If we are in the range bound, return the curve
    -- unaltered
    | insideX && insideY = pure $ CubicBezierPrim bezier
    -- If one of the component is outside, clamp
    -- the components on the boundaries and output a
    -- straight line on this boundary. Useful for the
    -- filing case, to clamp the polygon drawing on
    -- the edge
    | outsideX || outsideY =
        pure . CubicBezierPrim $ clampedA `straightLine` clampedD
    -- Not completly inside nor outside, just divide
    -- and conquer.
    | otherwise =
        recurse (CubicBezier a ab abbc m) <>
            recurse (CubicBezier m bccd cd d)
  where -- Minimal & maximal dimension of the bezier curve
        bmin = vmin a . vmin b $ vmin c d
        bmax = vmax a . vmax b $ vmin c d

        recurse = clipCubicBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedD = clamper d

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        --                     BC
        --         B X----------X---------X C
        --          /      ___/   \___     \
        --         /   __X------X------X_   \
        --        /___/ ABBC       BCCD  \___\
        --    AB X/                          \X CD
        --      /                              \
        --     /                                \
        --    /                                  \
        -- A X                                    X D
        ab = a `midPoint` b
        bc = b `midPoint` c
        cd = c `midPoint` d

        abbc = ab `midPoint` bc
        bccd = bc `midPoint` cd
        abbcbccd = abbc `midPoint` bccd

        edgeSeparator = vabs (abbcbccd ^-^ mini) ^<^ vabs (abbcbccd ^-^ maxi)
        edge = vpartition edgeSeparator mini maxi
        m = vpartition (vabs (abbcbccd ^-^ edge) ^< 0.1) edge abbcbccd

-- | Will subdivide the bezier from 0 to coeff and coeff to 1
cubicBezierBreakAt :: CubicBezier -> Float
                   -> (CubicBezier, CubicBezier)
cubicBezierBreakAt (CubicBezier a b c d) val =
    (CubicBezier a ab abbc abbcbccd, CubicBezier abbcbccd bccd cd d)
  where
    ab = lerp val a b
    bc = lerp val b c
    cd = lerp val c d

    abbc = lerp val ab bc
    bccd = lerp val bc cd
    abbcbccd = lerp val abbc bccd

decomposeCubicBeziers :: CubicBezier -> Container EdgeSample
decomposeCubicBeziers (CubicBezier aR bR cR dR) = go aR bR cR dR where
  go a@(V2 ax ay) _b _c d@(V2 dx dy) | insideX && insideY =
    pure $ EdgeSample (px + 0.5) (py + 0.5) (w * h) h
    where
      !floorA = vfloor a
      !floorD = vfloor d
      !(V2 px py)  = fromIntegral <$> vmin floorA floorD
      !(V1 w) = (px + 1 -) <$>  (V1 dx `midPoint` V1 ax)
      !h = dy - ay
      !(V2 insideX insideY) =
          floorA ^==^ floorD ^||^ vceil a ^==^ vceil d

  go a b c d = go a ab abbc m <> go m bccd cd d
    where
      --                     BC
      --         B X----------X---------X C
      --          /      ___/   \___     \
      --         /   __X------X------X_   \
      --        /___/ ABBC       BCCD  \___\
      --    AB X/                          \X CD
      --      /                              \
      --     /                                \
      --    /                                  \
      -- A X                                    X D
      !ab = a `midPoint` b
      !bc = b `midPoint` c
      !cd = c `midPoint` d
      !abbc = ab `midPoint` bc
      !bccd = bc `midPoint` cd

      !abbcbccd = abbc `midPoint` bccd

      mini = fromIntegral <$> vfloor abbcbccd
      maxi = fromIntegral <$> vceil abbcbccd
      !nearmin = vabs (abbcbccd ^-^ mini) ^< 0.1
      !nearmax = vabs (abbcbccd ^-^ maxi) ^< 0.1

      minMaxing mi nearmi ma nearma p
        | nearmi = mi
        | nearma = ma
        | otherwise = p

      !m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax
                     <*> abbcbccd

sanitizeCubicBezier :: CubicBezier -> Container Primitive
sanitizeCubicBezier bezier@(CubicBezier a b c d)
  | b `isNearby` c = sanitizeBezier $ Bezier a c d
  | a `isDistingableFrom` b &&
    c `isDistingableFrom` d =
       pure . CubicBezierPrim $ bezier
  | (ac `isDistingableFrom` b &&
     bd `isDistingableFrom` c) =
      pure . CubicBezierPrim $ bezier
  | ac `isDistingableFrom` b =
      pure . CubicBezierPrim $ CubicBezier a ac c d
  | bd `isDistingableFrom` c =
      pure . CubicBezierPrim $ CubicBezier a b bd d
  | otherwise = mempty
    where ac = a `midPoint` c
          bd = a `midPoint` d

