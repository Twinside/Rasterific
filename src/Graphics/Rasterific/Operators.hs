-- | Module providing basic helper functions to help
-- build vector/point calculations.
module Graphics.Rasterific.Operators
    (  -- * Lifted operators
      (^&&^)
    , (^||^)
    , (^==^)
    , (^/=^)
    , (^<=^)
    , (^<^)
    , (^<)

      -- *  Lifted functions
    , vmin
    , vmax
    , vabs
    , vfloor
    , vceil
    , clampPoint
    , midPoint
    , vpartition 
    , normal
    , ifZero
    ) where

import Control.Applicative( Applicative
                          , liftA2
                          , liftA3
                          , (<$>)
                          )

import Linear( V2( .. )
             , Additive( .. )
             {-, Metric( .. )-}
             , Epsilon( nearZero )
             , (^+^)
             , (^/)
             , normalize
             )

import Graphics.Rasterific.Types

infix  4 ^<, ^<=^, ^<^, ^==^, ^/=^
infixr 3 ^&&^
infixr 2 ^||^

-- | Pairwise boolean and operator
(^&&^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^&&^) = liftA2 (&&)

-- | Pairwise boolean or operator
(^||^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^||^) = liftA2 (||)

-- | Pairwise vector/point equal operator
(^==^) :: (Eq v, Applicative a) => a v -> a v -> a Bool
(^==^) = liftA2 (==)

-- | Pairwise vector/point lower than or equal operator
(^<=^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<=^) = liftA2 (<=)

-- | Pairwise vector/point lower than operator
(^<^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<^) = liftA2 (<)

-- | Component/scalar lower than operator.
(^<) :: (Applicative a, Ord v) => a v -> v -> a Bool
(^<) vec v = (< v) <$> vec

-- | Pairwise vector/point difference operator.
(^/=^) :: (Applicative a, Eq v) => a v -> a v -> a Bool
(^/=^) = liftA2 (/=)

-- | Min function between two vector/points.
-- Work on every component separately.
vmin :: (Ord n, Applicative a) => a n -> a n -> a n
vmin = liftA2 min

-- | Max function between to vector/point.
-- Work on every component separatly.
vmax :: (Ord n, Applicative a) => a n -> a n -> a n
vmax = liftA2 max

-- | Abs function for every component of the vector/point.
vabs :: (Num n, Functor a) => a n -> a n
vabs = fmap abs

-- | Floor function for every component of the vector/point.
vfloor :: (Functor a) => a Float -> a Int
vfloor = fmap floor

-- | ceil function for every component of the vector/point.
vceil :: (Functor a) => a Float -> a Int
vceil = fmap ceiling

-- | Given a point, clamp every coordinates between
-- a given minimum and maximum.
clampPoint :: Point -> Point -> Point -> Point
clampPoint mini maxi v = vmin maxi $ vmax mini v

-- | Given two points, return a point in the middle
-- of them.
midPoint :: (Additive a) => a Float -> a Float -> a Float
midPoint a b = (a ^+^ b) ^/ 2.0

-- | Given a boolean choice vector, return elements of
-- the first one if true, of the second one otherwise.
vpartition :: (Applicative a) => a Bool -> a v -> a v -> a v
vpartition = liftA3 choose
  where choose True a _ = a
        choose False _ b = b

-- | Calculate a normal vector
normal :: (Floating v, Epsilon v) => V2 v -> V2 v -> V2 v
normal (V2 ax ay) (V2 bx by) = normalize $ V2 (ay - by) (bx - ax)

-- | Return the second operand if the vector is
-- nearly null
ifZero :: (Epsilon v) => v -> v -> v
ifZero u v | nearZero u = v
           | otherwise = u

