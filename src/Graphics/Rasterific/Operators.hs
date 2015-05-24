{-# LANGUAGE CPP #-}
-- | Module providing basic helper functions to help
-- build vector/point calculations.
module Graphics.Rasterific.Operators
    ( Point
      -- * Lifted operators
    , (^&&^)
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
    , middle
    , vpartition 
    , normal
    , ifZero
    , isNearby
    , isDistingableFrom
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( Applicative, (<$>) )
#endif
import Control.Applicative( liftA2, liftA3 )

import Graphics.Rasterific.Linear
             ( V2( .. )
             , Additive( .. )
             , Epsilon( nearZero )
             , (^+^)
             , (^*)
             , dot
             , normalize
             )

infix  4 ^<, ^<=^, ^<^, ^==^, ^/=^
infixr 3 ^&&^
infixr 2 ^||^

-- | Represent a point
type Point = V2 Float

-- | Pairwise boolean and operator
(^&&^) :: (Applicative a) => a Bool -> a Bool -> a Bool
{-# INLINE (^&&^) #-}
(^&&^) = liftA2 (&&)

-- | Pairwise boolean or operator
(^||^) :: (Applicative a) => a Bool -> a Bool -> a Bool
{-# INLINE (^||^) #-}
(^||^) = liftA2 (||)

-- | Pairwise vector/point equal operator
(^==^) :: (Eq v, Applicative a) => a v -> a v -> a Bool
{-# INLINE (^==^) #-}
(^==^) = liftA2 (==)

-- | Pairwise vector/point lower than or equal operator
(^<=^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
{-# INLINE (^<=^) #-}
(^<=^) = liftA2 (<=)

-- | Pairwise vector/point lower than operator
(^<^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
{-# INLINE (^<^) #-}
(^<^) = liftA2 (<)

-- | Component/scalar lower than operator.
(^<) :: (Applicative a, Ord v) => a v -> v -> a Bool
{-# INLINE (^<) #-}
(^<) vec v = (< v) <$> vec

-- | Pairwise vector/point difference operator.
(^/=^) :: (Applicative a, Eq v) => a v -> a v -> a Bool
{-# INLINE (^/=^) #-}
(^/=^) = liftA2 (/=)

-- | Min function between two vector/points.
-- Work on every component separately.
vmin :: (Ord n, Applicative a) => a n -> a n -> a n
{-# INLINE vmin #-}
vmin = liftA2 min

-- | Max function between to vector/point.
-- Work on every component separatly.
vmax :: (Ord n, Applicative a) => a n -> a n -> a n
{-# INLINE vmax #-}
vmax = liftA2 max

-- | Abs function for every component of the vector/point.
vabs :: (Num n, Functor a) => a n -> a n
{-# INLINE vabs #-}
vabs = fmap abs

-- | Floor function for every component of the vector/point.
vfloor :: (Functor a) => a Float -> a Int
{-# INLINE vfloor #-}
vfloor = fmap floor

-- | ceil function for every component of the vector/point.
vceil :: (Functor a) => a Float -> a Int
{-# INLINE vceil #-}
vceil = fmap ceiling

-- | Given a point, clamp every coordinates between
-- a given minimum and maximum.
clampPoint :: Point -> Point -> Point -> Point
{-# INLINE clampPoint #-}
clampPoint mini maxi v = vmin maxi $ vmax mini v

-- | Given two points, return a point in the middle
-- of them.
midPoint :: (Additive a) => a Float -> a Float -> a Float
{-# INLINE midPoint #-}
midPoint a b = (a ^+^ b) ^* 0.5

middle :: (Fractional a) => a -> a -> a
{-# INLINE middle #-}
middle a b = (a + b) * 0.5

-- | Given a boolean choice vector, return elements of
-- the first one if true, of the second one otherwise.
vpartition :: (Applicative a) => a Bool -> a v -> a v -> a v
{-# INLINE vpartition #-}
vpartition = liftA3 choose
  where choose True a _ = a
        choose False _ b = b

-- | Calculate a normal vector
normal :: (Floating v, Epsilon v) => V2 v -> V2 v -> V2 v
{-# INLINE normal #-}
normal (V2 ax ay) (V2 bx by) = normalize $ V2 (ay - by) (bx - ax)

-- | Return the second operand if the vector is
-- nearly null
ifZero :: (Epsilon v) => v -> v -> v
{-# INLINE ifZero #-}
ifZero u v | nearZero u = v
           | otherwise = u

-- | Tell if two points are nearly indistinguishable.
-- If indistinguishable, we can treat them as the same
-- point.
isNearby :: Point -> Point -> Bool
{-# INLINE isNearby #-}
isNearby p1 p2 = squareDist < 0.0001
  where vec = p1 ^-^ p2
        squareDist = vec `dot` vec

-- | simply `not (a `isNearby` b)`
isDistingableFrom :: Point -> Point -> Bool
{-# INLINE isDistingableFrom #-}
isDistingableFrom a b = not $ isNearby a b

