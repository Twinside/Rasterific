module Graphics.Rasterific.Operators
    ( Point
    , (^&&^)
    , (^||^)
    , (^==^)
    , (^<=^)
    , (^<^)
    , (^<)
    , vmin
    , vmax
    , vabs
    , vfloor
    , vceil
    , clampPoint
    , midPoint
    , vpartition 
    ) where

import Control.Applicative( Applicative
                          , liftA2
                          , liftA3
                          , (<$>)
                          )

import Linear( V2( .. )
             , Additive( .. )
             , (^+^)
             , (^/)
             )

type Point = V2 Float

infix  4 ^<, ^<=^, ^<^, ^==^
infixr 3 ^&&^
infixr 2 ^||^

(^&&^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^&&^) = liftA2 (&&)

(^||^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^||^) = liftA2 (||)

(^==^) :: (Eq v, Applicative a) => a v -> a v -> a Bool
(^==^) = liftA2 (==)

(^<=^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<=^) = liftA2 (<=)

(^<^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<^) = liftA2 (<)

(^<) :: (Applicative a, Ord v) => a v -> v -> a Bool
(^<) vec v = (< v) <$> vec

vmin :: (Ord n, Applicative a) => a n -> a n -> a n
vmin = liftA2 min

vmax :: (Ord n, Applicative a) => a n -> a n -> a n
vmax = liftA2 max

vabs :: (Num n, Functor a) => a n -> a n
vabs = fmap abs

vfloor :: (Additive a) => a Float -> a Int
vfloor = fmap floor

vceil :: (Additive a) => a Float -> a Int
vceil = fmap ceiling

clampPoint :: Point -> Point -> Point -> Point
clampPoint mini maxi v = vmin maxi $ vmax mini v

midPoint :: (Additive a) => a Float -> a Float -> a Float
midPoint a b = (a ^+^ b) ^/ 2.0

vpartition :: (Applicative a) => a Bool -> a v -> a v -> a v
vpartition = liftA3 choose
  where choose True a _ = a
        choose False _ b = b

