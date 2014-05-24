-- | This module is a reduction of the `Linear` package
-- from Edward Kmett to match just the need of Rasterific.
--
-- If the flag `embed_linear` is disabled, this module is
-- just a reexport from the real linear package.
--
{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Linear
    ( V2( .. )
    , V1( .. )
    , Additive( .. )
    , Epsilon( .. )
    , Metric( .. )
    , (^*)
    , (^/)
    , normalize
    ) where

#ifdef EXTERNAL_LINEAR
-- We just reexport
import Linear
#else

import Control.Applicative( Applicative, pure, (<*>) )

infixl 6 ^+^, ^-^
infixl 7 ^*, ^/

-- | A 2-dimensional vector
--
-- >>> pure 1 :: V2 Int
-- V2 1 1
--
-- >>> V2 1 2 + V2 3 4
-- V2 4 6
--
-- >>> V2 1 2 * V2 3 4
-- V2 3 8
--
-- >>> sum (V2 1 2)
-- 3
data V2 a = V2 !a !a
    deriving (Eq, Show)

newtype V1 a = V1 a
    deriving (Eq, Show)

instance Functor V2 where
    {-# INLINE fmap #-}
    fmap f (V2 a b) = V2 (f a) (f b)

instance Num a => Num (V2 a) where
  (V2 a b) + (V2 a' b') = V2 (a + a') (b + b')
  {-# INLINE (+) #-}
  (V2 a b) - (V2 a' b') = V2 (a - a') (b - b')
  {-# INLINE (-) #-}
  (V2 a b) * (V2 a' b') = V2 (a * a') (b * b')
  {-# INLINE (*) #-}
  negate (V2 a b) = V2 (negate a) (negate b)
  {-# INLINE negate #-}
  abs (V2 a b) = V2 (abs a) (abs b)
  {-# INLINE abs #-}
  signum (V2 a b) = V2 (signum a) (signum b)
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Functor V1 where
    {-# INLINE fmap #-}
    fmap f (V1 a) = V1 $ f a

instance Applicative V2 where
    {-# INLINE pure #-}
    pure a = V2 a a
    {-# INLINE (<*>) #-}
    (V2 f1 f2) <*> (V2 a b) = V2 (f1 a) (f2 b)

instance Applicative V1 where
    {-# INLINE pure #-}
    pure = V1 
    {-# INLINE (<*>) #-}
    (V1 f) <*> (V1 v) = V1 $ f v

-- | A vector is an additive group with additional structure.
class Functor f => Additive f where
  -- | The zero vector
  zero :: Num a => f a
  -- | Compute the sum of two vectors
  --
  -- >>> V2 1 2 ^+^ V2 3 4
  -- V2 4 6
  (^+^) :: Num a => f a -> f a -> f a

  -- | Compute the difference between two vectors
  --
  -- >>> V2 4 5 - V2 3 1
  -- V2 1 4
  (^-^) :: Num a => f a -> f a -> f a

  -- | Linearly interpolate between two vectors.
  lerp :: Num a => a -> f a -> f a -> f a

-- | Provides a fairly subjective test to see if a quantity is near zero.
--
-- >>> nearZero (1e-11 :: Double)
-- False
--
-- >>> nearZero (1e-17 :: Double)
-- True
--
-- >>> nearZero (1e-5 :: Float)
-- False
--
-- >>> nearZero (1e-7 :: Float)
-- True
class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6
  {-# INLINE nearZero #-}

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12
  {-# INLINE nearZero #-}

instance Epsilon a => Epsilon (V2 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Additive V2 where
    zero = V2 0 0
    {-# INLINE zero #-}

    (V2 a b) ^+^ (V2 a' b') = V2 (a + a') (b + b')
    {-# INLINE (^+^) #-}

    (V2 a b) ^-^ (V2 a' b') = V2 (a - a') (b - b')
    {-# INLINE (^-^) #-}

    lerp v a b = a ^+^ (b ^-^ a) ^* v
    {-# INLINE lerp #-}

instance Additive V1 where
    zero = V1 0
    {-# INLINE zero #-}

    (V1 a) ^+^ (V1 a') = V1 (a + a')
    {-# INLINE (^+^) #-}

    (V1 a) ^-^ (V1 a') = V1 (a - a')
    {-# INLINE (^-^) #-}

    lerp v a b = a ^+^ (b ^-^ a) ^* v
    {-# INLINE lerp #-}

-- | Free and sparse inner product/metric spaces.
class Additive f => Metric f where
  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector @f a@ into a covector @f a -> a@.
  --
  -- >>> V2 1 2 `dot` V2 3 4
  -- 11
  dot :: Num a => f a -> f a -> a

  -- | Compute the squared norm. The name quadrance arises from
  -- Norman J. Wildberger's rational trigonometry.
  quadrance :: Num a => f a -> a
  quadrance v = dot v v

  -- | Compute the quadrance of the difference
  qd :: Num a => f a -> f a -> a
  qd f g = quadrance (f ^-^ g)

  -- | Compute the distance between two vectors in a metric space
  distance :: Floating a => f a -> f a -> a
  distance f g = norm (f ^-^ g)

  -- | Compute the norm of a vector in a metric space
  norm :: Floating a => f a -> a
  norm v = sqrt (quadrance v)

  -- | Convert a non-zero vector to unit vector.
  signorm :: Floating a => f a -> f a
  signorm v = fmap (/m) v where
    m = norm v

instance Metric V2 where
    dot (V2 a b) (V2 a' b') = a * a' + b * b'
    {-# INLINE dot #-}

    quadrance (V2 a b) = a * a + b * b
    {-# INLINE quadrance #-}

    norm v = sqrt (quadrance v)
    {-# INLINE norm #-}

-- | Compute the right scalar product
--
-- >>> V2 3 4 ^* 2
-- V2 6 8
(^*) :: (Functor f, Num a) => f a -> a -> f a
{-# INLINE (^*) #-}
(^*) f n = fmap (* n) f

-- | Compute division by a scalar on the right.
(^/) :: (Functor f, Floating a) => f a -> a -> f a
{-# INLINE (^/) #-}
(^/) f n = fmap (/ n) f

-- | Normalize a 'Metric' functor to have unit 'norm'. This function
-- does not change the functor if its 'norm' is 0 or 1.
normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
{-# INLINE normalize #-}
normalize v = if nearZero l || nearZero (1-l) then v
             else fmap (/ sqrt l) v
  where l = quadrance v

#endif

