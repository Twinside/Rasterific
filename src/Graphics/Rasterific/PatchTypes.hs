{-# LANGUAGE DeriveFunctor #-}
module Graphics.Rasterific.PatchTypes where

import Data.Monoid( (<>) )
import Graphics.Rasterific.Types

type CoonColorWeight = Float

-- | Values associated to the corner of a patch
-- @
--  North               East
--      +--------------+
--      |0            1|
--      |              |
--      |              |
--      |              |
--      |3            2|
--      +--------------+
--  West                South
-- @
data ParametricValues a = ParametricValues
    { _northValue :: !a
    , _eastValue  :: !a
    , _southValue :: !a
    , _westValue  :: !a
    }
    deriving (Functor, Show)

instance Applicative ParametricValues where
    pure a = ParametricValues a a a a
    ParametricValues n e s w <*> ParametricValues n' e' s' w' =
        ParametricValues (n n') (e e') (s s') (w w')

instance Foldable ParametricValues where
  foldMap f (ParametricValues n e s w) = f n <> f e <> f s <> f w

transposeParametricValues :: ParametricValues a -> ParametricValues a
transposeParametricValues (ParametricValues n e s w) = ParametricValues n w s e

data TensorPatch px = TensorPatch
  { _curve0 :: !CubicBezier
  , _curve1 :: !CubicBezier
  , _curve2 :: !CubicBezier
  , _curve3 :: !CubicBezier
  , _tensorValues :: !(ParametricValues px)
  }

instance Transformable (TensorPatch px) where
  transform f (TensorPatch c0 c1 c2 c3 v) =
    TensorPatch
        (transform f c0)
        (transform f c1)
        (transform f c2)
        (transform f c3)
        v
  transformM f (TensorPatch c0 c1 c2 c3 v) =
    TensorPatch
        <$> transformM f c0
        <*> transformM f c1
        <*> transformM f c2
        <*> transformM f c3
        <*> return v


instance {-# OVERLAPPING #-} PointFoldable (TensorPatch px) where
  foldPoints f acc (TensorPatch c0 c1 c2 c3 _) = g c3 . g c2 . g c1 $ g c0 acc
    where g v a = foldPoints f a v

--
-- @
--                        ----->
--                  North     _____----------------+
--   ^          +------------/                     /
--   |         /                                  /       |
--   |        /                                  /        |
--   |       /                                  /  east   |
--   | west |                                  /          |
--          |                                 |           v
--           \                                 \   .
--            \                  __-------------+
--             +----------------/
--                    South
--                       <-----
-- @
--
data CoonPatch px = CoonPatch
    { _north :: !CubicBezier
    , _east :: !CubicBezier
    , _south :: !CubicBezier
    , _west :: !CubicBezier
    , _coonValues :: {-# UNPACK #-} !(ParametricValues px)
    }
    deriving Show

instance {-# OVERLAPPING #-} Transformable (CoonPatch px) where
  transformM = transformCoonM
  transform = transformCoon 

instance {-# OVERLAPPING #-} PointFoldable (CoonPatch px) where
  foldPoints f acc (CoonPatch n e s w _) = g n . g e . g s $ g w acc
    where g v a = foldPoints f a v

transformCoonM :: Monad m => (Point -> m Point) -> CoonPatch px -> m (CoonPatch px)
transformCoonM f (CoonPatch n e s w v) =
  CoonPatch <$> transformM f n <*> transformM f e <*> transformM f s <*> transformM f w
            <*> return v

transformCoon :: (Point -> Point) -> CoonPatch px -> CoonPatch px
transformCoon f (CoonPatch n e s w v) =
    CoonPatch
        (transform f n)
        (transform f e)
        (transform f s)
        (transform f w)
        v

