{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Rasterific.PatchTypes where

import Data.Monoid( (<>) )
import qualified Data.Vector as V

import Codec.Picture( Image )

import Graphics.Rasterific.MiniLens
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Transformations

type CoonColorWeight = Float

data PatchInterpolation
  = PatchBilinear
  | PatchBicubic
  deriving (Eq, Show)

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

data Derivative px = Derivative
  { _derivValues :: !(Holder px Float)
  , _xDerivative :: !(Holder px Float)
  , _yDerivative :: !(Holder px Float)
  , _xyDerivative :: !(Holder px Float)
  }

deriving instance Show (Holder px Float) => Show (Derivative px)

xDerivative :: Lens' (Derivative px) (Holder px Float)
xDerivative = lens _xDerivative setter where
  setter o v = o { _xDerivative = v }

yDerivative :: Lens' (Derivative px) (Holder px Float)
yDerivative = lens _yDerivative setter where
  setter o v = o { _yDerivative = v }

instance Applicative ParametricValues where
    pure a = ParametricValues a a a a
    ParametricValues n e s w <*> ParametricValues n' e' s' w' =
        ParametricValues (n n') (e e') (s s') (w w')

instance Foldable ParametricValues where
  foldMap f (ParametricValues n e s w) = f n <> f e <> f s <> f w

transposeParametricValues :: ParametricValues a -> ParametricValues a
transposeParametricValues (ParametricValues n e s w) = ParametricValues n w s e

data TensorPatch weight = TensorPatch
  { _curve0 :: !CubicBezier
  , _curve1 :: !CubicBezier
  , _curve2 :: !CubicBezier
  , _curve3 :: !CubicBezier
  , _tensorValues :: !weight
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
data CoonPatch weight = CoonPatch
    { _north :: !CubicBezier
    , _east :: !CubicBezier
    , _south :: !CubicBezier
    , _west :: !CubicBezier
    , _coonValues :: !weight
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

-- | Define a mesh patch grid, the grid is conceptually
-- a regular grid of _meshPatchWidth * _meshPatchHeight
-- patches but with shared edges
data MeshPatch px = MeshPatch
  { -- | Count of horizontal of *patch*
    _meshPatchWidth  :: !Int
    -- | Count of vertical of *patch*
  , _meshPatchHeight :: !Int
    -- | Main points defining the patch, of size
    -- (_meshPatchWidth + 1) * (_meshPatchHeight + 1)
  , _meshPrimaryVertices :: !(V.Vector Point)

    -- | For each line, store the points in between each
    -- vertex. There is two points between each vertex, so
    -- _meshPatchWidth * (_meshPatchHeight + 1) points
  , _meshHorizontalSecondary :: !(V.Vector InterBezier)
    -- | For each colun, store the points in between each
    -- vertex. Two points between each vertex, so
    -- _meshPatchHeight * (_meshPatchWidth + 1)
  , _meshVerticalSecondary :: !(V.Vector InterBezier)

   -- | Colors for each vertex points
  , _meshColors :: !(V.Vector px)
  }
  deriving (Eq, Show, Functor)

data InterBezier = InterBezier 
  { _inter0 :: !Point
  , _inter1 :: !Point
  }
  deriving (Eq, Show)

instance Transformable InterBezier where
  transform f (InterBezier a b) = InterBezier (f a) (f b)
  transformM f (InterBezier a b) = InterBezier <$> f a <*> f b

transformMeshM :: Monad m => (Point -> m Point) -> MeshPatch px -> m (MeshPatch px)
transformMeshM f MeshPatch { .. } = do
  vertices <- mapM f _meshPrimaryVertices
  hSecondary <- mapM (transformM f) _meshHorizontalSecondary
  vSecondary <- mapM (transformM f) _meshVerticalSecondary
  return $ MeshPatch
      { _meshPatchWidth = _meshPatchWidth 
      , _meshPatchHeight = _meshPatchHeight
      , _meshPrimaryVertices = vertices 
      , _meshHorizontalSecondary = hSecondary 
      , _meshVerticalSecondary = vSecondary
      , _meshColors = _meshColors
      }

instance {-# OVERLAPPING  #-} Transformable (MeshPatch px) where
  transformM = transformMeshM

data Derivatives = Derivatives
  { _interNorthWest :: !Point
  , _interNorthEast :: !Point
  , _interSouthWest :: !Point
  , _interSouthEast :: !Point
  }
  deriving (Eq, Show)

instance Transformable Derivatives where
  transform f (Derivatives a b c d) =
     Derivatives (f a) (f b) (f c) (f d)
  transformM f (Derivatives a b c d) =
     Derivatives <$> f a <*> f b <*> f c <*> f d


-- | Represent a point in the paramaetric U,V space
-- from [0, 1]²
type UV = V2 CoonColorWeight

-- | Define a rectangle in the U,V parametric space.
type UVPatch = ParametricValues UV

newtype CubicCoefficient px = CubicCoefficient
    { getCubicCoefficients :: ParametricValues (V4 (Holder px Float))
    }

data ImageMesh px = ImageMesh
    { _meshImage :: !(Image px)
    , _meshTransform :: !Transformation
    }

