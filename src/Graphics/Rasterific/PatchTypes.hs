{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Rasterific.PatchTypes
  ( -- * New geometry
    CoonPatch( .. )
  , TensorPatch( .. )
  , MeshPatch( .. )
  , InterBezier( .. )

    -- * Types
  , CoonColorWeight
  , PatchInterpolation( .. )
  , ParametricValues( .. )
  , Derivative( .. )
  , Derivatives( .. )
  , UV
  , UVPatch
  , CubicCoefficient( .. )
  , ImageMesh( .. )

    -- * Helper functions
  , transposeParametricValues 
  , coonPointAt
  , toTensorPatch
  , foldMeshPoints
  , isVerticalOrientation

    -- * Lenses
  , xDerivative
  , yDerivative
  ) where

import Data.Monoid( (<>) )
import qualified Data.Vector as V

import Codec.Picture( Image )

import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.MiniLens
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Transformations

-- | Type of coordinate interpolation
type CoonColorWeight = Float

-- | How do we want to perform color/image interpolation
-- within the patch.
data PatchInterpolation
  = -- | Bilinear interpolation
    --
    -- @
    -- import qualified Data.Vector as V
    -- let colorCycle = cycle
    --       [ PixelRGBA8 0 0x86 0xc1 255
    --       , PixelRGBA8 0xff 0xf4 0xc1 255
    --       , PixelRGBA8 0xFF 0x53 0x73 255
    --       , PixelRGBA8 0xff 0xf4 0xc1 255
    --       , PixelRGBA8 0 0x86 0xc1 255]
    --     colors = V.fromListN (4 * 4) colorCycle
    -- renderMeshPatch PatchBilinear $ generateLinearGrid 3 3 (V2 10 10) (V2 60 60) colors
    -- @
    --
    -- <<docimages/mesh_patch_interp_bilinear.png>>
    --
    PatchBilinear
    -- | Bicubic interpolation
    --
    -- @
    -- import qualified Data.Vector as V
    -- let colorCycle = cycle
    --       [ PixelRGBA8 0 0x86 0xc1 255
    --       , PixelRGBA8 0xff 0xf4 0xc1 255
    --       , PixelRGBA8 0xFF 0x53 0x73 255
    --       , PixelRGBA8 0xff 0xf4 0xc1 255
    --       , PixelRGBA8 0 0x86 0xc1 255]
    --     colors = V.fromListN (4 * 4) colorCycle
    -- renderMeshPatch PatchBicubic $ generateLinearGrid 3 3 (V2 10 10) (V2 60 60) colors
    -- @
    --
    -- <<docimages/mesh_patch_interp_bicubic.png>>
    --
  | PatchBicubic
  deriving (Eq, Show)

-- | Values associated to the corner of a patch
--
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
--
data ParametricValues a = ParametricValues
  { _northValue :: !a
  , _eastValue  :: !a
  , _southValue :: !a
  , _westValue  :: !a
  }
  deriving (Functor, Show)

-- | Store the derivative necessary for cubic interpolation in
-- the gradient mesh.
data Derivative px = Derivative
  { _derivValues :: !(Holder px Float)
  , _xDerivative :: !(Holder px Float)
  , _yDerivative :: !(Holder px Float)
  , _xyDerivative :: !(Holder px Float)
  }

deriving instance Show (Holder px Float) => Show (Derivative px)

-- | Helping lens
xDerivative :: Lens' (Derivative px) (Holder px Float)
xDerivative = lens _xDerivative setter where
  setter o v = o { _xDerivative = v }

-- | Help lens
yDerivative :: Lens' (Derivative px) (Holder px Float)
yDerivative = lens _yDerivative setter where
  setter o v = o { _yDerivative = v }

instance Applicative ParametricValues where
    pure a = ParametricValues a a a a
    ParametricValues n e s w <*> ParametricValues n' e' s' w' =
        ParametricValues (n n') (e e') (s s') (w w')

instance Foldable ParametricValues where
  foldMap f (ParametricValues n e s w) = f n <> f e <> f s <> f w

-- | Transpose (switch vertical/horizontal orientation) of values.
transposeParametricValues :: ParametricValues a -> ParametricValues a
transposeParametricValues (ParametricValues n e s w) = ParametricValues n w s e

-- | Describe a tensor patch
data TensorPatch weight = TensorPatch
  { _curve0 :: !CubicBezier
  , _curve1 :: !CubicBezier
  , _curve2 :: !CubicBezier
  , _curve3 :: !CubicBezier
  , _tensorValues :: !weight
  }

isVerticalOrientation :: TensorPatch a -> Bool
isVerticalOrientation p = dy > dx where
  CubicBezier a _ _ d = _curve0 p
  V2 dx dy = abs <$> (d ^-^ a)

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

-- | Define the boundary and interpolated values of a coon patch.
--
-- @
--                        ----->
--                  North     _____----------------+
--   ^          +------------//                     // .
--   |         //                                  //       |
--   |        //                                  //        |
--   |       //                                  //  east   |
--   | west |                                  /          |
--          |                                 |           v
--           \\                                 \\   .
--            \\                  __-------------+
--             +----------------/
--                    South
--                       <-----
-- @
--
data CoonPatch weight = CoonPatch
    { _north :: !CubicBezier -- ^ North border, from left to right at top
    , _east :: !CubicBezier  -- ^ East obrder, from top to bottom
    , _south :: !CubicBezier -- ^ South border from right to left
    , _west :: !CubicBezier  -- ^ West border from bottom to top
    , _coonValues :: !weight -- ^ The patch values
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
    -- | Points used to define tensor patch, if  not define,
    -- the rest of the data structure describes a Coon patch.
    -- size must be equal to `_meshPatchWidth*_meshPatchHeight`
  , _meshTensorDerivatives :: !(Maybe (V.Vector Derivatives))
  }
  deriving (Eq, Show, Functor)

-- | Store the two bezier control points of a bezier.
data InterBezier = InterBezier 
  { _inter0 :: !Point
  , _inter1 :: !Point
  }
  deriving (Eq, Show)

instance Transformable InterBezier where
  transform f (InterBezier a b) = InterBezier (f a) (f b)
  transformM f (InterBezier a b) = InterBezier <$> f a <*> f b

instance PointFoldable InterBezier where
  foldPoints f acc (InterBezier a b) = f (f acc a) b

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
      , _meshTensorDerivatives = Nothing
      }

instance {-# OVERLAPPING  #-} Transformable (MeshPatch px) where
  transformM = transformMeshM

instance {-# OVERLAPPING  #-} PointFoldable (MeshPatch px) where
  foldPoints = foldMeshPoints

foldMeshPoints :: (a -> Point -> a) -> a -> MeshPatch px -> a
foldMeshPoints f acc m = acc4 where
  acc1 = V.foldl' f acc (_meshPrimaryVertices m)
  acc2 = foldPoints f acc1 (_meshHorizontalSecondary m)
  acc3 = foldPoints f acc2 (_meshVerticalSecondary m)
  acc4 = case _meshTensorDerivatives m of
    Nothing -> acc3
    Just v -> foldPoints f acc3 v

-- | Store the inner points of a tensor patch.
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

instance PointFoldable Derivatives where
  foldPoints f acc (Derivatives a b c d) = f (f (f (f acc a) b) c) d

-- | Represent a point in the paramaetric U,V space
-- from [0, 1]^2
type UV = V2 CoonColorWeight

-- | Define a rectangle in the U,V parametric space.
type UVPatch = ParametricValues UV

-- | Store information for cubic interpolation in a patch.
newtype CubicCoefficient px = CubicCoefficient
    { getCubicCoefficients :: ParametricValues (V4 (Holder px Float))
    }

-- | Type storing the information to be able to interpolate
-- part of an image in a patch.
data ImageMesh px = ImageMesh
    { _meshImage :: !(Image px)
    , _meshTransform :: !Transformation
    }

-- C1: top      _north
-- C2: bottom   _south
-- D1: left     _west
-- D2: right    _east

-- | Return a postion of a point in the coon patch.
coonPointAt :: CoonPatch a -> UV -> Point
coonPointAt CoonPatch { .. } (V2 u v) = sc ^+^ sd ^-^ sb
  where
    CubicBezier c10 _ _ c11 = _north
    CubicBezier c21 _ _ c20 = _south

    sc = lerp v c2 c1
    sd = lerp u d2 d1
    sb = lerp v (lerp u c21 c20)
                (lerp u c11 c10)

    CubicBezier _ _ _ c1 = fst $ cubicBezierBreakAt _north u
    CubicBezier _ _ _ c2 = fst $ cubicBezierBreakAt _south (1 - u)

    CubicBezier _ _ _ d2 = fst $ cubicBezierBreakAt _east v
    CubicBezier _ _ _ d1 = fst $ cubicBezierBreakAt _west (1 - v)

-- | Convert a coon patch in
toTensorPatch :: CoonPatch a -> TensorPatch a
toTensorPatch CoonPatch { .. } = TensorPatch
    { _curve0 = _north
    , _curve1 = CubicBezier wt p11 p21 et
    , _curve2 = CubicBezier wb p12 p22 eb
    , _curve3 = CubicBezier sd  sc  sb sa
    , _tensorValues = _coonValues
    }
  where
    formula a b c d e f g h =
      (a ^* (-4) ^+^
       (b ^+^ c) ^* 6 ^-^
       (d ^+^ e) ^* 2 ^+^
       (f ^+^ g) ^* 3 ^-^
       h) ^* (1/9)

    p11 = formula p00 p10 p01 p30 p03 p13 p31 p33
    p21 = formula p30 p20 p31 p00 p33 p23 p01 p03
    p12 = formula p03 p13 p02 p33 p00 p10 p32 p30
    p22 = formula p33 p23 p32 p03 p30 p20 p02 p00

    CubicBezier p00 p10 p20 p30 = _north
    CubicBezier _ p02 p01 _ = _west
    CubicBezier _ p31 p32 _ = _east
    CubicBezier p33 p23 p13 p03 = _south

    CubicBezier sa sb sc sd = _south
    CubicBezier _ et eb _ = _east
    CubicBezier _ wb wt _ = _west


