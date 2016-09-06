{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Rasterific.MeshPatch
    ( InterBezier( .. )
    , Derivatives( .. )
    , MeshPatch( .. )
    , MutableMesh
    , thawMesh
    , setVertice
    , setHorizPoints
    , setVertPoints
    , setColor
    , generateLinearGrid
    , coonPatchAt
    , coonPatchesOf
    , renderCoonMesh
    , freezeMesh
    , withMesh
    ) where

import Control.Monad.ST( runST )
import Control.Monad.Reader( runReaderT )
import Control.Monad.Reader.Class
import Control.Monad.Primitive( PrimMonad, PrimState )
import Data.Vector( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Patch
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Immediate
{-import Control.Monad.State.Class-}

data InterBezier = InterBezier 
  { _inter0 :: !Point
  , _inter1 :: !Point
  }
  deriving (Eq, Show)

instance Transformable InterBezier where
  transform f (InterBezier a b) = InterBezier (f a) (f b)
  transformM f (InterBezier a b) = InterBezier <$> f a <*> f b

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

   -- | Derivatives used for Tensor patch
  , _meshDerivatives :: !(Maybe (V.Vector Derivatives))

   -- | Colors for each vertex points
  , _meshColors :: !(V.Vector px)
  }
  deriving (Eq, Show, Functor)

data MutableMesh s px = MutableMesh
  { _meshMutWidth :: !Int
  , _meshMutHeight :: !Int
  , _meshMutPrimaryVertices :: !(MV.MVector s Point)
  , _meshMutHorizSecondary :: !(MV.MVector s InterBezier)
  , _meshMutVertSecondary :: !(MV.MVector s InterBezier)
  , _meshMutDer :: !(Maybe (MV.MVector s Derivatives))
  , _meshMutColors :: !(MV.MVector s px)
  }

thawMesh :: PrimMonad m => MeshPatch px -> m (MutableMesh (PrimState m) px)
thawMesh MeshPatch { .. } = do
  let _meshMutWidth = _meshPatchWidth
      _meshMutHeight = _meshPatchHeight
  _meshMutPrimaryVertices <- V.thaw _meshPrimaryVertices 
  _meshMutHorizSecondary <- V.thaw _meshHorizontalSecondary
  _meshMutVertSecondary <- V.thaw _meshVerticalSecondary
  _meshMutDer <- traverse V.thaw _meshDerivatives
  _meshMutColors <- V.thaw _meshColors
  return MutableMesh { .. }

freezeMesh :: PrimMonad m => MutableMesh (PrimState m) px -> m (MeshPatch px)
freezeMesh MutableMesh { .. } = do
  let _meshPatchWidth = _meshMutWidth
      _meshPatchHeight = _meshMutHeight
  _meshPrimaryVertices <- V.freeze _meshMutPrimaryVertices 
  _meshHorizontalSecondary <- V.freeze _meshMutHorizSecondary
  _meshVerticalSecondary <- V.freeze _meshMutVertSecondary
  _meshDerivatives<- traverse V.freeze _meshMutDer 
  _meshColors <- V.freeze _meshMutColors
  return MeshPatch { .. }

withMesh :: MeshPatch px
         -> (forall m. (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m) =>
                        m a)
         -> (a, MeshPatch px)
withMesh mesh act = runST $ do
  mut <- thawMesh  mesh
  v <- runReaderT act mut
  final <- freezeMesh mut
  return (v, final)

setVertice :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
           => Int -> Int -> Point -> m ()
setVertice x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutPrimaryVertices idx p

setHorizPoints :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
               => Int -> Int -> InterBezier -> m ()
setHorizPoints x y p = do
  MutableMesh { .. } <- ask
  let idx = y * _meshMutWidth + x
  MV.write _meshMutHorizSecondary idx p

setVertPoints :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
              => Int -> Int -> InterBezier -> m ()
setVertPoints x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutVertSecondary idx p


setColor :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
         => Int -> Int -> px -> m ()
setColor x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutColors idx p

transformMeshM :: Monad m => (Point -> m Point) -> MeshPatch px -> m (MeshPatch px)
transformMeshM f MeshPatch { .. } = do
  vertices <- mapM f _meshPrimaryVertices
  hSecondary <- mapM (transformM f) _meshHorizontalSecondary
  vSecondary <- mapM (transformM f) _meshVerticalSecondary
  derivatives <- mapM (mapM (transformM f)) _meshDerivatives
  return $ MeshPatch
      { _meshPatchWidth = _meshPatchWidth 
      , _meshPatchHeight = _meshPatchHeight
      , _meshPrimaryVertices = vertices 
      , _meshHorizontalSecondary = hSecondary 
      , _meshVerticalSecondary = vSecondary
      , _meshDerivatives = derivatives
      , _meshColors = _meshColors
      }

instance {-# OVERLAPPING  #-} Transformable (MeshPatch px) where
  transformM = transformMeshM

generateLinearGrid :: Int -> Int -> Point -> V2 Float -> V.Vector px
                   -> MeshPatch px
generateLinearGrid w h base (V2 dx dy) colors = MeshPatch
  { _meshPatchWidth = w
  , _meshPatchHeight = h
  , _meshPrimaryVertices = vertices 
  , _meshHorizontalSecondary = hSecondary 
  , _meshVerticalSecondary = vSecondary
  , _meshDerivatives = Nothing
  , _meshColors = colors
  }
  where
    vertexCount = (w + 1) * (h + 1)
    vertices =
      V.fromListN vertexCount [base ^+^ V2 (dx * fromIntegral x) (dy * fromIntegral y)
                                        | y <- [0 .. h], x <- [0 .. w]]
    at x y = vertices ! (y * (w + 1) + x)
    hSecondary = V.fromListN ((h + 1) * w)
        [InterBezier (p0 ^+^ delta ^* (1/3)) (p0 ^+^ delta ^* (2/3))
            | y <- [0 .. h], x <- [0 .. w - 1]
            , let p0 = at x y
                  p1 = at (x + 1) y
                  delta = p1 ^-^ p0
            ]

    vSecondary = V.fromListN ((w + 1) * h)
        [InterBezier (p0 ^+^ delta ^* (1/3)) (p0 ^+^ delta ^* (2/3))
            | y <- [0 .. h - 1], x <- [0 .. w]
            , let p0 = at x y
                  p1 = at x (y + 1)
                  delta = p1 ^-^ p0
            ]


coonPatchAt :: MeshPatch px -> Int -> Int -> CoonPatch px
coonPatchAt mesh x y = CoonPatch 
    { _north = CubicBezier p00 p01 p02 p03
    , _east  = CubicBezier p03 p13 p23 p33
    , _south = CubicBezier p33 p32 p31 p30
    , _west  = CubicBezier p30 p20 p10 p00
    , _coonValues = ParametricValues
        { _northValue = c00
        , _eastValue  = c03
        , _southValue = c33
        , _westValue  = c30
        }
    }
  where
    w = _meshPatchWidth mesh
    vertices = _meshPrimaryVertices mesh
    colors = _meshColors mesh
    
    hInter = _meshHorizontalSecondary mesh
    vInter = _meshVerticalSecondary mesh
    
    baseIx = (w + 1) * y + x
    p00 = vertices ! baseIx
    c00 = colors   ! baseIx
    
    p03 = vertices ! (baseIx + 1)
    c03 = colors   ! (baseIx + 1)
    
    p30 = vertices ! (baseIx + w + 1)
    c30 = colors   ! (baseIx + w + 1)
    p33 = vertices ! (baseIx + w + 2)
    c33 = colors   ! (baseIx + w + 2)
    
    baseH = w * y + x
    InterBezier p01 p02 = hInter ! baseH
    InterBezier p31 p32 = hInter ! (baseH + w)

    baseV = (w + 1) * y + x
    InterBezier p10 p20 = vInter ! baseV
    InterBezier p13 p23 = vInter ! (baseV + 1)

coonPatchesOf :: MeshPatch px -> [CoonPatch px]
coonPatchesOf mesh@MeshPatch { .. } =
  [coonPatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

renderCoonMesh :: (PrimMonad m, RenderablePixel px, InterpolablePixel px)
               => MeshPatch px -> DrawContext m px ()
renderCoonMesh = mapM_ renderCoonPatch . coonPatchesOf

