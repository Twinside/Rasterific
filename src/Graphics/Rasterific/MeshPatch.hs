{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#define SVG_2
-- | Module defining the type of mesh patch grid.
module Graphics.Rasterific.MeshPatch
    ( -- * Types
      InterBezier( .. )
    , Derivatives( .. )
    , MeshPatch( .. )
    , CubicCoefficient( .. )

     -- * Functions
    , calculateMeshColorDerivative
    , verticeAt
    , generateLinearGrid
    , generateImageMesh

      -- * Extraction functions
      -- ** Simple
    , coonPatchAt
    , tensorPatchAt
    , coonImagePatchAt
    , tensorImagePatchAt
    , coonPatchAtWithDerivative
    , tensorPatchAtWithDerivative

      -- ** Multiple
    , coonPatchesOf
    , tensorPatchesOf
    , imagePatchesOf
    , tensorImagePatchesOf
    , cubicCoonPatchesOf
    , cubicTensorPatchesOf

      -- * Mutable mesh
    , MutableMesh
    , thawMesh
    , freezeMesh

     -- * Monadic mesh creation
    , withMesh
    , setVertice
    , getVertice
    , setHorizPoints
    , setVertPoints
    , setColor
    ) where

{-import Debug.Trace-}
{-import Text.Printf-}

import Data.Monoid( (<>) )
import Control.Monad.ST( runST )
import Control.Monad.Reader( runReaderT )
import Control.Monad.Reader.Class
import Control.Monad.Primitive( PrimMonad, PrimState )
import Data.Vector( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as VG

import Codec.Picture( Image( imageWidth, imageHeight ) )
import Graphics.Rasterific.Linear
import Graphics.Rasterific.MiniLens
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PatchTypes

#ifdef SVG_2
slopeOf :: (Additive h, Applicative h)
        => h Float -> h Float -> h Float
        -> Point -> Point -> Point
        -> h Float
slopeOf prevColor thisColor nextColor
        prevPoint thisPoint nextPoint 
  | nearZero distPrev || nearZero distNext = zero
  | otherwise = slopeVal <$> slopePrev <*> slope <*> slopeNext
  where
    distPrev = thisPoint `distance` prevPoint
    distNext = thisPoint `distance` nextPoint

    slopePrev | nearZero distPrev = zero
              | otherwise = (thisColor ^-^ prevColor) ^/ distPrev
    slopeNext | nearZero distNext = zero
              | otherwise = (nextColor ^-^ thisColor) ^/ distNext
    slope = (slopePrev ^+^ slopeNext) ^* 0.5

    slopeVal :: Float -> Float -> Float -> Float
    slopeVal sp s sn
      | signum sp /= signum sn = 0
      | abs s > abs minSlope = minSlope
      | otherwise = s
      where
        minSlope
          | abs sp < abs sn = 3 * sp
          | otherwise = 3 * sn
#else
slopeBasic :: (Additive h)
           => h Float -> h Float
           -> Point -> Point
           -> h Float
slopeBasic prevColor nextColor prevPoint nextPoint 
  | nearZero d = zero
  | otherwise = (nextColor ^-^ prevColor) ^/ d
  where
    d = prevPoint `distance` nextPoint
#endif

-- | Prepare a gradient mesh to use cubic color interpolation, see
-- renderCubicMesh documentation to see the global use of this function.
calculateMeshColorDerivative :: forall px. (InterpolablePixel px)
                             => MeshPatch px -> MeshPatch (Derivative px)
calculateMeshColorDerivative mesh = mesh { _meshColors = withEdgesDerivatives } where
  withEdgesDerivatives =
     colorDerivatives V.// (topDerivative <> bottomDerivative <> leftDerivative <> rightDerivative)
  colorDerivatives =
     V.fromListN (w * h) [interiorDerivative x y | y <- [0 .. h - 1], x <- [0 .. w - 1]]

  w = _meshPatchWidth mesh + 1
  h = _meshPatchHeight mesh + 1
  clampX = max 0 . min (w - 1)
  clampY = max 0 . min (h - 1)

  rawColorAt x y =_meshColors mesh V.! (y * w + x)
  atColor x y = toFloatPixel $ rawColorAt (clampX x) (clampY y)
#ifdef SVG_2
  isOnVerticalBorder x = x == 0 || x == w - 1 
  isOnHorizontalBorder y = y == 0 || y == h - 1
#endif

  pointAt x y = verticeAt mesh (clampX x) (clampY y)
  derivAt x y = colorDerivatives  V.! (y * w + x)


  topDerivative = 
    [edgeDerivative yDerivative 0 1 x 0 | x <- [1 .. w - 2]]
  bottomDerivative = 
    [edgeDerivative yDerivative 0 (-1) x (h - 1) | x <- [1 .. w - 2]]
  leftDerivative =
    [edgeDerivative xDerivative 1 0 0 y | y <- [1 .. h - 2]]
  rightDerivative = 
    [edgeDerivative xDerivative (-1) 0 (w - 1) y | y <- [1 .. h - 2]]

  edgeDerivative :: Lens' (Derivative px) (Holder px Float) -> Int -> Int -> Int -> Int
                 -> (Int, Derivative px)
  edgeDerivative coord dx dy x y
    | nearZero d = (ix, oldDeriv)
    | otherwise = (ix, oldDeriv & coord .~ otherDeriv)
    where
      ix = y * w + x
      oldDeriv = derivAt x y
      derivs = oldDeriv .^ coord
      otherDeriv = (c ^/ d) ^-^ derivs
      c = (atColor (x+dx) (y+dy) ^-^ atColor x y) ^* 2
      d = pointAt (x+dx) (y+dy) `distance` pointAt x y

  -- General case
  interiorDerivative x y
#ifdef SVG_2
    | isOnHorizontalBorder y && isOnVerticalBorder x = Derivative thisColor zero zero zero
    | isOnHorizontalBorder y = Derivative thisColor dx zero zero
    | isOnVerticalBorder x = Derivative thisColor zero dy zero
#endif
    | otherwise = Derivative thisColor dx dy dxy
    where
#ifdef SVG_2
      dx = slopeOf
          cxPrev thisColor cxNext
          xPrev this xNext
      
      dy = slopeOf
          cyPrev thisColor cyNext
          yPrev this yNext
          -- -}
      
      dxy = zero
#else
      dx = slopeBasic cxPrev cxNext xPrev xNext
      dy = slopeBasic cyPrev cyNext yPrev yNext

      dxy | nearZero xyDist = zero
          | otherwise = (cxyNext ^-^ cyxPrev ^-^ cyxNext ^+^ cxyPrev) ^/ (xyDist)
      xyDist = (xNext `distance` xPrev) * (yNext `distance` yPrev)

      cxyPrev = atColor (x - 1) (y - 1)
      xyPrev = pointAt (x - 1) (y - 1)

      cxyNext = atColor (x + 1) (y + 1)
      xyNext = pointAt (x + 1) (y + 1)

      cyxPrev = atColor (x - 1) (y + 1)
      yxPrev = pointAt (x - 1) (y + 1)

      cyxNext = atColor (x + 1) (y - 1)
      yxNext = pointAt (x + 1) (y - 1)
#endif

      cxPrev = atColor (x - 1) y
      thisColor = atColor x y
      cxNext = atColor (x + 1) y
      
      cyPrev = atColor x (y - 1)
      cyNext = atColor x (y + 1)
      
      xPrev = pointAt (x - 1) y
      this  = pointAt x y
      xNext = pointAt (x + 1) y
      
      yPrev = pointAt x (y - 1)
      yNext = pointAt x (y + 1)

-- | Mutable version of a MeshPatch
data MutableMesh s px = MutableMesh
  { _meshMutWidth :: !Int
  , _meshMutHeight :: !Int
  , _meshMutPrimaryVertices :: !(MV.MVector s Point)
  , _meshMutHorizSecondary :: !(MV.MVector s InterBezier)
  , _meshMutVertSecondary :: !(MV.MVector s InterBezier)
  , _meshMutColors :: !(MV.MVector s px)
  , _meshMutTensorDerivatives :: !(Maybe (MV.MVector s Derivatives))
  }

-- | Normal mesh to mutable mesh
thawMesh :: PrimMonad m => MeshPatch px -> m (MutableMesh (PrimState m) px)
thawMesh MeshPatch { .. } = do
  let _meshMutWidth = _meshPatchWidth
      _meshMutHeight = _meshPatchHeight
  _meshMutPrimaryVertices <- V.thaw _meshPrimaryVertices 
  _meshMutHorizSecondary <- V.thaw _meshHorizontalSecondary
  _meshMutVertSecondary <- V.thaw _meshVerticalSecondary
  _meshMutColors <- V.thaw _meshColors
  _meshMutTensorDerivatives <- case _meshTensorDerivatives of
      Nothing -> return Nothing
      Just v -> Just <$> V.thaw v
  return MutableMesh { .. }

-- | Mutable mesh to freezed mesh.
freezeMesh :: PrimMonad m => MutableMesh (PrimState m) px -> m (MeshPatch px)
freezeMesh MutableMesh { .. } = do
  let _meshPatchWidth = _meshMutWidth
      _meshPatchHeight = _meshMutHeight
  _meshPrimaryVertices <- V.freeze _meshMutPrimaryVertices 
  _meshHorizontalSecondary <- V.freeze _meshMutHorizSecondary
  _meshVerticalSecondary <- V.freeze _meshMutVertSecondary
  _meshTensorDerivatives <- case _meshMutTensorDerivatives of
        Nothing -> return Nothing
        Just v -> Just <$> V.freeze v
  _meshColors <- V.freeze _meshMutColors
  return MeshPatch { .. }

-- | Retrieve a mesh primary vertex purely
verticeAt :: MeshPatch px
          -> Int -- ^ Between 0 and _meshPatchWidth + 1 (excluded)
          -> Int -- ^ Between 0 and _meshPatchHeight + 1 (excluded)
          -> Point
verticeAt m x y = _meshPrimaryVertices m ! idx where
    idx = y * (_meshPatchWidth m + 1) + x

-- | Given an original MeshPatch, provide context to mutate it through
-- modification functions.
withMesh :: MeshPatch px
         -> (forall m. (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m) =>
                        m a)
         -> (a, MeshPatch px)
withMesh mesh act = runST $ do
  mut <- thawMesh  mesh
  v <- runReaderT act mut
  final <- freezeMesh mut
  return (v, final)

-- | Set the vertex of a mesh at a given coordinate
setVertice :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
           => Int   -- ^ x coordinate in [0, w]
           -> Int   -- ^ y coordinate in [0, h]
           -> Point -- ^ new point value
           -> m ()
setVertice x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutPrimaryVertices idx p

-- | Get the position of vertex
getVertice :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
           => Int -> Int -> m Point
getVertice x y = do
  p <- ask
  let idx = y * (_meshMutWidth p + 1) + x
  MV.read (_meshMutPrimaryVertices p) idx

-- | Set the two control bezier points horizontally
setHorizPoints :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
               => Int -> Int -> InterBezier -> m ()
setHorizPoints x y p = do
  MutableMesh { .. } <- ask
  let idx = y * _meshMutWidth + x
  MV.write _meshMutHorizSecondary idx p

-- | Set the two control bezier points vertically
setVertPoints :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
              => Int -> Int -> InterBezier -> m ()
setVertPoints x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutVertSecondary idx p


-- | Set the value associated with a vertex
setColor :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
         => Int -> Int -> px -> m ()
setColor x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutColors idx p

-- | Generate a meshpatch at the size given by the image and
-- a number of cells in a mesh
generateImageMesh :: Int      -- ^ Horizontal cell count
                  -> Int      -- ^ Vertical cell count
                  -> Point    -- ^ Position of the corner upper left
                  -> Image px -- ^ Image to transform through a mesh
                  -> MeshPatch (ImageMesh px)
generateImageMesh w h base img = generateLinearGrid w h base (V2 dx dy) infos where
  dx = fromIntegral (imageWidth img) / fromIntegral w
  dy = fromIntegral (imageHeight img) / fromIntegral h
  infos = V.fromListN ((w + 1) * (h + 1))
    [ImageMesh img $ trans <> scaling
        | y <- [0 .. h]
        , x <- [0 .. w]
        , let fx = fromIntegral x
              fy = fromIntegral y
              trans = translate (V2 (fx * dx) (fy * dy))
              scaling = scale dx dy]


-- | Generate a valid gradient with the shape of a simple grid
-- using some simple information. You can use `thawMesh` and `freezeMesh`
-- to mutate it.
generateLinearGrid :: Int           -- ^ Width in patch
                   -> Int           -- ^ Height in patch
                   -> Point         -- ^ Position of the upper left corner
                   -> V2 Float      -- ^ Size of each patch in x adn y
                   -> V.Vector px   -- ^ Vector of values, size must be (width + 1) * (height + 1)
                   -> MeshPatch px
generateLinearGrid w h base (V2 dx dy) colors = MeshPatch
  { _meshPatchWidth = w
  , _meshPatchHeight = h
  , _meshPrimaryVertices = vertices 
  , _meshHorizontalSecondary = hSecondary 
  , _meshVerticalSecondary = vSecondary
  , _meshTensorDerivatives = Nothing
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

type ColorPreparator px pxt = ParametricValues px -> pxt

-- | Extract a Coons patch at a given position.
coonPatchAt :: MeshPatch px
            -> Int -- ^ x
            -> Int -- ^ y
            -> CoonPatch (ParametricValues px)
coonPatchAt = coonPatchAt' id

-- | Extract a tensor patch at a given position
tensorPatchAt :: MeshPatch px
              -> Int -- ^ x
              -> Int -- ^ y
              -> TensorPatch (ParametricValues px)
tensorPatchAt = tensorPatchAt' id

-- | Extract an image patch out of a mesh at a given position.
coonImagePatchAt :: MeshPatch (ImageMesh px)
                 -> Int -- ^ x
                 -> Int -- ^ y
                 -> CoonPatch (ImageMesh px)
coonImagePatchAt = coonPatchAt' _northValue


-- | Extract a tensor image patch out of a mesh at
-- a given position.
tensorImagePatchAt :: MeshPatch (ImageMesh px)
                   -> Int -- ^ x
                   -> Int -- ^ y
                   -> TensorPatch (ImageMesh px)
tensorImagePatchAt = tensorPatchAt' _northValue

-- | Extract a Coons patch for cubic interpolation at a given position
-- see `calculateMeshColorDerivative`
coonPatchAtWithDerivative :: (InterpolablePixel px)
                          => MeshPatch (Derivative px)
                          -> Int -- ^ x
                          -> Int -- ^ y
                          -> CoonPatch (CubicCoefficient px)
coonPatchAtWithDerivative = coonPatchAt' cubicPreparator

-- | Extract a tensor patch for cubic interpolation at a given position
-- see `calculateMeshColorDerivative`
tensorPatchAtWithDerivative :: (InterpolablePixel px)
                            => MeshPatch (Derivative px)
                            -> Int -- ^ x
                            -> Int -- ^ y
                            -> TensorPatch (CubicCoefficient px)
tensorPatchAtWithDerivative = tensorPatchAt' cubicPreparator

rawMatrix :: V.Vector (V.Vector Float)
rawMatrix = V.fromListN 16 $ V.fromListN 16 <$>
  [ [ 1, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0 ]
  , [ 0, 0, 0, 0,  1, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0 ]
  , [-3, 3, 0, 0, -2,-1, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0 ]
  , [ 2,-2, 0, 0,  1, 1, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0 ]
  , [ 0, 0, 0, 0,  0, 0, 0, 0,  1, 0, 0, 0,  0, 0, 0, 0 ]
  , [ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  1, 0, 0, 0 ]
  , [ 0, 0, 0, 0,  0, 0, 0, 0, -3, 3, 0, 0, -2,-1, 0, 0 ]
  , [ 0, 0, 0, 0,  0, 0, 0, 0,  2,-2, 0, 0,  1, 1, 0, 0 ]
  , [-3, 0, 3, 0,  0, 0, 0, 0, -2, 0,-1, 0,  0, 0, 0, 0 ]
  , [ 0, 0, 0, 0, -3, 0, 3, 0,  0, 0, 0, 0, -2, 0,-1, 0 ]
  , [ 9,-9,-9, 9,  6, 3,-6,-3,  6,-6, 3,-3,  4, 2, 2, 1 ]
  , [-6, 6, 6,-6, -3,-3, 3, 3, -4, 4,-2, 2, -2,-2,-1,-1 ]
  , [ 2, 0,-2, 0,  0, 0, 0, 0,  1, 0, 1, 0,  0, 0, 0, 0 ]
  , [ 0, 0, 0, 0,  2, 0,-2, 0,  0, 0, 0, 0,  1, 0, 1, 0 ]
  , [-6, 6, 6,-6, -4,-2, 4, 2, -3, 3,-3, 3, -2,-1,-2,-1 ]
  , [ 4,-4,-4, 4,  2, 2,-2,-2,  2,-2, 2,-2,  1, 1, 1, 1 ]
  ]

cubicPreparator :: (InterpolablePixel px)
                => ParametricValues (Derivative px)
                -> CubicCoefficient px
cubicPreparator ParametricValues { .. } =
    CubicCoefficient $ ParametricValues (sliceAt 0) (sliceAt 4) (sliceAt 8) (sliceAt 12) where
  Derivative c00 fx00 fy00 fxy00 = _northValue
  Derivative c10 fx10 fy10 fxy10 = _eastValue
  Derivative c01 fx01 fy01 fxy01 = _westValue
  Derivative c11 fx11 fy11 fxy11 = _southValue

  resultVector = mulVec $ V.fromListN 16
    [  c00,   c10,   c01,   c11
    , fx00,  fx10,  fx01,  fx11 
    , fy00,  fy10,  fy01,  fy11 
    ,fxy00, fxy10, fxy01, fxy11
    ]

  mulVec vec = VG.foldl' (^+^) zero . VG.zipWith (^*) vec <$> rawMatrix

  sliceAt i = V4 
    (resultVector V.! i)
    (resultVector V.! (i + 1))
    (resultVector V.! (i + 2))
    (resultVector V.! (i + 3))

tensorPatchAt' :: ColorPreparator px pxt -> MeshPatch px -> Int -> Int
               -> TensorPatch pxt
tensorPatchAt' preparator mesh@MeshPatch { _meshTensorDerivatives = Nothing } x y =
    toTensorPatch $ coonPatchAt' preparator mesh x y
tensorPatchAt' preparator mesh x y = TensorPatch
  { _curve0 = CubicBezier p00 p01 p02 p03
  , _curve1 = CubicBezier p10 p11 p12 p13
  , _curve2 = CubicBezier p20 p21 p22 p23
  , _curve3 = CubicBezier p30 p31 p32 p33
  , _tensorValues = preparator $ ParametricValues
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

    Derivatives p11 p12 p21 p22 = case _meshTensorDerivatives mesh of
      Nothing -> error "Not a tensor patch"
      Just v -> v ! (w * y + x)


coonPatchAt' :: ColorPreparator px pxt
             -> MeshPatch px -> Int -> Int -> CoonPatch pxt
coonPatchAt' preparator mesh x y = CoonPatch 
    { _north = CubicBezier p00 p01 p02 p03
    , _east  = CubicBezier p03 p13 p23 p33
    , _south = CubicBezier p33 p32 p31 p30
    , _west  = CubicBezier p30 p20 p10 p00
    , _coonValues = preparator $ ParametricValues
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

-- | Extract a list of all the Coons patches of the mesh.
coonPatchesOf :: MeshPatch px -> [CoonPatch (ParametricValues px)]
coonPatchesOf mesh@MeshPatch { .. } =
  [coonPatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

-- | Extract a list of all the tensor patches of the mesh.
tensorPatchesOf :: MeshPatch px -> [TensorPatch (ParametricValues px)]
tensorPatchesOf mesh@MeshPatch { .. } =
  [tensorPatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

-- | Extract all the Coons patches of a mesh using an image interpolation.
imagePatchesOf :: MeshPatch (ImageMesh px) -> [CoonPatch (ImageMesh px)]
imagePatchesOf mesh@MeshPatch { .. } =
  [coonImagePatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

-- | Extract all the tensor patches of a mesh using an image interpolation.
tensorImagePatchesOf :: MeshPatch (ImageMesh px) -> [TensorPatch (ImageMesh px)]
tensorImagePatchesOf mesh@MeshPatch { .. } =
  [tensorImagePatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

-- | Extract all the Coons patches of a mesh using cubic interpolation.
cubicCoonPatchesOf :: (InterpolablePixel px)
                   => MeshPatch (Derivative px)
                   -> [CoonPatch (CubicCoefficient px)]
cubicCoonPatchesOf mesh@MeshPatch { .. } =
  [coonPatchAtWithDerivative mesh x y
        | y <- [0 .. _meshPatchHeight - 1]
        , x <- [0 .. _meshPatchWidth - 1] ]

-- | Extract all the tensor patches of a mesh using cubic interpolation.
cubicTensorPatchesOf :: (InterpolablePixel px)
                     => MeshPatch (Derivative px)
                     -> [TensorPatch (CubicCoefficient px)]
cubicTensorPatchesOf mesh@MeshPatch { .. } =
  [tensorPatchAtWithDerivative mesh x y
        | y <- [0 .. _meshPatchHeight - 1]
        , x <- [0 .. _meshPatchWidth - 1] ]

