{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module defining the type of mesh patch grid.
module Graphics.Rasterific.MeshPatch
    ( -- * Types
      InterBezier( .. )
    , Derivatives( .. )
    , MeshPatch( .. )

     -- * Functions
    , calculateMeshColorDerivative
    , verticeAt
    , generateLinearGrid
    , coonPatchAt
    , coonPatchAtWithDerivative
    , coonPatchesOf
    , cubicCoonPatchesOf

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

import Data.Foldable( foldl' )
import Data.Monoid( (<>) )
import Control.Monad.ST( runST )
import Control.Monad.Reader( runReaderT )
import Control.Monad.Reader.Class
import Control.Monad.Primitive( PrimMonad, PrimState )
import Data.Vector( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Graphics.Rasterific.Linear
import Graphics.Rasterific.MiniLens
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.PatchTypes

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

slopeBasic :: (Additive h, Applicative h)
           => h Float -> h Float
           -> Point -> Point
           -> h Float
slopeBasic prevColor nextColor prevPoint nextPoint 
  | nearZero d = zero
  | otherwise = (nextColor ^-^ prevColor) ^/ d
  where
    d = prevPoint `distance` nextPoint
          
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
      {-| signum sp /= signum sn = 0-}
      {-| abs s > abs minSlope = minSlope-}
      | otherwise = s
      where
        {-minSlope-}
          {-| abs sp < abs sn = 3 * sp-}
          {-| otherwise = 3 * sn-}

calculateMeshColorDerivative :: forall px. (InterpolablePixel px, Show (Derivative px))
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
  isOnVerticalBorder x = x == 0 || x == w - 1 
  isOnHorizontalBorder y = y == 0 || y == h - 1

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
    {-| isOnHorizontalBorder y && isOnVerticalBorder x = Derivative thisColor zero zero-}
    {-| isOnHorizontalBorder y = Derivative thisColor dx zero-}
    {-| isOnVerticalBorder x = Derivative thisColor zero dy-}
    | otherwise = Derivative thisColor dx dy dxy
    where
      dx = slopeBasic cxPrev cxNext xPrev xNext
      dy = slopeBasic cyPrev cyNext yPrev yNext

{-
      dx = slopeOf
          cxPrev thisColor cxNext
          xPrev this xNext
      
      dy = slopeOf
          cyPrev thisColor cyNext
          yPrev this yNext
          -- -}
      
      dxy | nearZero xyDist = zero
          | otherwise = (cxyNext ^-^ cyxPrev ^-^ cyxNext ^+^ cxyPrev) ^/ (xyDist)
      xyDist = (xNext `distance` xPrev) * (yNext `distance` yPrev)
      {-  
y12a[j][k]=(ya[j+1][k+1]-
            ya[j+1][k-1]-
            ya[j-1][k+1]+
            ya[j-1][k-1])
/((x1a[j+1]-x1a[j-1])*(x2a[k+1]-x2a[k-1]));
---}
      cxyPrev = atColor (x - 1) (y - 1)
      xyPrev = pointAt (x - 1) (y - 1)

      cxyNext = atColor (x + 1) (y + 1)
      xyNext = pointAt (x + 1) (y + 1)

      cyxPrev = atColor (x - 1) (y + 1)
      yxPrev = pointAt (x - 1) (y + 1)

      cyxNext = atColor (x + 1) (y - 1)
      yxNext = pointAt (x + 1) (y - 1)

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
  }

thawMesh :: PrimMonad m => MeshPatch px -> m (MutableMesh (PrimState m) px)
thawMesh MeshPatch { .. } = do
  let _meshMutWidth = _meshPatchWidth
      _meshMutHeight = _meshPatchHeight
  _meshMutPrimaryVertices <- V.thaw _meshPrimaryVertices 
  _meshMutHorizSecondary <- V.thaw _meshHorizontalSecondary
  _meshMutVertSecondary <- V.thaw _meshVerticalSecondary
  _meshMutColors <- V.thaw _meshColors
  return MutableMesh { .. }

freezeMesh :: PrimMonad m => MutableMesh (PrimState m) px -> m (MeshPatch px)
freezeMesh MutableMesh { .. } = do
  let _meshPatchWidth = _meshMutWidth
      _meshPatchHeight = _meshMutHeight
  _meshPrimaryVertices <- V.freeze _meshMutPrimaryVertices 
  _meshHorizontalSecondary <- V.freeze _meshMutHorizSecondary
  _meshVerticalSecondary <- V.freeze _meshMutVertSecondary
  _meshColors <- V.freeze _meshMutColors
  return MeshPatch { .. }

-- | Retrieve a mesh primary vertice purely
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

-- | Set the vertice of a mesh at a given coordinate
setVertice :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
           => Int   -- ^ x coordinate in [0, w]
           -> Int   -- ^ y coordinate in [0, h]
           -> Point -- ^ new point value
           -> m ()
setVertice x y p = do
  MutableMesh { .. } <- ask
  let idx = y * (_meshMutWidth + 1) + x
  MV.write _meshMutPrimaryVertices idx p


getVertice :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
           => Int -> Int -> m Point
getVertice x y = do
  p <- ask
  let idx = y * (_meshMutWidth p + 1) + x
  MV.read (_meshMutPrimaryVertices p) idx

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

generateLinearGrid :: Int -> Int -> Point -> V2 Float -> V.Vector px
                   -> MeshPatch px
generateLinearGrid w h base (V2 dx dy) colors = MeshPatch
  { _meshPatchWidth = w
  , _meshPatchHeight = h
  , _meshPrimaryVertices = vertices 
  , _meshHorizontalSecondary = hSecondary 
  , _meshVerticalSecondary = vSecondary
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

type ColorPreparator px pxt = ParametricValues px -> ParametricValues pxt

coonPatchAt :: MeshPatch px -> Int -> Int -> CoonPatch px
coonPatchAt = coonPatchAt' id

coonPatchAtWithDerivative :: (InterpolablePixel px, Show (Holder px Float))
                          => MeshPatch (Derivative px) -> Int -> Int
                          -> CoonPatch (V4 (Holder px Float))
coonPatchAtWithDerivative = coonPatchAt' cubicPreparator


rawMatrix :: [[Float]]
rawMatrix =
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

{-mulVec :: [[Float]] -> [Float] -> [Float]-}
mulVec mtrx vec = [foldl' (^+^) zero $ zipWith (^*) vec l | l <- mtrx]

cubicPreparator :: (InterpolablePixel px, Show (Holder px Float))
                => ParametricValues (Derivative px)
                -> ParametricValues (V4 (Holder px Float))
cubicPreparator ParametricValues { .. } = ParametricValues a' b' c' d' where
  Derivative c00 fx00 fy00 fxy00 = _northValue
  Derivative c10 fx10 fy10 fxy10 = _eastValue
  Derivative c01 fx01 fy01 fxy01 = _westValue
  Derivative c11 fx11 fy11 fxy11 = _southValue

  vec = [  c00,   c10,   c01,   c11
        , fx00,  fx10,  fx01,  fx11 
        , fy00,  fy10,  fy01,  fy11 
        ,fxy00, fxy10, fxy01, fxy11
        ]

  a' = V4 a b c d
  b' = V4 e f g h
  c' = V4 i j k l
  d' = V4 m n o p
  [a, b, c, d
    ,e, f, g, h
    ,i, j, k, l
    ,m, n, o, p] = mulVec rawMatrix vec

{-  
  a = V4
    c00
    fx00
    ((c10 ^-^ c00) ^* 3 ^-^ fx00 ^* 2 ^-^ fx10)
    ((c00 ^-^ c10) ^* 2 ^+^ fx00 ^+^ fx10)

  b = V4
    fy00
    zero
    ((fy00 ^-^ fy10) ^* (-3))
    ((fy00 ^-^ fy10) ^* 2)

  c = V4
    ((c01 ^-^ c00) ^* 3 ^-^ fy00 ^* 2 ^-^ fy01)
    ((fx01 ^-^ fx00) ^* 3)
    (((c00 ^-^ c01 ^-^ c10 ^+^ c11) ^* 3 ^+^
      (fx00 ^-^ fx01 ^+^ fy00 ^-^ fy10) ^* 2 ^+^ 
      fx10 ^-^ fx11 ^+^ fy01 ^-^ fy11) ^* 3)
    ((c01 ^-^ c00 ^+^ c10 ^-^ c11) ^* 6 ^+^
     (fx01 ^-^ fx00 ^-^ fx10 ^+^ fx11) ^* 3 ^+^
     (fy10 ^-^ fy00) ^* 4 ^+^
     (fy11 ^-^ fy01) ^* 2 )

  d = V4
    ((c00 ^-^ c01) ^* 2 ^+^ fy00 ^+^ fy01)
    ((fx00 ^-^ fx01) ^* 2)
    ((c01 ^-^ c00 ^+^ c10 ^-^ c11) ^* 6 ^+^
     (fx01 ^-^ fx00) ^* 4 ^+^
     (fx11 ^-^ fx10) ^* 2 ^+^
     (fy10 ^-^ fy00 ^-^ fy01 ^+^ fy11) ^* 3)
    (((c00 ^-^ c01 ^-^ c10 ^+^ c11) ^* 2 ^+^
       fx00 ^-^ fx01 ^+^ fx10 ^-^ fx11 ^+^ fy00 ^+^ fy01 ^-^ fy10 ^-^ fy11) ^* 2)
---}
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

coonPatchesOf :: MeshPatch px -> [CoonPatch px]
coonPatchesOf mesh@MeshPatch { .. } =
  [coonPatchAt mesh x y | y <- [0 .. _meshPatchHeight - 1], x <- [0 .. _meshPatchWidth - 1]]

cubicCoonPatchesOf :: (InterpolablePixel px, Show (Holder px Float))
                   => MeshPatch (Derivative px) -> [CoonPatch (V4 (Holder px Float))]
cubicCoonPatchesOf mesh@MeshPatch { .. } =
  [coonPatchAtWithDerivative mesh x y
        | y <- [0 .. _meshPatchHeight - 1]
        , x <- [0 .. _meshPatchWidth - 1] ]

