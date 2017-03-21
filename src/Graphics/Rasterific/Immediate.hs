{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
-- | This module implements drawing primitives to draw directly into
-- the output texture, without generating an intermediate scene
-- representation.
--
-- If you need to draw complex scenes or plot an important set of
-- data, this is the module you should use. The downside is that
-- you must specify everything you need at each draw call, there
-- is no API to help you propagate constants.
--
-- The "stroking" must be done using the functions of the
-- `Graphics.Rasterific.Outline` module.
module Graphics.Rasterific.Immediate
    ( DrawContext
    , DrawOrder( .. )
    , orderToDrawing

    , runDrawContext
    , fillWithTextureAndMask
    , fillWithTexture
    , fillWithTextureNoAA
    , fillOrder

    , textToDrawOrders
    , transformOrder

    , meshToImage
    ) where


import Control.Monad.ST( ST, runST )
import Data.Maybe( fromMaybe )
import qualified Data.Foldable as F
import Control.Monad.Free( liftF )
import Control.Monad.State( evalStateT, execStateT, lift )
import Control.Monad.Trans.State( get )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , MutableImage( .. )
                          , unsafeFreezeImage
                          , fillImageWith )

import Control.Monad.Primitive( PrimMonad, primToPrim )
import qualified Data.Vector.Storable.Mutable as M
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Linear( V2( .. ) )
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Shading
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.Types
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.CubicBezier.FastForwardDifference
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific.ComplexPrimitive
import Graphics.Rasterific.Command
import Graphics.Rasterific.PlaneBoundable

import qualified Data.Vector.Unboxed as VU
import Graphics.Text.TrueType( Dpi, getStringCurveAtPoint )

-- | Reify a filling function call, to be able to manipulate
-- them in a simpler fashion.
data DrawOrder px = DrawOrder
    { -- | Primitives to be filled.
      _orderPrimitives :: ![[Primitive]]
      -- | Texture for the filled primitives.
    , _orderTexture    :: !(Texture px)
      -- | How to fill the primitives.
    , _orderFillMethod :: !FillMethod
      -- | Optional mask used for clipping.
    , _orderMask       :: !(Maybe (Texture (PixelBaseComponent px)))
      -- | Function to perform direct drawing
    , _orderDirect     :: !(forall s. DrawContext (ST s) px ())
    }

instance PlaneBoundable (DrawOrder px) where
  planeBounds =
    foldMap (foldMap planeBounds) . _orderPrimitives

transformOrder :: (Point -> Point) -> DrawOrder px -> DrawOrder px
transformOrder f order =
  order { _orderPrimitives = transform f $ _orderPrimitives order }

transformOrderM :: Monad m => (Point -> m Point) -> DrawOrder px -> m (DrawOrder px)
transformOrderM f order = do
  v <- transformM f $ _orderPrimitives order 
  return $ order { _orderPrimitives = v}

instance Transformable (DrawOrder px) where
  transform = transformOrder
  transformM = transformOrderM

-- | Transform back a low level drawing order to a more
-- high level Drawing
orderToDrawing :: DrawOrder px -> Drawing px ()
orderToDrawing order =
  usingTexture . mapM_ filler $ _orderPrimitives order
    where
      usingTexture sub =
          liftF $ SetTexture (_orderTexture order) sub ()
      filler prims =
          liftF $ Fill (_orderFillMethod order) prims ()

-- | Render the drawing orders on the canvas.
fillOrder :: (PrimMonad m, RenderablePixel px)
          => DrawOrder px -> DrawContext m px ()
fillOrder o@DrawOrder { _orderMask = Nothing } = do
  F.forM_ (_orderPrimitives o) $
    fillWithTexture (_orderFillMethod o) (_orderTexture o)
  img <- get
  lift $ primToPrim $ flip evalStateT img $ _orderDirect o

fillOrder o@DrawOrder { _orderMask = Just mask } = do
  F.forM_ (_orderPrimitives o) $
    fillWithTextureAndMask (_orderFillMethod o) (_orderTexture o) mask
  img <- get
  lift $ primToPrim $ flip evalStateT img $ _orderDirect o

-- | Start an image rendering. See `fillWithTexture` for
-- an usage example. This function can work with either
-- `IO` or `ST`.
runDrawContext :: forall m px . (PrimMonad m, RenderablePixel px)
               => Int   -- ^ Rendering width
               -> Int   -- ^ Rendering height
               -> px    -- ^ Background color
               -> DrawContext m px () -- ^ Actual drawing computation
               -> m (Image px)
runDrawContext width height background drawing = do
  buff <- M.new (width * height * componentCount background)
  let mutable = MutableImage width height buff
  fillImageWith mutable background
  img <- execStateT drawing mutable
  unsafeFreezeImage img

mapExec :: Monad m => (a -> m ()) -> [a] -> m ()
mapExec f = foldr ((>>) . f) (return ())

isCoverageDrawable :: MutableImage s px -> CoverageSpan -> Bool
isCoverageDrawable img coverage =
    _coverageVal coverage > 0 && x >= 0 && y >= 0 && x < imgWidth && y < imgHeight
  where
    !imgWidth = fromIntegral $ mutableImageWidth img
    !imgHeight = fromIntegral $ mutableImageHeight img
    x = _coverageX coverage
    y = _coverageY coverage

-- | Fill some geometry.
--
-- > immediateDrawExample :: Image PixelRGBA8
-- > immediateDrawExample = runST $
-- >   runDrawContext 200 200 (PixelRGBA8 0 0 0 0) $
-- >     fillWithTexture FillWinding texture geometry
-- >   where
-- >     circlePrimitives = circle (V2 100 100) 50
-- >     geometry = strokize 4 JoinRound (CapRound, CapRound) circlePrimitives
-- >     texture = uniformTexture (PixelRGBA8 255 255 255 255)
--
-- <<docimages/immediate_fill.png>>
--
fillWithTexture :: (PrimMonad m, RenderablePixel px)
                => FillMethod
                -> Texture px  -- ^ Color/Texture used for the filling
                -> [Primitive] -- ^ Primitives to fill
                -> DrawContext m px ()
fillWithTexture fillMethod texture els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        !filler = primToPrim . transformTextureToFiller meshToImage texture img
        clipped = foldMap (clip mini maxi) els
        spans = rasterize fillMethod clipped
    lift . mapExec filler $ filter (isCoverageDrawable img) spans

-- | Function identical to 'fillWithTexture' but with anti-aliasing
-- (and transparency) disabled.
fillWithTextureNoAA :: (PrimMonad m, RenderablePixel px)
                => FillMethod
                -> Texture px  -- ^ Color/Texture used for the filling
                -> [Primitive] -- ^ Primitives to fill
                -> DrawContext m px ()
fillWithTextureNoAA fillMethod texture els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        !filler = primToPrim . transformTextureToFiller meshToImage texture img
        clipped = foldMap (clip mini maxi) els
        spans = rasterize fillMethod clipped
    lift . mapExec (filler . toOpaqueCoverage) $ filter (isCoverageDrawable img) spans

-- | Fill some geometry using a composition mask for visibility.
--
-- > immediateDrawMaskExample :: Image PixelRGBA8
-- > immediateDrawMaskExample = runST $
-- >   runDrawContext 200 200 (PixelRGBA8 0 0 0 255) $
-- >     forM_ [1 .. 10] $ \ix ->
-- >        fillWithTextureAndMask FillWinding texture mask $
-- >            rectangle (V2 10 (ix * 18 - 5)) 180 13
-- >   where
-- >     texture = uniformTexture $ PixelRGBA8 0 0x86 0xc1 255
-- >     mask = sampledImageTexture
-- >          $ runST
-- >          $ runDrawContext 200 200 0
-- >          $ fillWithTexture FillWinding (uniformTexture 255) maskGeometry
-- > 
-- >     maskGeometry = strokize 15 JoinRound (CapRound, CapRound)
-- >                  $ circle (V2 100 100) 80
--
-- <<docimages/immediate_mask.png>>
--
fillWithTextureAndMask
    :: (PrimMonad m, RenderablePixel px)
    => FillMethod
    -> Texture px  -- ^ Color/Texture used for the filling of the geometry
    -> Texture (PixelBaseComponent px) -- ^ Texture used for the mask.
    -> [Primitive]                     -- ^ Primitives to fill
    -> DrawContext m px ()
fillWithTextureAndMask fillMethod texture mask els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize fillMethod $ foldMap (clip mini maxi) els
        !shader = primToPrim
                . transformTextureToFiller meshToImage (ModulateTexture texture mask) img
    lift . mapM_ shader $ filter (isCoverageDrawable img) spans

-- | Helper function transforming text range to draw order.
textToDrawOrders :: Dpi             -- ^ Current output device resolution
                 -> Texture px      -- ^ Texture to use if no texture is defined in the range
                 -> Point           -- ^ Baseline position
                 -> [TextRange px]  -- ^ Text description.
                 -> [DrawOrder px]
textToDrawOrders dpi defaultTexture (V2 x y) descriptions = 
    toOrder <$> zip floatCurves linearDescriptions where

  toOrder (curve, d) = DrawOrder 
    { _orderPrimitives = [beziersOfChar curve]
    , _orderFillMethod = FillWinding
    , _orderMask = Nothing
    , _orderTexture = fromMaybe defaultTexture $ _textTexture d
    , _orderDirect = return ()
    }

  floatCurves =
    getStringCurveAtPoint dpi (x, y)
      [(_textFont d, _textSize d, _text d) | d <- descriptions]

  linearDescriptions =
    concat [map (const d) $ _text d | d <- descriptions]

  beziersOfChar curves = concat
    [fmap BezierPrim . bezierFromPath . fmap (uncurry V2) $ VU.toList c | c <- curves]


meshToImage :: forall px. (RenderablePixel px)
            => Maybe Transformation -> Int-> Int 
            -> PatchInterpolation -> MeshPatch px
            -> Image px
meshToImage mayTrans width height i baseMesh 
  | not hasTransparency = rendering
  | otherwise = runST $ runDrawContext width height background $ fillOrder order
  where
    mesh = case mayTrans >>= inverseTransformation of
      Nothing -> baseMesh
      Just trans -> 
        transform (applyTransformation trans) baseMesh
    
    background = emptyPx :: px
    clipBackground = emptyValue :: PixelBaseComponent px
    
    rendering = runST $ runDrawContext width height background $ case i of
      PatchBilinear -> mapM_ rasterizeCoonPatch $ coonPatchesOf opaqueMesh 
      PatchBicubic ->
          mapM_ rasterizeCoonPatch
              . cubicCoonPatchesOf 
              $ calculateMeshColorDerivative opaqueMesh 
    
    hasTransparency =
        F.any ((/= fullValue) . pixelOpacity) $ _meshColors mesh
    
    opacifier px = mixWithAlpha (\_ _ a -> a) (\_ _ -> fullValue) px px
    
    opaqueMesh = opacifier <$> mesh
    transparencyMesh = pixelOpacity <$> mesh
    
    clipPath =
      runST $ runDrawContext width height clipBackground $ case i of
        PatchBilinear -> mapM_ rasterizeCoonPatch $ coonPatchesOf transparencyMesh
        PatchBicubic ->
            mapM_ rasterizeCoonPatch
                . cubicCoonPatchesOf 
                $ calculateMeshColorDerivative transparencyMesh
    
    order = DrawOrder
          { _orderPrimitives = [rectangle (V2 0 0) (fromIntegral width) (fromIntegral height)]
          , _orderTexture    = AlphaModulateTexture (RawTexture rendering) (RawTexture clipPath)
          , _orderFillMethod = FillWinding
          , _orderMask       = Nothing
          , _orderDirect     = return ()
          }
