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

    , runDrawContext
    , fillWithTextureAndMask
    , fillWithTexture
    ) where

import qualified Data.Foldable as F
import Control.Monad.ST( ST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , Pixel8
                          , PixelRGBA8
                          , MutableImage( .. )
                          , unsafeFreezeImage
                          , fillImageWith )

import Control.Monad.Primitive( PrimState, PrimMonad, primToPrim )
import qualified Data.Vector.Storable.Mutable as M
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Linear( V2( .. ) )
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Shading
import Graphics.Rasterific.Types

-- | Monad used to describe the drawing context.
type DrawContext m px a =
    StateT (MutableImage (PrimState m) px) m a

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
mapExec f = go
  where
    go [] = return ()
    go (x : xs) = f x >> go xs

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
{-# SPECIALIZE fillWithTexture
    :: FillMethod -> Texture PixelRGBA8 -> [Primitive]
    -> DrawContext (ST s) PixelRGBA8 () #-}
{-# SPECIALIZE fillWithTexture
    :: FillMethod -> Texture Pixel8 -> [Primitive]
    -> DrawContext (ST s) Pixel8 () #-}
fillWithTexture fillMethod texture els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        !filler = primToPrim . transformTextureToFiller texture img
        clipped = F.foldMap (clip mini maxi) els
        spans = rasterize fillMethod clipped
    lift . mapExec filler $ filter (isCoverageDrawable img) spans

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
        spans = rasterize fillMethod $ F.foldMap (clip mini maxi) els
        !shader = primToPrim
                . transformTextureToFiller (modulateTexture texture mask) img
    lift . mapM_ shader $ filter (isCoverageDrawable img) spans


