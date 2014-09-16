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
-- If you need to draw complex drawing or plot an important set of
-- data, this is the module you should use.
module Graphics.Rasterific.Immediate
    ( DrawContext

    , fillWithTextureAndMask
    , fillWithTexture
    , runDrawContext
    ) where

import qualified Data.Foldable as F
import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , Pixel8
                          , PixelRGBA8
                          , MutableImage( .. )
                          , unsafeFreezeImage
                          , fillImageWith )

import Control.Monad.Primitive( PrimState, PrimMonad )
import qualified Data.Vector.Storable.Mutable as M
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Linear( V2( .. ) )
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Shading
import Graphics.Rasterific.Types

-- | Monad used to describe the drawing context.
type DrawContext s px a =
    StateT (MutableImage s px) (ST s) a

runDrawContext :: forall m px . (PrimMonad m, RenderablePixel px)
               => Int   -- ^ Rendering width
               -> Int   -- ^ Rendering height
               -> px    -- ^ Background color
               -> DrawContext (PrimState m) px ()
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

-- | Fill some geometry. The geometry should be "looping",
-- ie. the last point of the last primitive should
-- be equal to the first point of the first primitive.
--
-- The primitive should be connected.
fillWithTexture :: RenderablePixel px
                => FillMethod
                -> Texture px  -- ^ Color/Texture used for the filling
                -> [Primitive] -- ^ Primitives to fill
                -> DrawContext s px ()
{-# SPECIALIZE fillWithTexture
    :: FillMethod -> Texture PixelRGBA8 -> [Primitive]
    -> DrawContext s PixelRGBA8 () #-}
{-# SPECIALIZE fillWithTexture
    :: FillMethod -> Texture Pixel8 -> [Primitive]
    -> DrawContext s Pixel8 () #-}
fillWithTexture fillMethod texture els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        !filler = transformTextureToFiller texture img
        clipped = F.foldMap (clip mini maxi) els
        spans = rasterize fillMethod clipped
    lift . mapExec filler $ filter (isCoverageDrawable img) spans

fillWithTextureAndMask
    :: RenderablePixel px
    => FillMethod
    -> Texture px  -- ^ Color/Texture used for the filling
    -> Texture (PixelBaseComponent px)
    -> [Primitive] -- ^ Primitives to fill
    -> DrawContext s px ()
fillWithTextureAndMask fillMethod texture mask els = do
    img@(MutableImage width height _) <- get
    let !mini = V2 0 0
        !maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize fillMethod $ F.foldMap (clip mini maxi) els
        !shader = transformTextureToFiller (modulateTexture texture mask) img
    lift . mapM_ shader $ filter (isCoverageDrawable img) spans


