{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Main module of Rasterific, an Haskell rasterization engine.
module Graphics.Rasterific
    ( Bezier( .. )
    , CubicBezier( .. )
    , Polygon( .. )
    , Texture
    , Compositor
    , DrawContext
    , Modulable
    , uniformTexture
    , renderContext
    , fill
    , stroke
    , compositionDestination
    , compositionAlpha
    ) where

import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , MutableImage( .. )
                          , createMutableImage
                          , unsafeFreezeImage )

import Linear( V2( .. ) )
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Compositor
{-import Graphics.Rasterific.Operators-}
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Polygon
import Graphics.Rasterific.Types

-- | Monad used to describe the drawing context.
type DrawContext s a px = StateT (MutableImage s px) (ST s) a

-- | Function to call in order to start the image creation.
renderContext :: (Pixel px)
              => Int -> Int -> px -> (forall s. DrawContext s a px) -> Image px
renderContext width height background drawing = runST $
  createMutableImage width height background
        >>= execStateT drawing
        >>= unsafeFreezeImage

stroke :: ( Pixel px, Modulable (PixelBaseComponent px) , Strokable el)
       => Texture px -> Float -> Join -> (Cap, Cap)
       -> [el] -> DrawContext s () px
stroke texture width join caping =
    fill texture . strokize width join caping

fill :: (Pixel px, Modulable (PixelBaseComponent px), Rasterizable el)
     => Texture px -> [el] -> DrawContext s () px
fill texture els = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize $ els >>= clip mini maxi
    lift $ mapM_ (composeCoverageSpan texture img) spans

composeCoverageSpan :: forall s px .
                      ( Pixel px, Modulable (PixelBaseComponent px) )
                    => Texture px
                    -> MutableImage s px
                    -> CoverageSpan
                    -> ST s ()
{-# INLINE composeCoverageSpan #-}
composeCoverageSpan texture img coverage 
  | cov == 0 || initialX < 0 || y < 0 || imgWidth < initialX || imgHeight < y = return ()
  | otherwise = go 0 initialX initIndex
  where compCount = componentCount (undefined :: px)
        maxi = _coverageLength coverage
        imgData = mutableImageData img
        y = floor $ _coverageY coverage
        initialX = floor $ _coverageX coverage
        imgWidth = mutableImageWidth img
        imgHeight = mutableImageHeight img
        initIndex = (initialX + y * imgWidth) * compCount
        (cov, icov) = clampCoverage $ _coverageVal coverage

        go count _   _ | count >= maxi = return ()
        go count x idx = do
          oldPixel <- unsafeReadPixel imgData idx
          unsafeWritePixel imgData idx
            . compositionAlpha cov icov oldPixel
            $ texture x y
          go (count + 1) (x + 1) $ idx + compCount

