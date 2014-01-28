{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Main module of Rasterific, an Haskell rasterization engine.
module Graphics.Rasterific
    ( 
      -- * Rasterization command
      fill
    , stroke
    , strokeDebug
    , renderContext

      -- * Rasterization types
    , Texture
    , Compositor
    , DrawContext
    , Modulable

      -- * Geometry description
    , Point
    , Vector
    , CubicBezier( .. )
    , Line( .. )
    , Bezier( .. )
    , Primitive( .. )

      -- ** Geometry Helpers
    , clip
    , bezierFromPath
    , lineFromPath
    , cubicBezierFromPath

      -- * Rasterization control
    , Join( .. )
    , Cap( .. )
    , SamplerRepeat( .. )
    , DashPattern

    ) where

{-import Control.Applicative( (<$>) )-}
import Control.Monad( forM_ )
import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , MutableImage( .. )
                          , createMutableImage
                          , unsafeFreezeImage )

import Linear( V2( .. ) )
import Graphics.Rasterific.Compositor
{-import Graphics.Rasterific.Operators-}
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Types
import Graphics.Rasterific.Line
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Stroke

{-import Debug.Trace-}
{-import Text.Printf-}

-- | Monad used to describe the drawing context.
type DrawContext s px a =
    StateT (MutableImage s px) (ST s) a

-- | Function to call in order to start the image creation.
-- the upper left corner is the point (0, 0)
renderContext :: (Pixel px)
              => Int -- ^ Rendering width
              -> Int -- ^ Rendering height
              -> px  -- ^ Background color
              -> (forall s. DrawContext s px a) -- ^ Rendering action
              -> Image px
renderContext width height background drawing = runST $
  createMutableImage width height background
        >>= execStateT drawing
        >>= unsafeFreezeImage

-- | Will stroke geometry with a given stroke width.
-- The elements should be connected
stroke :: ( Pixel px, Modulable (PixelBaseComponent px))
       => Texture px  -- ^ Stroke color/texture
       -> Float       -- ^ Stroke width
       -> Join        -- ^ Which kind of join will be used
       -> (Cap, Cap)  -- ^ Start and end capping.
       -> [Primitive] -- ^ List of elements to render
       -> DrawContext s px ()
stroke texture width join caping =
    fill texture . strokize width join caping

-- | Internal debug function
strokeDebug :: ( Pixel px, Modulable (PixelBaseComponent px))
            => Texture px -> Texture px -> Texture px
            -> Float -> Join -> (Cap, Cap)
            -> [Primitive] -> DrawContext s px ()
strokeDebug debugPair debugImpair texture width join caping elems =
  fill texture stroked >> forM_ (zip debugColor stroked) subStroke
    where stroked = strokize width join caping elems
          -- | Infinite list repeating color pattern
          debugColor = debugPair : debugImpair : debugColor
          subStroke (color, el) =
            stroke color 2 (JoinMiter 0) (CapStraight 0, CapStraight 0) [el]

-- | Clip the geometry to a rectangle.
clip :: Point     -- ^ Minimum point (corner upper left)
     -> Point     -- ^ Maximum point (corner bottom right)
     -> Primitive -- ^ Primitive to be clipped
     -> [Primitive]
clip mini maxi (LinePrim l) = clipLine mini maxi l
clip mini maxi (BezierPrim b) = clipBezier mini maxi b
clip mini maxi (CubicBezierPrim c) = clipCubicBezier mini maxi c

-- | Fill some geometry. The geometry should be "looping",
-- ie. the last point of the last primitive should
-- be equal to the first point of the first primitive.
--
-- The primitive should be connected.
fill :: (Pixel px, Modulable (PixelBaseComponent px))
     => Texture px  -- ^ Color/Texture used for the filling
     -> [Primitive] -- ^ Primitives to fill
     -> DrawContext s px ()
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

