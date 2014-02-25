{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Main module of Rasterific, an Haskell rasterization engine.
module Graphics.Rasterific
    ( 
      -- * Rasterization command
      fill
    , withTexture
    , withClipping
    , stroke
    , dashedStroke
    , printTextAt

    , strokeDebug
    , renderDrawing

      -- * Rasterization types
    , Texture
    , Compositor
    , Drawing
    , Modulable

      -- * Geometry description
    , Point
    , Vector
    , CubicBezier( .. )
    , Line( .. )
    , Bezier( .. )
    , Primitive( .. )

      -- * Helpers
    , line
    , rectangle
    , circle

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

import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import Control.Monad.Free( Free( .. ), liftF )
import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , MutableImage( .. )
                          , createMutableImage
                          , unsafeFreezeImage )

import qualified Data.Vector.Unboxed as VU
import Linear( V2( .. ), (^+^), (^*) )

import Graphics.Rasterific.Compositor
{-import Graphics.Rasterific.Operators-}
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Types
import Graphics.Rasterific.Line
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Stroke

import Graphics.Text.TrueType( Font, PointSize, getStringCurveAtPoint )

{-import Debug.Trace-}
{-import Text.Printf-}

-- | Monad used to describe the drawing context.
type DrawContext s px a =
    StateT (MutableImage s px) (ST s) a

------------------------------------------------
----    Free Monad DSL section
------------------------------------------------
type Drawing px a = Free (DrawCommand px) a

data DrawCommand px next
    = Fill [Primitive] next
    | TextFill Font PointSize Point String next
    | SetTexture (Texture px) 
                 (Drawing px ()) next
    | WithCliping (forall innerPixel. Drawing innerPixel ())
                  (Drawing px ()) next

instance Functor (DrawCommand px) where
    fmap f (TextFill font size pos str next) =
        TextFill font size pos str $ f next
    fmap f (Fill prims next) = Fill prims $ f next
    fmap f (SetTexture t sub next) = SetTexture t sub $ f next
    fmap f (WithCliping sub com next) =
        WithCliping sub com (f next)

-- | Define the texture applyied to all the children
-- draw call.
--
-- > withTexture (uniformTexture $ PixelRGBA8 0 0x86 0xc1 255) $ do
-- >     fill $ circle (V2 50 50) 20
-- >     fill $ circle (V2 100 100) 20
-- >     withTexture (uniformTexture $ PixelRGBA8 0xFF 0x53 0x73 255)
-- >          $ circle (V2 150 150) 20
--
-- <<docimages/with_texture.png>>
--
withTexture :: Texture px -> Drawing px () -> Drawing px ()
withTexture texture subActions =
    liftF $ SetTexture texture subActions ()

-- | Fill some geometry. The geometry should be "looping",
-- ie. the last point of the last primitive should
-- be equal to the first point of the first primitive.
--
-- The primitive should be connected.
--
-- > fill $ circle (V2 100 100) 75 
--
-- <<docimages/fill_circle.png>>
--
fill :: [Primitive] -> Drawing px ()
fill prims = liftF $ Fill prims ()

-- | Draw some geometry using a clipping path.
--
-- > withClipping (fill $ circle (V2 100 100) 75) $
-- >     mapM_ (stroke 7 JoinRound (CapRound, CapRound))
-- >       [line (V2 0 yf) (V2 200 (yf + 10)) 
-- >                      | y <- [5 :: Int, 17 .. 200]
-- >                      , let yf = fromIntegral y ]
--
-- <<docimages/with_clipping.png>>
--
withClipping
    :: (forall innerPixel. Drawing innerPixel ()) -- ^ The clipping path
    -> Drawing px () -- ^ The actual geometry to clip
    -> Drawing px ()
withClipping clipPath drawing =
    liftF $ WithCliping clipPath drawing ()

-- | Will stroke geometry with a given stroke width.
-- The elements should be connected
--
-- > stroke 5 JoinRound (CapRound, CapRound) $ circle (V2 100 100) 75
--
-- <<docimages/stroke_circle.png>>
--
stroke :: Float       -- ^ Stroke width
       -> Join        -- ^ Which kind of join will be used
       -> (Cap, Cap)  -- ^ Start and end capping.
       -> [Primitive] -- ^ List of elements to render
       -> Drawing px ()
stroke width join caping = fill . strokize width join caping

-- | Draw a string at a given position
printTextAt :: Font     -- ^ Drawing font
            -> Int      -- ^ font Point size
            -> Point    -- ^ Baseline begining position
            -> String  -- ^ String to print
            -> Drawing px ()
printTextAt font pointSize point string =
    liftF $ TextFill font pointSize point string ()

-- | Function to call in order to start the image creation.
-- the upper left corner is the point (0, 0)
renderDrawing
    :: forall px
     . ( Pixel px
       , Pixel (PixelBaseComponent px)
       , Modulable (PixelBaseComponent px)
       , PixelBaseComponent (PixelBaseComponent px) ~ (PixelBaseComponent px)
       )
    => Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> px  -- ^ Background color
    -> Drawing px () -- ^ Rendering action
    -> Image px
renderDrawing width height background drawing = runST $
  createMutableImage width height background
        >>= execStateT (go Nothing stupidDefaultTexture drawing)
        >>= unsafeFreezeImage
  where
    clipBackground = emptyValue :: PixelBaseComponent px
    clipForeground = fullValue :: PixelBaseComponent px
    stupidDefaultTexture =
        uniformTexture $ colorMap (const clipBackground) background

    clipRender =
      renderDrawing width height clipBackground 
            . withTexture (uniformTexture clipForeground)
        

    go :: Maybe (Texture (PixelBaseComponent px))
       -> Texture px
       -> Drawing px ()
       -> DrawContext s px ()
    go _ _ (Pure ()) = return ()
    go Nothing texture (Free (Fill prims next)) =
        fillWithTexture texture prims >> go Nothing texture next
    go mo@(Just moduler) texture (Free (Fill prims next)) =
        fillWithTextureAndMask texture moduler prims >> go mo texture next
    go moduler texture (Free (SetTexture tx sub next)) =
        go moduler tx sub >> go moduler texture next
    go moduler texture (Free (TextFill font size (V2 x y) str next)) =
        forM_ drawCalls (go moduler texture) >> go moduler texture next
      where
        drawCalls = beziersOfChar <$> getStringCurveAtPoint 90 (x, y) [(font, size, str)]

        beziersOfChar curves = liftF $ Fill bezierCurves ()
          where
            bezierCurves = concat
              [map BezierPrim . bezierFromPath . map (uncurry V2)
                              $ VU.toList c | c <- curves]

    go moduler texture (Free (WithCliping clipPath path next)) =
        go newModuler texture path >> go moduler texture next
      where
        modulationTexture :: Texture (PixelBaseComponent px)
        modulationTexture = imageTexture $ clipRender clipPath

        newModuler = Just $ subModuler moduler

        subModuler Nothing = modulationTexture
        subModuler (Just v) =
            modulateTexture v modulationTexture

-- | With stroke geometry with a given stroke width, using
-- a dash pattern.
--
-- > dashedStroke [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0)
-- >        [line (V2 0 100) (V2 200 100)]
--
-- <<docimages/dashed_stroke.png>>
--
dashedStroke
    :: DashPattern -- ^ Dashing pattern to use for stroking
    -> Float       -- ^ Stroke width
    -> Join        -- ^ Which kind of join will be used
    -> (Cap, Cap)  -- ^ Start and end capping.
    -> [Primitive] -- ^ List of elements to render
    -> Drawing px ()
dashedStroke dashing width join caping =
    mapM_ fill . dashedStrokize dashing width join caping

-- | Internal debug function
strokeDebug :: ( Pixel px, Modulable (PixelBaseComponent px))
            => Texture px -> Texture px
            -> Float -> Join -> (Cap, Cap)
            -> [Primitive] -> Drawing px ()
strokeDebug debugPair debugImpair width join caping elems = do
  fill stroked
  forM_ (zip debugColor stroked) subStroke
    where stroked = strokize width join caping elems
          -- | Infinite list repeating color pattern
          debugColor = debugPair : debugImpair : debugColor
          subStroke (color, el) =
              withTexture color $ stroke 2 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0) [el]

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
fillWithTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                => Texture px  -- ^ Color/Texture used for the filling
                -> [Primitive] -- ^ Primitives to fill
                -> DrawContext s px ()
fillWithTexture texture els = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize $ els >>= clip mini maxi
    lift $ mapM_ (composeCoverageSpan texture img) spans

fillWithTextureAndMask
    :: ( Pixel px
      , Pixel (PixelBaseComponent px)
      , Modulable (PixelBaseComponent px))
    => Texture px  -- ^ Color/Texture used for the filling
    -> Texture (PixelBaseComponent px)
    -> [Primitive] -- ^ Primitives to fill
    -> DrawContext s px ()
fillWithTextureAndMask texture mask els = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize $ els >>= clip mini maxi
    lift $ mapM_ (composeCoverageSpanWithMask texture mask img) spans

composeCoverageSpan :: forall s px .
                      ( Pixel px, Modulable (PixelBaseComponent px) )
                    => Texture px
                    -> MutableImage s px
                    -> CoverageSpan
                    -> ST s ()
{-# INLINE composeCoverageSpan #-}
composeCoverageSpan texture img coverage 
  | initialCov == 0 || initialX < 0 || y < 0 || imgWidth < initialX || imgHeight < y = return ()
  | otherwise = go 0 initialX initIndex
  where compCount = componentCount (undefined :: px)
        maxi = _coverageLength coverage
        imgData = mutableImageData img
        y = floor $ _coverageY coverage
        initialX = floor $ _coverageX coverage
        imgWidth = mutableImageWidth img
        imgHeight = mutableImageHeight img
        initIndex = (initialX + y * imgWidth) * compCount
        (initialCov, _) =
            clampCoverage $ _coverageVal coverage

        shader = texture SamplerPad

        go count _   _ | count >= maxi = return ()
        go count x idx = do
          oldPixel <- unsafeReadPixel imgData idx
          let px = shader (fromIntegral x) (fromIntegral y)
              opacity = pixelOpacity px
              (cov, icov) = coverageModulate initialCov opacity
          unsafeWritePixel imgData idx
            $ compositionAlpha cov icov oldPixel px
            
          go (count + 1) (x + 1) $ idx + compCount

composeCoverageSpanWithMask
    :: forall s px 
     . ( Pixel px
       , Pixel (PixelBaseComponent px)
       , Modulable (PixelBaseComponent px) )
    => Texture px
    -> Texture (PixelBaseComponent px)
    -> MutableImage s px
    -> CoverageSpan
    -> ST s ()
{-# INLINE composeCoverageSpanWithMask #-}
composeCoverageSpanWithMask texture mask img coverage 
  | initialCov == 0 || initialX < 0 || y < 0 || imgWidth < initialX || imgHeight < y = return ()
  | otherwise = go 0 initialX initIndex
  where compCount = componentCount (undefined :: px)
        maxi = _coverageLength coverage
        imgData = mutableImageData img
        y = floor $ _coverageY coverage
        initialX = floor $ _coverageX coverage
        imgWidth = mutableImageWidth img
        imgHeight = mutableImageHeight img
        initIndex = (initialX + y * imgWidth) * compCount
        (initialCov, _) =
            clampCoverage $ _coverageVal coverage

        maskShader = mask SamplerPad
        shader = texture SamplerPad

        go count _   _ | count >= maxi = return ()
        go count x idx = do
          oldPixel <- unsafeReadPixel imgData idx
          let fx = fromIntegral x
              fy = fromIntegral y
              maskValue = maskShader fx fy
              px = shader fx fy
              (coeffMasked, _) = coverageModulate initialCov maskValue
              (cov, icov) = coverageModulate coeffMasked $ pixelOpacity px
          unsafeWritePixel imgData idx
            $ compositionAlpha cov icov oldPixel px
          go (count + 1) (x + 1) $ idx + compCount


-- | Generate a list of primitive representing a circle.
--
-- > fill $ circle (V2 100 100) 75 
--
-- <<docimages/fill_circle.png>>
--
circle :: Point -- ^ Circle center in pixels
       -> Float -- ^ Circle radius in pixels
       -> [Primitive]
circle center radius = CubicBezierPrim . scaleMove <$> cubicBezierCircle 
  where
    mv p = (p ^* radius) ^+^ center
    scaleMove (CubicBezier p1 p2 p3 p4) =
        CubicBezier (mv p1) (mv p2) (mv p3) (mv p4)

-- | Generate a list of primitive representing a
-- rectangle
--
-- > fill $ rectangle (V2 30 30) 150 100
--
-- <<docimages/fill_rect.png>>
--
rectangle :: Point -- ^ Corner upper left
          -> Float -- ^ Width in pixel
          -> Float -- ^ Height in pixel
          -> [Primitive]
rectangle p@(V2 px py) w h =
  LinePrim <$> lineFromPath 
    [ p, V2 (px + w) py, V2 (px + w) (py + h), V2 px (py + h), p ]

-- | Return a simple line ready to be stroked.
--
-- > stroke 17 JoinRound (CapRound, CapRound) $
-- >     line (V2 10 10) (V2 180 170)
--
-- <<docimages/stroke_line.png>>
--
line :: Point -> Point -> [Primitive]
line p1 p2 = [LinePrim $ Line p1 p2]

