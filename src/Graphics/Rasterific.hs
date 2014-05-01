{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
-- | Main module of Rasterific, an Haskell rasterization engine.
--
-- Creating an image is rather simple, here is a simple example
-- of a drawing and saving it in a PNG file:
--
-- > import Codec.Picture( PixelRGBA8( .. ), writePng )
-- > import Graphics.Rasterific
-- > import Graphics.Rasterific.Texture
-- >
-- > main :: IO ()
-- > main = do
-- >   let white = PixelRGBA8 255 255 255 255
-- >       drawColor = PixelRGBA8 0 0x86 0xc1 255
-- >       recColor = PixelRGBA8 0xFF 0x53 0x73 255
-- >       img = renderDrawing 400 200 white $
-- >          withTexture (uniformTexture drawColor) $ do
-- >             fill $ circle (V2 0 0) 30
-- >             stroke 4 JoinRound (CapRound, CapRound) $
-- >                    circle (V2 400 200) 40
-- >             withTexture (uniformTexture recColor) .
-- >                    fill $ rectangle (V2 100 100) 200 100
-- >
-- >   writePng "yourimage.png" img
--
-- <<docimages/module_example.png>>
--
-- The coordinate system is the picture classic one, with the origin in
-- the upper left corner; with the y axis growing to the bottom and the
-- x axis growing to the right:
--
-- <<docimages/coordinate.png>>
--
module Graphics.Rasterific
    (
      -- * Rasterization command
      fill
    , fillWithMethod
    , withTexture
    , withClipping
    , withTransformation
    , stroke
    , dashedStroke
    , dashedStrokeWithOffset
    , printTextAt

    , renderDrawing
    , pathToPrimitives

      -- * Rasterization types
    , Texture
    , Drawing
    , Modulable

      -- * Geometry description
    , V2( .. )
    , Point
    , Vector
    , CubicBezier( .. )
    , Line( .. )
    , Bezier( .. )
    , Primitive( .. )
    , Path( .. )
    , PathCommand( .. )
    , Transformable( .. )
    , PointFoldable( .. )

      -- * Helpers
    , line
    , rectangle
    , roundedRectangle
    , circle
    , ellipse
    , polyline
    , polygon
    , drawImageAtSize
    , drawImage

      -- ** Geometry Helpers
    , clip
    , bezierFromPath
    , lineFromPath
    , cubicBezierFromPath

      -- * Rasterization control
    , Join( .. )
    , Cap( .. )
    , SamplerRepeat( .. )
    , FillMethod( .. )
    , DashPattern

      -- * Debugging helper
    , dumpDrawing

    ) where

import qualified Data.Foldable as F
import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import Control.Monad.Free( Free( .. ), liftF )
import Control.Monad.Free.Church( F, fromF )
import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Data.Monoid( Monoid( .. ), (<>) )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , Pixel8
                          , PixelRGBA8
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
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.TensorPatch

import Graphics.Text.TrueType( Font, PointSize, getStringCurveAtPoint )

{-import Debug.Trace-}
{-import Text.Printf-}

-- | Monad used to describe the drawing context.
type DrawContext s px a =
    StateT (MutableImage s px) (ST s) a

------------------------------------------------
----    Free Monad DSL section
------------------------------------------------

-- | Monad used to record the drawing actions.
type Drawing px = F (DrawCommand px)

data DrawCommand px next
    = Fill FillMethod [Primitive] next
    | Stroke Float Join (Cap, Cap) [Primitive] next
    | DashedStroke Float DashPattern Float Join (Cap, Cap) [Primitive] next
    | TextFill Font PointSize Point String next
    | SetTexture (Texture px)
                 (Drawing px ()) next
    | WithCliping (forall innerPixel. Drawing innerPixel ())
                  (Drawing px ()) next
    | WithTransform Transformation (Drawing px ()) next

-- | This function will spit out drawing instructions to
-- help debugging.
--
-- The outputted code looks like Haskell, but there is no
-- guarantee that it is compilable.
dumpDrawing :: (Show px) => Drawing px () -> String
dumpDrawing = go . fromF where
  go :: Show px => Free (DrawCommand px) () -> String
  go (Pure ()) = "return ()"
  go (Free (Fill _ prims next)) =
    "fill " ++ show prims ++ " >>=\n" ++   go next
  go (Free (TextFill _ _ _ text next)) =
    "-- Text : " ++ text ++ "\n" ++   go next
  go (Free (SetTexture _tx drawing next)) =
    "withTexture ({- texture -}) (" ++
              go (fromF drawing) ++ ") >>=\n" ++ go next
  go (Free (DashedStroke o pat w j cap prims next)) =
    "dashedStrokeWithOffset "
              ++ show o ++ " "
              ++ show pat ++ " "
              ++ show w ++ " ("
              ++ show j ++ ") "
              ++ show cap ++ " "
              ++ show prims ++ " >>=\n" ++   go next
  go (Free (Stroke w j cap prims next)) =
    "stroke " ++ show w ++ " ("
              ++ show j ++ ") "
              ++ show cap ++ " "
              ++ show prims ++ " >>=\n" ++   go next
  go (Free (WithTransform trans sub next)) =
    "withTransform (" ++ show trans ++ ") (" 
                      ++ go (fromF sub) ++ ") >>=\n "
                      ++ go next
  go (Free (WithCliping clipping draw next)) =
    "withClipping (" ++   go (fromF $ withTexture clipTexture clipping)
                     ++ ")\n" ++
        "         (" ++ go (fromF draw) ++ ")\n >>= " ++
              go next
        where clipTexture = uniformTexture (0xFF :: Pixel8)


instance Functor (DrawCommand px) where
    fmap f (TextFill font size pos str next) =
        TextFill font size pos str $ f next
    fmap f (Fill method  prims next) = Fill method prims $ f next
    fmap f (SetTexture t sub next) = SetTexture t sub $ f next
    fmap f (WithCliping sub com next) =
        WithCliping sub com $ f next
    fmap f (Stroke w j caps prims next) =
        Stroke w j caps prims $ f next
    fmap f (DashedStroke st pat w j caps prims next) =
        DashedStroke st pat w j caps prims $ f next
    fmap f (WithTransform trans draw next) =
        WithTransform trans draw $ f next

instance Monoid (Drawing px ()) where
    mempty = return ()
    mappend a b = a >> b

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

-- | Draw all the sub drawing commands using a transformation.
withTransformation :: Transformation -> Drawing px () -> Drawing px ()
withTransformation trans sub =
    liftF $ WithTransform trans sub ()

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
fill prims = liftF $ Fill FillWinding prims ()

-- | This function let you choose how to fill the primitives
-- in case of self intersection. See `FillMethod` documentation
-- for more information.
fillWithMethod :: FillMethod -> [Primitive] -> Drawing px ()
fillWithMethod method prims =
    liftF $ Fill method prims ()

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
stroke width join caping prims =
    liftF $ Stroke width join caping prims ()

-- | Draw a string at a given position.
-- Text printing imply loading a font, there is no default
-- font (yet). Below an example of font rendering using a
-- font installed on Microsoft Windows.
--
-- > import Graphics.Text.TrueType( loadFontFile )
-- > import Codec.Picture( PixelRGBA8( .. ), writePng )
-- > import Graphics.Rasterific
-- > import Graphics.Rasterific.Texture
-- >
-- > main :: IO ()
-- > main = do
-- >   fontErr <- loadFontFile "C:/Windows/Fonts/arial.ttf"
-- >   case fontErr of
-- >     Left err -> putStrLn err
-- >     Right font ->
-- >       writePng "text_example.png" .
-- >           renderDrawing 300 70 (PixelRGBA8 255 255 255 255)
-- >               . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
-- >                       printTextAt font 12 (V2 20 40) "A simple text test!"
--
-- <<docimages/text_example.png>>
--
-- You can use any texture, like a gradient while rendering text.
--
printTextAt :: Font     -- ^ Drawing font
            -> Int      -- ^ font Point size
            -> Point    -- ^ Baseline begining position
            -> String  -- ^ String to print
            -> Drawing px ()
printTextAt font pointSize point string =
    liftF $ TextFill font pointSize point string ()

data RenderContext px = RenderContext
    { currentClip           :: Maybe (Texture (PixelBaseComponent px))
    , currentTexture        :: Texture px
    , currentTransformation :: Maybe (Transformation, Transformation)
    }

-- | Function to call in order to start the image creation.
-- Tested pixels type are PixelRGBA8 and Pixel8, pixel types
-- in other colorspace will probably produce weird results.
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
        >>= execStateT (go initialContext $ fromF drawing)
        >>= unsafeFreezeImage
  where
    initialContext = RenderContext Nothing stupidDefaultTexture Nothing
    clipBackground = emptyValue :: PixelBaseComponent px
    clipForeground = fullValue :: PixelBaseComponent px
    stupidDefaultTexture =
        uniformTexture $ colorMap (const clipBackground) background

    clipRender =
      renderDrawing width height clipBackground
            . withTexture (uniformTexture clipForeground)

    textureOf ctxt@RenderContext { currentTransformation = Just (_, t) } =
        transformTexture t $ currentTexture ctxt
    textureOf ctxt = currentTexture ctxt

    geometryOf RenderContext { currentTransformation = Just (trans, _) } =
        transform (applyTransformation trans)
    geometryOf _ = id

    go :: RenderContext px
       -> Free (DrawCommand px) ()
       -> DrawContext s px ()
    go _ (Pure ()) = return ()
    go ctxt (Free (WithTransform trans sub next)) = do
        let trans'
              | Just (t, _) <- currentTransformation ctxt = t <> trans
              | otherwise = trans
        go ctxt { currentTransformation =
                        Just (trans', inverseTransformation trans') } $ fromF sub
        go ctxt next
    go ctxt@RenderContext { currentClip = Nothing }
       (Free (Fill method prims next)) = do
        fillWithTexture method (textureOf ctxt) $ geometryOf ctxt prims
        go ctxt next
    go ctxt@RenderContext { currentClip = Just moduler }
       (Free (Fill method prims next)) = do
        fillWithTextureAndMask method (currentTexture ctxt)
            moduler $ geometryOf ctxt prims
        go ctxt next

    go ctxt (Free (Stroke w j cap prims next)) =
        go ctxt . Free $ Fill FillWinding prim' next
            where prim' = listOfContainer $ strokize w j cap prims
    go ctxt (Free (SetTexture tx sub next)) = do
        go (ctxt { currentTexture = tx }) $ fromF sub
        go ctxt next
    go ctxt (Free (DashedStroke o d w j cap prims next)) = do
        let recurse sub =
                go ctxt . liftF $ Fill FillWinding sub ()
        mapM_ recurse $ dashedStrokize o d w j cap prims
        go ctxt next

    go ctxt (Free (TextFill font size (V2 x y) str next)) = do
        forM_ drawCalls (go ctxt)
        go ctxt next
      where
        drawCalls =
            beziersOfChar <$> getStringCurveAtPoint 90 (x, y)
                                    [(font, size, str)]

        beziersOfChar curves = liftF $ Fill FillWinding bezierCurves ()
          where
            bezierCurves = concat
              [map BezierPrim . bezierFromPath . map (uncurry V2)
                              $ VU.toList c | c <- curves]

    go ctxt (Free (WithCliping clipPath path next)) = do
        go (ctxt { currentClip = newModuler }) $ fromF path
        go ctxt next
      where
        modulationTexture :: Texture (PixelBaseComponent px)
        modulationTexture = imageTexture $ clipRender clipPath

        newModuler = Just . subModuler $ currentClip ctxt

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
dashedStroke = dashedStrokeWithOffset 0.0

-- | With stroke geometry with a given stroke width, using
-- a dash pattern. The offset is there to specify the starting
-- point into the pattern, the value can be negative.
--
-- > dashedStrokeWithOffset 3 [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0)
-- >        [line (V2 0 100) (V2 200 100)]
--
-- <<docimages/dashed_stroke_with_offset.png>>
--
dashedStrokeWithOffset
    :: Float       -- ^ Starting offset
    -> DashPattern -- ^ Dashing pattern to use for stroking
    -> Float       -- ^ Stroke width
    -> Join        -- ^ Which kind of join will be used
    -> (Cap, Cap)  -- ^ Start and end capping.
    -> [Primitive] -- ^ List of elements to render
    -> Drawing px ()
dashedStrokeWithOffset _ [] width join caping prims =
    stroke width join caping prims
dashedStrokeWithOffset offset dashing width join caping prims =
    liftF $ DashedStroke offset dashing width join caping prims ()

-- | Clip the geometry to a rectangle.
clip :: Point     -- ^ Minimum point (corner upper left)
     -> Point     -- ^ Maximum point (corner bottom right)
     -> Primitive -- ^ Primitive to be clipped
     -> Container Primitive
clip mini maxi (LinePrim l) = clipLine mini maxi l
clip mini maxi (BezierPrim b) = clipBezier mini maxi b
clip mini maxi (CubicBezierPrim c) = clipCubicBezier mini maxi c

-- | Fill some geometry. The geometry should be "looping",
-- ie. the last point of the last primitive should
-- be equal to the first point of the first primitive.
--
-- The primitive should be connected.
fillWithTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                => FillMethod
                -> Texture px  -- ^ Color/Texture used for the filling
                -> [Primitive] -- ^ Primitives to fill
                -> DrawContext s px ()
fillWithTexture fillMethod texture els = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize fillMethod . listOfContainer $ F.foldMap (clip mini maxi) els 
    lift $ F.mapM_ (composeCoverageSpan texture img) spans

fillWithTextureAndMask
    :: ( Pixel px
       , Pixel (PixelBaseComponent px)
       , Modulable (PixelBaseComponent px))
    => FillMethod
    -> Texture px  -- ^ Color/Texture used for the filling
    -> Texture (PixelBaseComponent px)
    -> [Primitive] -- ^ Primitives to fill
    -> DrawContext s px ()
fillWithTextureAndMask fillMethod texture mask els = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans = rasterize fillMethod . listOfContainer $ F.foldMap (clip mini maxi) els
    lift $ mapM_ (composeCoverageSpanWithMask texture mask img) spans

composeCoverageSpan :: forall s px .
                      ( Pixel px, Modulable (PixelBaseComponent px) )
                    => Texture px
                    -> MutableImage s px
                    -> CoverageSpan
                    -> ST s ()
{-# SPECIALIZE
    composeCoverageSpan
        :: forall s. 
           Texture PixelRGBA8
        -> MutableImage s PixelRGBA8
        -> CoverageSpan -> ST s () #-}
{-# SPECIALIZE
    composeCoverageSpan
        :: forall s. 
           Texture Pixel8
        -> MutableImage s Pixel8
        -> CoverageSpan -> ST s () #-}
composeCoverageSpan texture img coverage
  | initialCov == 0 || initialX < 0 || y < 0 || imgWidth < initialX || imgHeight < y = return ()
  | otherwise = go 0 initialX initIndex
  where !compCount = componentCount (undefined :: px)
        !maxi = _coverageLength coverage
        !imgData = mutableImageData img
        !y = floor $ _coverageY coverage
        !initialX = floor $ _coverageX coverage
        !imgWidth = mutableImageWidth img
        !imgHeight = mutableImageHeight img
        !initIndex = (initialX + y * imgWidth) * compCount
        !(initialCov, _) =
            clampCoverage $ _coverageVal coverage

        !shader = texture SamplerPad

        go count _   _ | count >= maxi = return ()
        go !count !x !idx = do
          oldPixel <- unsafeReadPixel imgData idx
          let px = shader (fromIntegral x) (fromIntegral y)
              !opacity = pixelOpacity px
              !(cov, icov) = coverageModulate initialCov opacity
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
circle center radius =
    CubicBezierPrim . transform mv <$> cubicBezierCircle
  where
    mv p = (p ^* radius) ^+^ center

-- | Generate a list of primitive representing an ellipse.
--
-- > fill $ ellipse (V2 100 100) 75 30
--
-- <<docimages/fill_ellipse.png>>
--
ellipse :: Point -> Float -> Float -> [Primitive]
ellipse center rx ry =
    CubicBezierPrim . transform mv <$> cubicBezierCircle
  where
    mv (V2 x y) = V2 (x * rx) (y * ry) ^+^ center

-- | Generate a strokable line out of points list.
-- Just an helper around `lineFromPath`.
--
-- > stroke 4 JoinRound (CapRound, CapRound) $
-- >    polyline [V2 10 10, V2 100 70, V2 190 190]
--
-- <<docimages/stroke_polyline.png>>
--
polyline :: [Point] -> [Primitive]
polyline = map LinePrim . lineFromPath

-- | Generate a fillable polygon out of points list.
-- Similar to the `polyline` function, but close the
-- path.
--
-- > fill $ polygon [V2 30 30, V2 100 70, V2 80 170]
--
-- <<docimages/fill_polygon.png>>
--
polygon :: [Point] -> [Primitive]
polygon [] = []
polygon [_] = []
polygon [_,_] = []
polygon lst@(p:_) = polyline $ lst ++ [p]

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

-- | Simply draw an image into the canvas. Take into account
-- any previous transformation performed on the geometry.
--
-- > drawImage textureImage 0 (V2 30 30)
--
-- <<docimages/image_simple.png>>
--
drawImage :: (Pixel px, Modulable (PixelBaseComponent px))
          => Image px       -- ^ Image to be drawn
          -> StrokeWidth    -- ^ Border size, drawn with current texture.
          -> Point          -- ^ Position of the corner upper left of the image.
          -> Drawing px ()
drawImage img@Image { imageWidth = w, imageHeight = h } s p =
    drawImageAtSize img s p (fromIntegral w) (fromIntegral h)

-- | Draw an image with the desired size
--
-- > drawImageAtSize textureImage 2 (V2 30 30) 128 128
--
-- <<docimages/image_resize.png>>
--
drawImageAtSize :: (Pixel px, Modulable (PixelBaseComponent px))
                => Image px    -- ^ Image to be drawn
                -> StrokeWidth -- ^ Border size, drawn with current texture.
                -> Point -- ^ Position of the corner upper left of the image.
                -> Float -- ^ Width of the drawn image
                -> Float -- ^ Height of the drawn image
                -> Drawing px ()
drawImageAtSize img@Image { imageWidth = w, imageHeight = h } borderSize p
            reqWidth reqHeight
    | borderSize <= 0 =
        withTransformation (translate p <> scale scaleX scaleY) .
            withTexture (sampledImageTexture img) $ fill rect
    | otherwise = do
        withTransformation (translate p <> scale scaleX scaleY) $ do
            withTexture (sampledImageTexture img) $ fill rect
        stroke borderSize (JoinMiter 0)
               (CapStraight 0, CapStraight 0) rect'
        where
          rect = rectangle (V2 0 0) rw rh
          rect' = rectangle p reqWidth reqHeight

          (rw, rh) = (fromIntegral w, fromIntegral h)
          scaleX | reqWidth == 0 = 1
                 | otherwise = reqWidth / rw

          scaleY | reqHeight == 0 = 1
                 | otherwise = reqHeight / rh

-- | Generate a list of primitive representing a rectangle
-- with rounded corner.
--
-- > fill $ roundedRectangle (V2 10 10) 150 150 20 10
--
-- <<docimages/fill_roundedRectangle.png>>
--
roundedRectangle :: Point -- ^ Corner upper left
                 -> Float -- ^ Width in pixel
                 -> Float -- ^ Height in pixel.
                 -> Float -- ^ Radius along the x axis of the rounded corner. In pixel.
                 -> Float -- ^ Radius along the y axis of the rounded corner. In pixel.
                 -> [Primitive]
roundedRectangle (V2 px py) w h rx ry =
    [ CubicBezierPrim . transform (^+^ V2 xFar yNear) $ cornerTopR
    , LinePrim $ Line (V2 xFar py) (V2 xNear py)
    , CubicBezierPrim . transform (^+^ V2 (px + rx) (py + ry)) $ cornerTopL
    , LinePrim $ Line (V2 px yNear) (V2 px yFar)
    , CubicBezierPrim . transform (^+^ V2 (px + rx) yFar) $ cornerBottomL
    , LinePrim $ Line (V2 xNear (py + h)) (V2 xFar (py + h))
    , CubicBezierPrim . transform (^+^ V2 xFar yFar) $ cornerBottomR
    , LinePrim $ Line (V2 (px + w) yFar) (V2 (px + w) yNear)
    ]
  where
   xNear = px + rx
   xFar = px + w - rx

   yNear = py + ry
   yFar = py + h - ry

   (cornerBottomR :
    cornerTopR     :
    cornerTopL  :
    cornerBottomL:_) = transform (\(V2 x y) -> V2 (x * rx) (y * ry)) <$> cubicBezierCircle

-- | Return a simple line ready to be stroked.
--
-- > stroke 17 JoinRound (CapRound, CapRound) $
-- >     line (V2 10 10) (V2 180 170)
--
-- <<docimages/stroke_line.png>>
--
line :: Point -> Point -> [Primitive]
line p1 p2 = [LinePrim $ Line p1 p2]

