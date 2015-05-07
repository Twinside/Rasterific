{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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
      -- ** Filling
      fill
    , fillWithMethod
      -- ** Stroking
    , stroke
    , dashedStroke
    , dashedStrokeWithOffset
      -- ** Text rendering
    , printTextAt
    , printTextRanges
      -- ** Texturing
    , withTexture
    , withClipping
    , withGroupOpacity
      -- ** Transformations
    , withTransformation
    , withPathOrientation
    , TextRange( .. )
    , PointSize( .. )

      -- * Generating images
    , ModulablePixel
    , RenderablePixel
    , renderDrawing
    , renderDrawingAtDpi
    , renderDrawingAtDpiToPDF
    , renderOrdersAtDpiToPdf 
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

      -- * Generic geometry description
    , Primitivable( .. )
    , Geometry( .. )

      -- * Generic geometry manipulation
    , Transformable( .. )
    , PointFoldable( .. )
    , PlaneBoundable( .. )
    , PlaneBound( .. )
    , boundWidth
    , boundHeight
    , boundLowerLeftCorner

      -- * Helpers
      -- ** line
    , line
      -- ** Rectangle
    , rectangle
    , roundedRectangle
      -- ** Circles
    , circle
    , ellipse
      -- ** Polygons
    , polyline
    , polygon
      -- ** Images
    , drawImageAtSize
    , drawImage
    , cacheDrawing

      -- ** Geometry Helpers
    , clip
    , bezierFromPath
    , lineFromPath
    , cubicBezierFromPath
    , firstTangeantOf
    , lastTangeantOf
    , firstPointOf
    , lastPointOf

      -- * Rasterization control
    , Join( .. )
    , Cap( .. )
    , SamplerRepeat( .. )
    , FillMethod( .. )
    , DashPattern
    , drawOrdersOfDrawing

      -- * Debugging helper
    , dumpDrawing
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Foldable( foldMap )
import Data.Monoid( Monoid( .. ) )
#endif

import Data.Monoid( (<>) )

import Control.Monad.Free( Free( .. ), liftF )
import Control.Monad.Free.Church( fromF )
import Control.Monad.ST( runST )
import Control.Monad.State( modify, execState )
import Data.Maybe( fromMaybe )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , PixelRGBA8( PixelRGBA8 )
                          , unpackPixel
                          , pixelMapXY )

import qualified Data.ByteString.Lazy as LB
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Linear( V2( .. ), (^+^), (^-^) )
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.MicroPdf
{-import Graphics.Rasterific.Texture-}
import Graphics.Rasterific.ComplexPrimitive
import Graphics.Rasterific.Types
import Graphics.Rasterific.Line
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.StrokeInternal
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PlaneBoundable
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.PathWalker
import Graphics.Rasterific.Command
{-import Graphics.Rasterific.TensorPatch-}

import Graphics.Text.TrueType( Font
                             , Dpi
                             , PointSize( .. )
                             )

{-import Debug.Trace-}
{-import Text.Printf-}

------------------------------------------------
----    Free Monad DSL section
------------------------------------------------

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

-- | Will render the whole subaction with a given group opacity, after
-- each element has been rendered. That means that completly opaque
-- overlapping shapes will be rendered transparently, not one after
-- another.
--
-- > withTexture (uniformTexture $ PixelRGBA8 0xFF 0x53 0x73 255) $
-- >     stroke 3 JoinRound (CapRound, CapRound) $
-- >         line (V2 0 100) (V2 200 100)
-- >
-- > withGroupOpacity 128 $ do
-- >    withTexture (uniformTexture $ PixelRGBA8 0 0x86 0xc1 255) .
-- >       fill $ circle (V2 70 100) 60
-- >    withTexture (uniformTexture $ PixelRGBA8 0xff 0xf4 0xc1 255) .
-- >       fill $ circle (V2 120 100) 60
--
-- <<docimages/group_opacity.png>>
--
-- To be compared to the item opacity
--
-- > withTexture (uniformTexture $ PixelRGBA8 0xFF 0x53 0x73 255) $
-- >     stroke 3 JoinRound (CapRound, CapRound) $
-- >         line (V2 0 100) (V2 200 100)
-- > withTexture (uniformTexture $ PixelRGBA8 0 0x86 0xc1 128) .
-- >    fill $ circle (V2 70 100) 60
-- > withTexture (uniformTexture $ PixelRGBA8 0xff 0xf4 0xc1 128) .
-- >    fill $ circle (V2 120 100) 60
--
-- <<docimages/item_opacity.png>>
withGroupOpacity :: Pixel px => PixelBaseComponent px -> Drawing px ()-> Drawing px ()
withGroupOpacity opa sub =
    liftF $ WithGlobalOpacity opa sub ()

-- | Draw all the sub drawing commands using a transformation.
withTransformation :: Transformation -> Drawing px () -> Drawing px ()
withTransformation trans sub =
    liftF $ WithTransform trans sub ()

-- | This command allows you to draw primitives on a given curve,
-- for example, you can draw text on a curve:
--
-- > let path = Path (V2 100 180) False
-- >                 [PathCubicBezierCurveTo (V2 20 20) (V2 170 20) (V2 300 200)] in
-- > stroke 3 JoinRound (CapStraight 0, CapStraight 0) path
-- > withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
-- >   withPathOrientation path 0 $
-- >     printTextAt font (PointSize 24) (V2 0 0) "Text on path"
--
-- <<docimages/text_on_path.png>>
--
-- You can note that the position of the baseline match the size of the
-- characters.
--
-- You are not limited to text drawing while using this function,
-- you can draw arbitrary geometry like in the following example:
--
-- > let path = Path (V2 100 180) False
-- >                 [PathCubicBezierCurveTo (V2 20 20) (V2 170 20) (V2 300 200)]
-- > withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
-- >   stroke 3 JoinRound (CapStraight 0, CapStraight 0) path
-- > 
-- > withPathOrientation path 0 $ do
-- >   printTextAt font (PointSize 24) (V2 0 0) "TX"
-- >   fill $ rectangle (V2 (-10) (-10)) 30 20
-- >   fill $ rectangle (V2 45 0) 10 20
-- >   fill $ rectangle (V2 60 (-10)) 20 20
-- >   fill $ rectangle (V2 100 (-15)) 20 50
--
-- <<docimages/geometry_on_path.png>>
--
withPathOrientation :: Path            -- ^ Path directing the orientation.
                    -> Float           -- ^ Basline Y axis position, used to align text properly.
                    -> Drawing px ()   -- ^ The sub drawings.
                    -> Drawing px ()
withPathOrientation path p sub =
    liftF $ WithPathOrientation path p sub ()

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
fill :: Geometry geom => geom -> Drawing px ()
fill prims = liftF $ Fill FillWinding (toPrimitives prims) ()

-- | This function let you choose how to fill the primitives
-- in case of self intersection. See `FillMethod` documentation
-- for more information.
fillWithMethod :: Geometry geom
               => FillMethod -> geom -> Drawing px ()
fillWithMethod method prims =
    liftF $ Fill method (toPrimitives prims) ()

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
stroke :: (Geometry geom)
       => Float       -- ^ Stroke width
       -> Join        -- ^ Which kind of join will be used
       -> (Cap, Cap)  -- ^ Start and end capping.
       -> geom        -- ^ List of elements to render
       -> Drawing px ()
stroke width join caping prims =
    liftF $ Stroke width join caping (toPrimitives prims) ()

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
-- >   fontErr <- loadFontFile "test_fonts/DejaVuSans.ttf"
-- >   case fontErr of
-- >     Left err -> putStrLn err
-- >     Right font ->
-- >       writePng "text_example.png" .
-- >           renderDrawing 300 70 (PixelRGBA8 255 255 255 255)
-- >               . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
-- >                       printTextAt font (PointSize 12) (V2 20 40)
-- >                            "A simple text test!"
--
-- <<docimages/text_example.png>>
--
-- You can use any texture, like a gradient while rendering text.
--
printTextAt :: Font            -- ^ Drawing font
            -> PointSize       -- ^ font Point size
            -> Point           -- ^ Drawing starting point (base line)
            -> String          -- ^ String to print
            -> Drawing px ()
printTextAt font pointSize point string =
    liftF $ TextFill point [description] ()
  where
    description = TextRange
        { _textFont    = font
        , _textSize    = pointSize
        , _text        = string
        , _textTexture = Nothing
        }

-- | Print complex text, using different texture font and
-- point size for different parts of the text.
--
-- > let blackTexture =
-- >       Just . uniformTexture $ PixelRGBA8 0 0 0 255
-- >     redTexture =
-- >       Just . uniformTexture $ PixelRGBA8 255 0 0 255
-- > in
-- > printTextRanges (V2 20 40)
-- >   [ TextRange font1 (PointSize 12) "A complex " blackTexture
-- >   , TextRange font2 (PointSize 8) "text test" redTexture]
--
-- <<docimages/text_complex_example.png>>
--
printTextRanges :: Point            -- ^ Starting point of the base line
                -> [TextRange px]   -- ^ Ranges description to be printed
                -> Drawing px ()
printTextRanges point ranges = liftF $ TextFill point ranges ()

data RenderContext px = RenderContext
    { currentClip           :: Maybe (Texture (PixelBaseComponent px))
    , currentTexture        :: Texture px
    , currentTransformation :: Maybe (Transformation, Transformation)
    }

-- | Function to call in order to start the image creation.
-- Tested pixels type are PixelRGBA8 and Pixel8, pixel types
-- in other colorspace will probably produce weird results.
-- Default DPI is 96
renderDrawing
    :: forall px . (RenderablePixel px)
    => Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> px  -- ^ Background color
    -> Drawing px () -- ^ Rendering action
    -> Image px
renderDrawing width height = renderDrawingAtDpi width height 96

renderOrdersAtDpiToPdf 
    :: Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> Dpi -- ^ Current DPI used for text rendering.
    -> [DrawOrder PixelRGBA8]  -- ^ Drawing Orders
    -> LB.ByteString
renderOrdersAtDpiToPdf w h dpi =
  renderOrdersToPdf renderer w h dpi
    where
      renderer = drawOrdersOfDrawing w h dpi (PixelRGBA8 0 0 0 0)

renderDrawingAtDpiToPDF
    :: Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> Dpi -- ^ Current DPI used for text rendering.
    -> Drawing PixelRGBA8 () -- ^ Rendering action
    -> LB.ByteString
renderDrawingAtDpiToPDF w h dpi =
  renderDrawingToPdf renderer w h dpi
    where
      renderer = drawOrdersOfDrawing w h dpi (PixelRGBA8 0 0 0 0)

-- | Function to call in order to start the image creation.
-- Tested pixels type are PixelRGBA8 and Pixel8, pixel types
-- in other colorspace will probably produce weird results.
renderDrawingAtDpi
    :: forall px . (RenderablePixel px)
    => Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> Dpi -- ^ Current DPI used for text rendering.
    -> px  -- ^ Background color
    -> Drawing px () -- ^ Rendering action
    -> Image px
renderDrawingAtDpi width height dpi background drawing =
    runST $ runDrawContext width height background
          $ mapM_ fillOrder
          $ drawOrdersOfDrawing width height dpi background drawing

emptyPx :: (RenderablePixel px) => px
-- | Really need a "builder" function for pixel
emptyPx = colorMap (const emptyValue) $ unpackPixel 0

cacheOrders :: forall px. (RenderablePixel px)
            => Maybe (Image px -> ImageTransformer px) -> [DrawOrder px] -> Drawing px ()
cacheOrders imageFilter orders = case imageFilter of
    Nothing -> drawImage resultImage 0 cornerUpperLeft
    Just f -> drawImage (pixelMapXY (f resultImage) resultImage) 0 cornerUpperLeft
  where
   PlaneBound mini maxi = foldMap planeBounds orders
   cornerUpperLeftInt = floor <$> mini :: V2 Int
   cornerUpperLeft = fromIntegral <$> cornerUpperLeftInt

   V2 width height = maxi ^-^ cornerUpperLeft ^+^ V2 1 1
   
   shiftOrder order@DrawOrder { _orderPrimitives = prims } =
       order { _orderPrimitives = fmap (transform (^-^ cornerUpperLeft)) <$> prims 
             , _orderTexture =
                 WithTextureTransform (translate cornerUpperLeft) $ _orderTexture order
             }
   
   resultImage =
     runST $ runDrawContext (ceiling width) (ceiling height) emptyPx
           $ mapM_ (fillOrder . shiftOrder) orders

-- | This function perform an optimisation, it will render a drawing
-- to an image interanlly and create a new order to render this image
-- instead of the geometry, effectively cuting the geometry generation
-- part.
--
-- It can save execution time when drawing complex elements multiple
-- times.
cacheDrawing
    :: forall px . (RenderablePixel px)
    => Int -- ^ Max rendering width
    -> Int -- ^ Max rendering height
    -> Dpi
    -> Drawing px ()
    -> Drawing px ()
cacheDrawing maxWidth maxHeight dpi sub =
  cacheOrders Nothing $ drawOrdersOfDrawing maxWidth maxHeight dpi emptyPx sub

-- | Transform a drawing into a serie of low-level drawing orders.
drawOrdersOfDrawing
    :: forall px . (RenderablePixel px) 
    => Int -- ^ Rendering width
    -> Int -- ^ Rendering height
    -> Dpi -- ^ Current assumed DPI
    -> px  -- ^ Background color
    -> Drawing px () -- ^ Rendering action
    -> [DrawOrder px]
drawOrdersOfDrawing width height dpi background drawing =
    go initialContext (fromF drawing) []
  where
    initialContext = RenderContext Nothing stupidDefaultTexture Nothing
    clipBackground = emptyValue :: PixelBaseComponent px
    clipForeground = fullValue :: PixelBaseComponent px

    clipRender =
      renderDrawing width height clipBackground
            . withTexture (SolidTexture clipForeground)

    textureOf ctxt@RenderContext { currentTransformation = Just (_, t) } =
        WithTextureTransform t $ currentTexture ctxt
    textureOf ctxt = currentTexture ctxt

    geometryOf :: Transformable a => RenderContext px -> a -> a
    geometryOf RenderContext { currentTransformation = Just (trans, _) } =
        transform (applyTransformation trans)
    geometryOf _ = id

    geometryOfO RenderContext { currentTransformation = Just (trans, _) } =
        transformOrder (applyTransformation trans)
    geometryOfO _ = id

    stupidDefaultTexture =
        SolidTexture $ colorMap (const clipBackground) background

    go :: RenderContext px -> Free (DrawCommand px) () -> [DrawOrder px]
       -> [DrawOrder px]
    go _ (Pure ()) rest = rest
    go ctxt (Free (WithGlobalOpacity opa sub next)) rest =
        go ctxt (Free (WithImageEffect opacifier sub next)) rest
      where 
        -- Todo: a colorMapWithAlpha is really needed in JP API.
        opacifier _ _ _ px = mixWithAlpha ignore alphaModulate px px
        ignore _ _ a = a
        alphaModulate _ v = opa `modulate` v

    go ctxt (Free (WithImageEffect effect sub next)) rest =
        go freeContext (fromF cached) after
      where
        cached = cacheOrders (Just effect) $ go ctxt (fromF sub) []
        after = go ctxt next rest
        freeContext = ctxt { currentClip = Nothing, currentTransformation = Nothing }


    go ctxt (Free (WithPathOrientation path base sub next)) rest = final where
      final = orders <> go ctxt next rest
      images = go ctxt (fromF sub) []

      drawer trans _ order =
        modify (transformOrder (applyTransformation trans) order :)

      orders = reverse $ execState (drawOrdersOnPath drawer 0 base path images) []

    go ctxt (Free (WithTransform trans sub next)) rest = final where
      trans'
        | Just (t, _) <- currentTransformation ctxt = t <> trans
        | otherwise = trans
      invTrans = fromMaybe mempty $ inverseTransformation trans'
      after = go ctxt next rest
      subContext =
          ctxt { currentTransformation = Just (trans', invTrans) }

      final = go subContext (fromF sub) after

    go ctxt (Free (Fill method prims next)) rest = order : after where
      after = go ctxt next rest
      order = DrawOrder 
            { _orderPrimitives = [geometryOf ctxt prims]
            , _orderTexture    = textureOf ctxt
            , _orderFillMethod = method
            , _orderMask       = currentClip ctxt
            }

    go ctxt (Free (Stroke w j cap prims next)) rest =
        go ctxt (Free $ Fill FillWinding prim' next) rest
            where prim' = listOfContainer $ strokize w j cap prims

    go ctxt (Free (SetTexture tx sub next)) rest =
        go (ctxt { currentTexture = tx }) (fromF sub) $ go ctxt next rest

    go ctxt (Free (DashedStroke o d w j cap prims next)) rest =
        foldr recurse after $ dashedStrokize o d w j cap prims
      where
        after = go ctxt next rest
        recurse sub =
            go ctxt (liftF $ Fill FillWinding sub ())

    go ctxt (Free (TextFill p descriptions next)) rest = calls <> go ctxt next rest where
      calls =
        geometryOfO ctxt <$> textToDrawOrders dpi (currentTexture ctxt) p descriptions

    go ctxt (Free (WithCliping clipPath path next)) rest =
        go (ctxt { currentClip = newModuler }) (fromF path) $
            go ctxt next rest
      where
        modulationTexture :: Texture (PixelBaseComponent px)
        modulationTexture = RawTexture $ clipRender clipPath

        newModuler = Just . subModuler $ currentClip ctxt

        subModuler Nothing = modulationTexture
        subModuler (Just v) =
            ModulateTexture v modulationTexture

-- | With stroke geometry with a given stroke width, using
-- a dash pattern.
--
-- > dashedStroke [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
-- >     line (V2 0 100) (V2 200 100)
--
-- <<docimages/dashed_stroke.png>>
--
dashedStroke
    :: Geometry geom
    => DashPattern -- ^ Dashing pattern to use for stroking
    -> Float       -- ^ Stroke width
    -> Join        -- ^ Which kind of join will be used
    -> (Cap, Cap)  -- ^ Start and end capping.
    -> geom        -- ^ List of elements to render
    -> Drawing px ()
dashedStroke = dashedStrokeWithOffset 0.0

-- | With stroke geometry with a given stroke width, using
-- a dash pattern. The offset is there to specify the starting
-- point into the pattern, the value can be negative.
--
-- > dashedStrokeWithOffset 3 [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
-- >     line (V2 0 100) (V2 200 100)
--
-- <<docimages/dashed_stroke_with_offset.png>>
--
dashedStrokeWithOffset
    :: Geometry geom
    => Float       -- ^ Starting offset
    -> DashPattern -- ^ Dashing pattern to use for stroking
    -> Float       -- ^ Stroke width
    -> Join        -- ^ Which kind of join will be used
    -> (Cap, Cap)  -- ^ Start and end capping.
    -> geom        -- ^ List of elements to render
    -> Drawing px ()
dashedStrokeWithOffset _ [] width join caping prims =
    stroke width join caping prims
dashedStrokeWithOffset offset dashing width join caping prims =
    liftF $ DashedStroke offset dashing width join caping (toPrimitives prims) ()

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

-- | Simply draw an image into the canvas. Take into account
-- any previous transformation performed on the geometry.
--
-- > drawImage textureImage 0 (V2 30 30)
--
-- <<docimages/image_simple.png>>
--
drawImage :: ModulablePixel px
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
drawImageAtSize img@Image { imageWidth = w, imageHeight = h } borderSize ip
            reqWidth reqHeight
    | borderSize <= 0 =
        withTransformation (translate p <> scale scaleX scaleY) .
            withTexture (SampledTexture img) $ fill rect
    | otherwise = do
        withTransformation (translate p <> scale scaleX scaleY) $
            withTexture (SampledTexture img) $ fill rect
        stroke (borderSize / 2) (JoinMiter 0)
               (CapStraight 0, CapStraight 0) rect'
        where
          p = ip ^-^ V2 0.5 0.5
          rect = rectangle (V2 0 0) rw rh
          rect' = rectangle p reqWidth reqHeight

          (rw, rh) = (fromIntegral w, fromIntegral h)
          scaleX | reqWidth == 0 = 1
                 | otherwise = reqWidth / rw

          scaleY | reqHeight == 0 = 1
                 | otherwise = reqHeight / rh

-- | Return a simple line ready to be stroked.
--
-- > stroke 17 JoinRound (CapRound, CapRound) $
-- >     line (V2 10 10) (V2 180 170)
--
-- <<docimages/stroke_line.png>>
--
line :: Point -> Point -> [Primitive]
line p1 p2 = [LinePrim $ Line p1 p2]

