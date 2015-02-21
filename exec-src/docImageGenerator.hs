{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>) )
#endif

import Control.Applicative( (<$>) )

import Control.Monad( forM_ )
import Control.Monad.ST( runST )
import Data.Monoid( (<>) )
import Codec.Picture
import Codec.Picture.Types( promoteImage )
import Graphics.Text.TrueType( loadFontFile )
import Graphics.Rasterific
import Graphics.Rasterific.Outline
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Immediate
import System.Directory( createDirectoryIfMissing )
import System.FilePath( (</>) )

import Graphics.Rasterific.Linear( (^+^) )

logo :: Int -> Bool -> Vector -> [Primitive]
logo size inv offset =
    map BezierPrim . bezierFromPath . way $ map (^+^ offset)
    [ (V2   0  is)
    , (V2   0   0)
    , (V2  is   0)
    , (V2 is2   0)
    , (V2 is2  is)
    , (V2 is2 is2)
    , (V2  is is2)
    , (V2  0  is2)
    , (V2  0   is)
    ]
  where is = fromIntegral size
        is2 = is + is

        way | inv = reverse
            | otherwise = id

backgroundColor :: PixelRGBA8
backgroundColor = PixelRGBA8 255 255 255 255

frontTexture, accentTexture, accent2Texture :: Texture PixelRGBA8
frontTexture = uniformTexture $ PixelRGBA8 0 0x86 0xc1 255
accentTexture = uniformTexture $ PixelRGBA8 0xff 0xf4 0xc1 255
accent2Texture = uniformTexture $ PixelRGBA8 0xFF 0x53 0x73 255

produceDocImage :: FilePath -> Drawing PixelRGBA8 () -> IO ()
produceDocImage filename drawing = writePng filename img
  where
    img = renderDrawing 200 200 backgroundColor
        $ withTexture frontTexture drawing

capTester :: (FilePath, Cap) -> IO ()
capTester (filename, cap) =
    produceDocImage filename $ do
        stroke 30 JoinRound (cap, cap) base_stroke
        withTexture accentTexture $
            stroke 2 JoinRound (cap, cap) base_stroke
  where 
    base_stroke = line (V2 0 200) (V2 100 100)

joinTester :: (FilePath, Join) -> IO ()
joinTester (filename, join) =
    produceDocImage filename $ do
        stroke 30 join (CapRound, CapRound) base_stroke
        withTexture accentTexture $
            stroke 2 join (CapRound, CapRound) base_stroke
  where 
    base_stroke = LinePrim <$>
        [ Line (V2 0 200) (V2 100 100)
        , Line (V2 100 100) (V2 200 200)
        ]

samplerTester :: (FilePath, SamplerRepeat) -> IO ()
samplerTester (filename, sampler) =
    produceDocImage filename $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (withSampler sampler $ linearGradientTexture gradDef
                        (V2 80 100) (V2 120 110)) $
            fill $ rectangle (V2 10 10) 180 180)

outFolder :: FilePath
outFolder = "docimages"

moduleExample :: IO ()
moduleExample = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      recColor = PixelRGBA8 0xFF 0x53 0x73 255
      img = renderDrawing 400 200 white $
         withTexture (uniformTexture drawColor) $ do
            fill $ circle (V2 0 0) 30
            stroke 4 JoinRound (CapRound, CapRound) $
                   circle (V2 400 200) 40
            withTexture (uniformTexture recColor) .
                fill $ rectangle (V2 100 100) 200 100

  writePng (outFolder </> "module_example.png") img

arialFont :: FilePath
arialFont =
#ifdef __WIN32__
  "C:/Windows/Fonts/arial.ttf"
#else
  "/usr/share/fonts/truetype/msttcorefonts/arial.ttf"
#endif

monospaceFont :: FilePath
monospaceFont =
#ifdef __WIN32__
  "C:/Windows/Fonts/consola.ttf"
#else
  "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
#endif

textOnPathExample :: IO ()
textOnPathExample = do
  fontErr <- loadFontFile arialFont
  case fontErr of
    Left err -> putStrLn err
    Right font ->
      let path = Path (V2 100 180) False
                      [PathCubicBezierCurveTo (V2 20 20) (V2 170 20) (V2 300 200)]
      in
      produceDocImage (outFolder </> "text_on_path.png") $ do
        stroke 3 JoinRound (CapStraight 0, CapStraight 0) $
            pathToPrimitives path

        withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
          withPathOrientation path 0 $
            printTextAt font (PointSize 24) (V2 0 0) "Text on path"

geometryOnPath :: IO ()
geometryOnPath = do
  fontErr <- loadFontFile arialFont
  case fontErr of
    Left err -> putStrLn err
    Right font ->
      produceDocImage (outFolder </> "geometry_on_path.png") $ do
        let path = Path (V2 100 180) False
                        [PathCubicBezierCurveTo (V2 20 20) (V2 170 20) (V2 300 200)]
        withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
          stroke 3 JoinRound (CapStraight 0, CapStraight 0) $
              pathToPrimitives path
     
        withPathOrientation path 0 $ do
          printTextAt font (PointSize 24) (V2 0 0) "TX"
          fill $ rectangle (V2 (-10) (-10)) 30 20
          fill $ rectangle (V2 45 0) 10 20
          fill $ rectangle (V2 60 (-10)) 20 20
          fill $ rectangle (V2 100 (-15)) 20 50

textExample :: IO ()
textExample = do
  fontErr <- loadFontFile arialFont
  case fontErr of
    Left err -> putStrLn err
    Right font ->
      writePng (outFolder </> "text_example.png") .
          renderDrawing 300 70 (PixelRGBA8 255 255 255 255)
              . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                      printTextAt font (PointSize 12) (V2 20 40) "A simple text test!"

textMultipleExample :: IO ()
textMultipleExample = do
  eitherFont1 <- loadFontFile arialFont
  eitherFont2 <- loadFontFile monospaceFont
  case (,) <$> eitherFont1 <*> eitherFont2 of
    Left err -> putStrLn err
    Right (font1, font2) ->
      writePng (outFolder </> "text_complex_example.png") .
          renderDrawing 300 70 (PixelRGBA8 255 255 255 255) $
              let blackTexture =
                    Just . uniformTexture $ PixelRGBA8 0 0 0 255
                  redTexture =
                    Just . uniformTexture $ PixelRGBA8 255 0 0 255
              in
              printTextRanges (V2 20 40)
                [ TextRange font1 (PointSize 12) "A complex " blackTexture
                , TextRange font2 (PointSize 8) "text test" redTexture]
                    
                    

coordinateSystem :: IO ()
coordinateSystem = do
    fontErr <- loadFontFile arialFont
    case fontErr of
        Left err -> putStrLn err
        Right font -> 
            writePng (outFolder </> "coordinate.png") 
                . renderDrawing 200 200 white
                $ create font
  where
    white = PixelRGBA8 255 255 255 255
    black = PixelRGBA8   0   0   0 255
    stroker = stroke 4 JoinRound (CapStraight 0, CapStraight 0)
    filler = fill . pathToPrimitives
    create font = withTexture (uniformTexture black) $ do
        stroker $ line (V2 10 40) (V2 190 40)
        stroker $ line (V2 40 10) (V2 40 190)
        printTextAt font (PointSize 12) (V2 4 37) "(0,0)"
        printTextAt font (PointSize 12) (V2 100 37) "(width, 0)"
        printTextAt font (PointSize 12) (V2 57 190) "(0, height)"
        filler $ Path (V2 170 30) True
            [PathLineTo (V2 195 40), PathLineTo (V2 170 50)]
        filler $ Path (V2 30 170) True
            [PathLineTo (V2 40 195), PathLineTo (V2 50 170)]

fillingSample :: FillMethod -> Drawing px ()
fillingSample fillMethod = fillWithMethod fillMethod geometry where
  geometry = transform (applyTransformation $ scale 0.35 0.4
                                           <> translate (V2 (-80) (-180)))
           $ concatMap pathToPrimitives
     [ Path (V2 484 499) True
         [ PathCubicBezierCurveTo (V2 681 452) (V2 639 312) (V2 541 314)
         , PathCubicBezierCurveTo (V2 327 337) (V2 224 562) (V2 484 499)
         ]
     , Path (V2 136 377) True
         [ PathCubicBezierCurveTo (V2 244 253) (V2 424 420) (V2 357 489)
         , PathCubicBezierCurveTo (V2 302 582) (V2 47 481) (V2 136 377)
         ]
     , Path (V2 340 265) True
         [ PathCubicBezierCurveTo (V2 64 371) (V2 128 748) (V2 343 536)
         , PathCubicBezierCurveTo (V2 668 216) (V2 17 273) (V2 367 575)
         , PathCubicBezierCurveTo (V2 589 727) (V2 615 159) (V2 340 265)
         ]
     ]

immediateDrawExample :: Image PixelRGBA8
immediateDrawExample = runST $
  runDrawContext 200 200 (PixelRGBA8 0 0 0 255) $
    fillWithTexture FillWinding texture geometry
  where
    circlePrimitives = circle (V2 100 100) 50
    geometry = strokize 4 JoinRound (CapRound, CapRound) circlePrimitives
    texture = uniformTexture (PixelRGBA8 255 255 255 255)

immediateDrawMaskExample :: Image PixelRGBA8
immediateDrawMaskExample = runST $
  runDrawContext 200 200 (PixelRGBA8 0 0 0 255) $
    forM_ [1 .. 10] $ \ix ->
       fillWithTextureAndMask FillWinding texture mask $
           rectangle (V2 10 (ix * 18 - 5)) 180 13
  where
    texture = uniformTexture $ PixelRGBA8 0 0x86 0xc1 255
    mask = sampledImageTexture
         $ runST
         $ runDrawContext 200 200 0
         $ fillWithTexture FillWinding (uniformTexture 255) maskGeometry

    maskGeometry = strokize 15 JoinRound (CapRound, CapRound)
                 $ circle (V2 100 100) 80

main :: IO ()
main = do
    let addFolder (p, v) = (outFolder </> p, v)
    createDirectoryIfMissing True outFolder
    moduleExample 
    mapM_ (capTester . addFolder)
        [ ("cap_straight.png", CapStraight 0)
        , ("cap_straight_1.png", CapStraight 1)
        , ("cap_round.png", CapRound)
        ]

    mapM_ (joinTester . addFolder)
        [ ("join_round.png", JoinRound)
        , ("join_miter.png", JoinMiter 0)
        , ("join_miter_5.png", JoinMiter 5)
        ]

    mapM_ (samplerTester . addFolder)
        [ ("sampler_pad.png", SamplerPad)
        , ("sampler_repeat.png", SamplerRepeat)
        , ("sampler_reflect.png", SamplerReflect)
        ]

    writePng (outFolder </> "immediate_fill.png") immediateDrawExample
    writePng (outFolder </> "immediate_mask.png") immediateDrawMaskExample 

    produceDocImage (outFolder </> "fill_circle.png") $
        fill $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "fill_ellipse.png") $
        fill $ ellipse (V2 100 100) 75 30

    produceDocImage (outFolder </> "stroke_circle.png") $
        stroke 5 JoinRound (CapRound, CapRound) $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "dashed_stroke.png") $
        dashedStroke [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
            line (V2 0 100) (V2 200 100)

    produceDocImage (outFolder </> "dashed_stroke_with_offset.png") $
        dashedStrokeWithOffset 3 [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
            line (V2 0 100) (V2 200 100)

    produceDocImage (outFolder </> "fill_rect.png") $
        fill $ rectangle (V2 30 30) 150 100

    produceDocImage (outFolder </> "with_texture.png") $
      withTexture frontTexture $ do
          fill $ circle (V2 50 50) 20
          fill $ circle (V2 100 100) 20
          withTexture accent2Texture $
               fill $ circle (V2 150 150) 20

    produceDocImage (outFolder </> "strokize_path.png") $
      stroke 3 (JoinMiter 0) (CapStraight 0, CapStraight 0) $
          strokize 40 JoinRound (CapRound, CapRound)
            [CubicBezierPrim $
                 CubicBezier (V2  40 160) (V2 40   40)
                             (V2 160  40) (V2 160 160)]

    produceDocImage (outFolder </> "strokize_dashed_path.png") $
      mapM_ (stroke 3 (JoinMiter 0) (CapStraight 0, CapStraight 0)) $
          dashedStrokize 0 [10, 5]
                         40 JoinRound (CapStraight 0, CapStraight 0)
            [CubicBezierPrim $
                 CubicBezier (V2  40 160) (V2 40   40)
                             (V2 160  40) (V2 160 160)]

    produceDocImage (outFolder </> "with_clipping.png") $
      withClipping (fill $ circle (V2 100 100) 75) $
          mapM_ (stroke 7 JoinRound (CapRound, CapRound))
            [line (V2 0 yf) (V2 200 (yf + 10)) 
                           | y <- [5 :: Int, 17 .. 200]
                           , let yf = fromIntegral y ]

    produceDocImage (outFolder </> "stroke_line.png") $
      stroke 17 JoinRound (CapRound, CapRound) $
        line (V2 10 10) (V2 180 170)

    produceDocImage (outFolder </> "linear_gradient.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (linearGradientTexture gradDef (V2 40 40) (V2 130 130)) $
            fill $ circle (V2 100 100) 100)

    produceDocImage (outFolder </> "radial_gradient.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (radialGradientTexture gradDef (V2 100 100) 75) $
            fill $ circle (V2 100 100) 100)

    produceDocImage (outFolder </> "radial_gradient_focus.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (radialGradientWithFocusTexture gradDef (V2 100 100) 75 (V2 70 70)) $
            fill $ circle (V2 100 100) 100)

    produceDocImage (outFolder </> "sampler_pad.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (linearGradientTexture gradDef (V2 80 100) (V2 120 110)) $
            fill $ rectangle (V2 10 10) 180 180)

    produceDocImage (outFolder </> "logo.png") $
        fill $ logo 80 False (V2 20 20) ++ 
               logo 40 True (V2 40 40)

    produceDocImage (outFolder </> "cubic_bezier.png") $
        stroke 5 JoinRound (CapRound, CapRound) $
            [CubicBezierPrim $ CubicBezier (V2 0 10) (V2 205 250)
                                           (V2 (-10) 250) (V2 160 35)]

    produceDocImage (outFolder </> "quadratic_bezier.png") $
        fill $ BezierPrim <$> [Bezier (V2 10 10) (V2 200 50) (V2 200 100)
                            ,Bezier (V2 200 100) (V2 150 200) (V2 120 175)
                            ,Bezier (V2 120 175) (V2 30 100) (V2 10 10)]

    produceDocImage (outFolder </> "simple_line.png") $
        fill $ LinePrim <$> [ Line (V2 10 10) (V2 190 10)
                            , Line (V2 190 10) (V2 95 170)
                            , Line (V2 95 170) (V2 10 10)]

    produceDocImage (outFolder </> "primitive_mixed.png") $
        fill
            [ CubicBezierPrim $ CubicBezier (V2 50 20) (V2 90 60)
                                            (V2  5 100) (V2 50 140)
            , LinePrim $ Line (V2 50 140) (V2 120 80)
            , LinePrim $ Line (V2 120 80) (V2 50 20) ]

    produceDocImage (outFolder </> "path_example.png") $
       fill . pathToPrimitives $ Path (V2 50 20) True
          [ PathCubicBezierCurveTo (V2 90 60) (V2  5 100) (V2 50 140)
          , PathLineTo (V2 120 80) ]

    produceDocImage (outFolder </> "stroke_polyline.png") $
        stroke 4 JoinRound (CapRound, CapRound) $
            polyline [V2 10 10, V2 100 70, V2 190 190]

    produceDocImage (outFolder </> "fill_polygon.png") $
        fill $ polygon [V2 30 30, V2 100 70, V2 80 170]

    produceDocImage  (outFolder </> "fill_roundedRectangle.png") $
        fill $ roundedRectangle (V2 10 10) 150 150 20 10

    produceDocImage  (outFolder </> "stroke_roundedRectangle.png") $
        stroke 4 JoinRound (CapRound, CapRound) $
            roundedRectangle (V2 10 10) 150 150 20 10

    produceDocImage (outFolder </> "fill_evenodd.png") $
        fillingSample FillEvenOdd

    produceDocImage (outFolder </> "fill_winding.png") $
        fillingSample FillWinding

    produceDocImage (outFolder </> "transform_rotate.png") $
        fill . transform (applyTransformation $ rotate 0.2)
             $ rectangle (V2 40 40) 120 120

    produceDocImage (outFolder </> "transform_rotate_center.png") $
        fill . transform (applyTransformation $ rotateCenter 0.2 (V2 200 200))
             $ rectangle (V2 40 40) 120 120

    produceDocImage (outFolder </> "transform_translate.png") $
        fill . transform (applyTransformation $ translate (V2 100 100))
             $ rectangle (V2 40 40) 40 40

    produceDocImage (outFolder </> "transform_scale.png") $
        fill . transform (applyTransformation $ scale 2 2)
             $ rectangle (V2 40 40) 40 40

    produceDocImage (outFolder </> "transform_skewx.png") $
        fill . transform (applyTransformation $ skewX 0.3)
             $ rectangle (V2 50 50) 80 80

    produceDocImage (outFolder </> "transform_skewy.png") $
        fill . transform (applyTransformation $ skewY 0.3)
             $ rectangle (V2 50 50) 80 80

    Right (ImageRGB8 img) <- readImage "avatar.png"
    let textureImage = promoteImage img
    produceDocImage (outFolder </> "sampled_texture_repeat.png") $
        withTexture (withSampler SamplerRepeat $
                        sampledImageTexture textureImage) $
            fill $ rectangle (V2 0 0) 200 200

    produceDocImage (outFolder </> "image_simple.png") $
        drawImage textureImage 0 (V2 30 30)

    produceDocImage (outFolder </> "image_resize.png") $
        drawImageAtSize textureImage 2 (V2 30 30) 128 128

    produceDocImage (outFolder </> "sampled_texture_reflect.png") $
        withTexture (withSampler SamplerReflect $
                        sampledImageTexture textureImage) $
            fill $ rectangle (V2 0 0) 200 200

    produceDocImage (outFolder </> "sampled_texture_pad.png") $
        withTexture (sampledImageTexture textureImage) $
            fill $ rectangle (V2 0 0) 200 200

    produceDocImage (outFolder </> "sampled_texture_rotate.png") $
        withTexture (withSampler SamplerRepeat $
                    transformTexture (rotateCenter 1 (V2 0 0))
                    $ sampledImageTexture textureImage) $
            fill $ rectangle (V2 0 0) 200 200

    produceDocImage (outFolder </> "sampled_texture_scaled.png") $
        withTexture (withSampler SamplerRepeat $
                    transformTexture (rotateCenter 1 (V2 0 0) <> 
                                      scale 0.5 0.25)
                    $ sampledImageTexture textureImage) $
            fill $ rectangle (V2 0 0) 200 200

    textExample
    textMultipleExample 
    coordinateSystem
    textOnPathExample
    geometryOnPath

