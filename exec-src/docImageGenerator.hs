
import Control.Applicative( (<$>) )
import Codec.Picture
import Graphics.Text.TrueType( loadFontFile )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Directory( createDirectoryIfMissing )
import System.FilePath( (</>) )

import Linear( (^+^) )

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



textExample :: IO ()
textExample = do
  fontErr <- loadFontFile "C:/Windows/Fonts/arial.ttf"
  case fontErr of
    Left err -> putStrLn err
    Right font ->
      writePng (outFolder </> "text_example.png") .
          renderDrawing 300 70 (PixelRGBA8 255 255 255 255)
              . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                      printTextAt font 12 (V2 20 40) "A simple text test!"

main :: IO ()
main = do
    let addFolder (p, v) = (outFolder </> p, v)
    createDirectoryIfMissing True outFolder
    moduleExample 
    textExample

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

    produceDocImage (outFolder </> "fill_circle.png") $
        fill $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "stroke_circle.png") $
        stroke 5 JoinRound (CapRound, CapRound) $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "dashed_stroke.png") $
        dashedStroke [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
            line (V2 0 100) (V2 200 100)

    produceDocImage (outFolder </> "fill_rect.png") $
        fill $ rectangle (V2 30 30) 150 100

    produceDocImage (outFolder </> "with_texture.png") $
      withTexture frontTexture $ do
          fill $ circle (V2 50 50) 20
          fill $ circle (V2 100 100) 20
          withTexture accent2Texture $
               fill $ circle (V2 150 150) 20

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
