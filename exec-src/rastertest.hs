{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

import Data.Monoid( (<>) )
import Control.Applicative( (<$>) )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

import Graphics.Text.TrueType( loadFontFile )
import Codec.Picture
import Linear( (^-^)
             , (^+^)
             , (^*)
             )

type Stroker =
    Float -> Join -> (Cap, Cap) -> [Primitive]
        -> Drawing PixelRGBA8 ()

type DashStroker = DashPattern -> Stroker

outFolder :: FilePath
outFolder = "test_results"

logo :: Int -> Bool -> Vector -> [Primitive]
logo size inv offset = map BezierPrim . bezierFromPath . way $ map (^+^ offset)
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


background, blue, black, yellow, red, white :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255
red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
{-grey = PixelRGBA8 128 128 128 255-}
yellow = PixelRGBA8 255 255 0 255
{-brightblue = PixelRGBA8 0 255 255 255-}
white = PixelRGBA8 255 255 255 255

biColor, triColor :: Gradient PixelRGBA8
biColor = [ (0.0, black) , (1.0, yellow) ]
triColor = [ (0.0, blue), (0.5, white) , (1.0, red) ]

logoTest :: Texture PixelRGBA8 -> String -> IO ()
logoTest texture prefix =
    writePng (outFolder </> (prefix ++ "logo.png")) img
  where 
    beziers = logo 40 False $ V2 10 10
    inverse = logo 20 True $ V2 20 20
    drawing = withTexture texture . fill $ beziers ++ inverse
    img = renderDrawing 100 100 background drawing

makeBox :: Point -> Point -> [Primitive]
makeBox (V2 sx sy) (V2 ex ey) = map LinePrim $ lineFromPath
    [ V2 sx sy
    , V2 ex sy
    , V2 ex ey
    , V2 sx ey
    , V2 sx sy
    ]

bigBox :: Texture PixelRGBA8 -> String -> IO ()
bigBox texture prefix =
    writePng (outFolder </> (prefix ++ "box.png")) img
  where 
    drawing = withTexture texture . fill $ makeBox (V2 10 10) (V2 390 390)
    img = renderDrawing 400 400 background drawing

circleTest :: Texture PixelRGBA8 -> String -> IO ()
circleTest texture prefix =
    writePng (outFolder </> (prefix ++ "circle.png")) img
  where 
    drawing = withTexture texture . fill $ circle (V2 100 100) 90
    img = renderDrawing 200 200 background drawing

cubicTest :: [Primitive]
cubicTest = map CubicBezierPrim $ cubicBezierFromPath 
    [ V2 50 20 
    , V2 90 60
    , V2  5 100
    , V2 50 140

    , V2 70 120
    , V2 80 100
    , V2 120 80

    , V2 100 60
    , V2 70  20
    , V2 50  20
    ]

cubicTest1 :: IO ()
cubicTest1 = writePng (outFolder </> "cubic1.png") img
  where texture = uniformTexture blue
        drawing = withTexture texture $ fill cubicTest
        img = renderDrawing 150 150 background drawing

clipTest :: IO ()
clipTest = writePng (outFolder </> "clip.png") img
  where texture = uniformTexture blue
        beziers =
            [ logo 20 False $ V2 (-10) (-10)
            , logo 20 False $ V2 80 80
            , logo 20 False $ V2 0 80
            , logo 20 False $ V2 80 0
            ]

        drawing = withTexture texture $ mapM_ fill beziers
        img = renderDrawing 100 100 background drawing

strokeTest2 :: Stroker -> String -> IO ()
strokeTest2 stroker prefix =
    writePng (outFolder </> (prefix ++ "stroke2.png")) img
  where texture = uniformTexture blue
        points = 
            [ V2 10 10, V2 100 100
            , V2 200 20, V2 300 100, V2 450 50]
        
        drawing = withTexture texture . sequence_ . concat $
            [ []
            , [stroker 9 JoinRound (CapRound, CapStraight 0)
                    . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroker 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            ]

        img = renderDrawing 500 500 background drawing

strokeTestCliping :: Stroker -> String -> IO ()
strokeTestCliping stroker prefix =
    writePng (outFolder </> (prefix ++ "stroke_clipping.png")) img
  where texture = uniformTexture blue
        points = 
            [ V2 10 10, V2 100 100
            , V2 200 20, V2 300 100, V2 450 50]

        clipShape = fill $ circle (V2 250 250) 200
        
        drawing = do
          withClipping clipShape .
            withTexture texture . sequence_ . concat $
            [ []
            , [stroker 9 JoinRound (CapRound, CapStraight 0)
                    . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroker 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            ]
          withTexture (uniformTexture $ PixelRGBA8 255 128 100 128)
                    . fill $ circle (V2 150 150) 40

        img = renderDrawing 500 500 background drawing

strokeLogo :: Stroker -> String -> IO ()
strokeLogo stroker prefix =
  writePng (outFolder </> (prefix ++ "stroke_logo.png")) img
    where texture = uniformTexture blue
          beziers = logo 40 False $ V2 10 10
          inverse = logo 20 True $ V2 20 20
          img = renderDrawing 100 100 background 
              . withTexture texture
              . stroker 4 JoinRound (CapRound, CapRound)
              $ beziers ++ inverse

strokeQuadraticIntersection ::
    Stroker -> Texture PixelRGBA8 -> String -> IO ()
strokeQuadraticIntersection stroker texture prefix =
  writePng (outFolder </> (prefix ++ "stroke_quad_intersection.png")) img
    where img = renderDrawing 500 500 background 
              . withTexture texture
              . stroker 40 JoinRound (CapRound, CapRound)
              . map BezierPrim
              $ bezierFromPath
                [ V2 30 30
                , V2 150 200
                , V2 450 450

                , V2 450 90
                , V2 30  450
                ]

strokeCubic :: Stroker -> Texture PixelRGBA8 -> String
            -> IO ()
strokeCubic stroker texture prefix =
    writePng (outFolder </> (prefix ++ "cubicStroke.png")) img
  where img = renderDrawing 500 500 background drawing
        cusp = CubicBezier
            (V2 10 230)
            (V2 350 570)
            (V2 10 570)
            (V2 350 230)

        loop = CubicBezier
            (V2 160 20)
            (V2 770 500)
            (V2 140 500)
            (V2 480 70)

        drawing = withTexture texture . sequence_ . concat $
            [ []
            , [stroker 4 JoinRound (CapRound, CapRound)
                    $ take 1 cubicTest ]

            , [stroker 15 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim cusp]]

            , [stroker 25 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim loop]]
            ]

strokeCubicDashed :: DashStroker -> Texture PixelRGBA8 -> String
                  -> IO ()
strokeCubicDashed stroker texture prefix =
    writePng (outFolder </> (prefix ++ "cubicStrokeDashed.png")) img
  where img = renderDrawing 500 500 background drawing
        cusp = CubicBezier
            (V2 10 230)
            (V2 350 570)
            (V2 10 570)
            (V2 350 230)

        loop = CubicBezier
            (V2 160 20)
            (V2 770 500)
            (V2 140 500)
            (V2 480 70)

        dashPattern = [10, 5, 20, 10]

        drawing = withTexture texture . sequence_ . concat $
            [ []
            , [stroker dashPattern 4 JoinRound (CapRound, CapRound)
                    $ take 1 cubicTest ]

            , [stroker dashPattern 15 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim cusp]]

            , [stroker dashPattern 25 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim loop]]
            ]

textAlignStringTest :: String -> String -> String -> IO ()
textAlignStringTest fontName filename txt = do
    putStrLn $ "Rendering " ++ fontName
    fontErr <- loadFontFile $ "C:/Windows/Fonts/" ++ fontName ++ ".ttf"
    case fontErr of
      Left err -> putStrLn err
      Right font ->
        writePng (outFolder </> filename) .
            renderDrawing 300 70 white
                . withTexture (uniformTexture black) $
                        printTextAt font 12 (V2 20 40) txt

strokeTest :: Stroker -> Texture PixelRGBA8 -> String
           -> IO ()
strokeTest stroker texture prefix =
    writePng (outFolder </> (prefix ++ "stroke.png")) img
  where beziers base = take 1 <$>
            take 3 [ logo 100 False $ V2 ix ix | ix <- [base, base + 20 ..] ]
        drawing = withTexture texture $ sequence_ . concat $
          [ []
          , [stroker (6 + ix) (JoinMiter ix)
                    (CapStraight 0, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 10)]
          , [stroker ix
                    (JoinMiter 1) (CapRound, CapStraight 1) b
                    | (ix, b) <- zip [1 ..] (beziers 60)]
          , [stroker ix (JoinMiter 1) (CapRound, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 110)]
          , [stroker 15
                    (JoinMiter 1) (CapStraight 1, CapStraight 0)
                    . take 1 $
                    logo 150 False $ V2 200 200]
          , [stroker 5
                    (JoinMiter 1) (CapStraight 0, CapStraight 0) $
                   logo 100 False $ V2 240 240]
          ]
        img = renderDrawing 500 500 background drawing

evenOddTest :: Texture PixelRGBA8 -> IO ()
evenOddTest texture = mapM_ tester [1 :: Int .. 20] where
  command =
    Path (V2 250 75) True
      [ PathLineTo (V2 323 301)
      , PathLineTo (V2 131 161)
      , PathLineTo (V2 369 161)
      , PathLineTo (V2 177 301)
      ]
  tester i =
    writePng (outFolder </> ("even_odd" ++ show i ++ ".png"))
        .  renderDrawing 300 300 white
        . withTexture texture
        . fillWithMethod FillEvenOdd
        . fmap (transform . applyTransformation $
                            scale 0.6 0.6
                            <> translate (V2 (-60) (-34))
                            <> rotateCenter (fromIntegral i / 6) (V2 (200) (200)))
        $ pathToPrimitives command

crash :: Texture PixelRGBA8 -> IO ()
crash texture = do
    writePng (outFolder </> "crash00.png") $
        renderDrawing 600 600 background $
            withTexture texture $ fill geom
  where
    geom = concat
        [line (V2 572.7273 572.7273) (V2 572.7273 27.272766)
        ,line (V2 572.7273 27.272728) (V2 27.272766 27.272728)
        ,line (V2 27.272728 27.272728) (V2 27.272728 572.72723)
        ,line (V2 27.272728 572.7273) (V2 572.72723 572.7273)

        ,line (V2 481.81818 481.81818) (V2 118.18182 481.81818)
        ,line (V2 118.181816 481.81818) (V2 118.181816 118.18182)
        ,line (V2 118.181816 118.181816) (V2 481.81818 118.181816)
        ,line (V2 481.81818 118.181816) (V2 481.81818 481.81818)
        ]
strokeCrash :: IO ()
strokeCrash = do
 let drawColor = PixelRGBA8 0 0x86 0xc1 255
     img = renderDrawing 600 600 white $
        withTexture (uniformTexture drawColor) $ do
           stroke 5 (JoinMiter 0) (CapStraight 0, CapStraight 0)
            [LinePrim (Line (V2 572.7273 572.7273) (V2 572.7273 27.272766))
            ,LinePrim (Line (V2 572.7273 27.272728) (V2 27.272766 27.272728))
            ,LinePrim (Line (V2 27.272728 27.272728) (V2 27.272728 572.72723))
            ,LinePrim (Line (V2 27.272728 572.7273) (V2 572.72723 572.7273))
            ]
           stroke 5 (JoinMiter 0) (CapStraight 0, CapStraight 0)
            [LinePrim (Line (V2 481.81818 481.81818) (V2 118.18182 481.81818))
            ,LinePrim (Line (V2 118.181816 481.81818) (V2 118.181816 118.18182))
            ,LinePrim (Line (V2 118.181816 118.181816) (V2 481.81818 118.181816))
            ,LinePrim (Line (V2 481.81818 118.181816) (V2 481.81818 481.81818))
            ]

 writePng (outFolder </> "stroke_crash.png") img

dashTest :: IO ()
dashTest = writePng (outFolder </> "dashed_wheel.png")
         . renderDrawing 550 550 white
         $ withTexture (uniformTexture black) drawing
  where
    drawing =
        dashedStrokeWithOffset 0.0 [4.0,4.0] 10.0
            (JoinMiter 0.0) (CapStraight 0.0,CapStraight 0.0) 
            [CubicBezierPrim
                (CubicBezier (V2 525.0 275.0) (V2 525.0 136.92882)
                             (V2 413.0712 25.0) (V2 275.0 25.0))
            ]

weirdCircle :: IO ()
weirdCircle = writePng (outFolder </> "bad_circle.png")
            . renderDrawing 400 200 white
            $ withTexture (uniformTexture black) drawing
  where
    drawing =
        fill [CubicBezierPrim $ CubicBezier (V2 375.0 125.0)
                                            (V2 375.0 55.96441)
                                            (V2 319.03558 0.0)
                                            (V2 250.0 0.0)
             ,CubicBezierPrim $ CubicBezier (V2 250.0 (-1.4210855e-14))
                                            (V2 180.96442 (-1.8438066e-14))
                                            (V2 125.0 55.964405)
                                            (V2 125.0 125.0)
             ,CubicBezierPrim $ CubicBezier (V2 125.0 125.0)
                                            (V2 125.0 194.03558)
                                            (V2 180.9644 250.0)
                                            (V2 250.0 250.0)
             ,CubicBezierPrim $ CubicBezier (V2 250.0 250.0)
                                            (V2 319.03558 250.0)
                                            (V2 375.0 194.0356)
                                            (V2 375.0 125.0)
             ]

main :: IO ()
main = do
  let uniform = uniformTexture blue
      biGradient =
        linearGradientTexture biColor (V2 10 10) (V2 90 90)
      radBiGradient =
        radialGradientTexture biColor (V2 45 45) 60
      bigBiGradient =
        linearGradientTexture biColor (V2 0 10) (V2 0 390)
      triGradient =
        linearGradientTexture triColor (V2 0 10) (V2 0 390)
      radTriGradient =
        radialGradientTexture triColor (V2 250 250) 200
      radFocusTriGradient =
        radialGradientWithFocusTexture
            triColor (V2 200 200) 70 (V2 250 200)
      radFocusTriGradient2 =
        radialGradientWithFocusTexture
            triColor (V2 200 200) 70 (V2 150 170)

  createDirectoryIfMissing True outFolder
  weirdCircle
  dashTest

  strokeCrash
  evenOddTest uniform
  logoTest uniform ""
  logoTest biGradient "gradient_"
  crash uniform

  bigBox uniform ""
  bigBox biGradient "gradient_"
  bigBox bigBiGradient "gradient_big_"
  bigBox triGradient "gradient_tri_"
  bigBox radBiGradient "rad_gradient_"
  bigBox radTriGradient "rad_trigradient_"
  bigBox radFocusTriGradient "rad_focus_trigradient_"
  bigBox radFocusTriGradient2 "rad_focus_trigradient_2_"

  circleTest uniform ""
  strokeTestCliping stroke ""

  cubicTest1
  clipTest
  strokeTest stroke uniform ""
  strokeTest stroke bigBiGradient "gradient_"
  strokeTest stroke radTriGradient "rad_gradient_"
  strokeLogo stroke ""

  strokeQuadraticIntersection stroke uniform ""
  strokeQuadraticIntersection stroke triGradient "gradient_"
  strokeQuadraticIntersection stroke radBiGradient "rad_gradient_"

  strokeTest2 stroke ""

  strokeCubic stroke uniform ""
  strokeCubic stroke bigBiGradient "gradient_"
  strokeCubic stroke radTriGradient "rad_gradient_"

  strokeCubicDashed dashedStroke uniform ""

  let testText =
        "Test of a text! It seems to be; à é è ç, working? () {} [] \" '"

  textAlignStringTest "CONSOLA" "alignedConsola.png" testText
  textAlignStringTest "arial" "alignedArial.png"
        "Just a simple test, gogo !!! Yay ; quoi ?"
  -- -}

