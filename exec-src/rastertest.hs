{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

import Data.Foldable( foldMap )
import Data.Monoid( (<>) )
import Control.Applicative( (<$>) )
import Graphics.Rasterific hiding ( fill
                                  , dashedStrokeWithOffset
                                  , dashedStroke
                                  , fillWithMethod, stroke)
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear( (^+^), (^-^) )
import Graphics.Rasterific.Transformations

import Graphics.Text.TrueType( loadFontFile )
import Codec.Picture
import Arbitrary
import System.Environment( getArgs )
import Criterion.Config( defaultConfig )
import Criterion.Main( parseArgs
                     , defaultOptions
                     , defaultMainWith
                     , bench
                     )
import qualified Sample as Sample

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

fill :: [Primitive] -> Drawing PixelRGBA8 ()
fill = fillWithMethod FillWinding

drawBoundingBox :: [Primitive] -> Drawing PixelRGBA8 ()
drawBoundingBox prims = do
  let PlaneBound mini maxi = foldMap planeBounds prims
      V2 width height = maxi ^-^ mini
  withTexture (uniformTexture red) $
      R.stroke 2 (JoinMiter 0) (CapStraight 0, CapStraight 0) $
        rectangle mini width height

stroke :: Float -> Join -> (Cap, Cap) -> [Primitive]
       -> Drawing PixelRGBA8 ()
stroke w j cap prims =
    R.stroke w j cap prims >> drawBoundingBox prims

dashedStroke :: DashPattern -> Float -> Join -> (Cap, Cap) -> [Primitive]
            -> Drawing PixelRGBA8 ()
dashedStroke p w j c prims =
    R.dashedStroke p w j c prims >> drawBoundingBox prims

dashedStrokeWithOffset
    :: Float -> DashPattern -> Float -> Join -> (Cap, Cap) -> [Primitive]
    -> Drawing PixelRGBA8 ()
dashedStrokeWithOffset o p w j c prims =
    R.dashedStrokeWithOffset o p w j c prims >> drawBoundingBox prims

fillWithMethod :: FillMethod -> [Primitive] -> Drawing PixelRGBA8 ()
fillWithMethod method prims =
  R.fillWithMethod method prims >> drawBoundingBox prims

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

        clipShape = R.fill $ circle (V2 250 250) 200
        
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

orientationAxisText :: IO ()
orientationAxisText =
    let trans = translate (V2 200 200) <> toNewXBase (V2 1 (-0.5)) in
    writePng (outFolder </> "axis_transform.png")
        . renderDrawing 400 400 white
        . withTexture (uniformTexture blue)
        . fill . transform (applyTransformation trans)
        . pathToPrimitives
        $ Path (V2 (-100) (-10)) True
              [ PathLineTo (V2 (-20) (-10))
              , PathLineTo (V2 0 5)
              , PathLineTo (V2 20 (-10))
              , PathLineTo (V2 100 (-10))
              , PathLineTo (V2 100 10)
              , PathLineTo (V2 (-100) 10)
              ]

complexEvenOddTest :: Int -> Texture PixelRGBA8 -> IO ()
complexEvenOddTest size texture = mapM_ tester [(filling, ix)
                                              | filling <- [(FillEvenOdd, "evenodd")
                                                           ,(FillWinding, "winding")]
                                              , ix <- [1 :: Int .. 3]] where
  command =
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

  tester ((method, name), i) =
    writePng (outFolder </> ("complex_" ++ name ++ "_" ++ show i ++ "_" ++ show size ++ "px.png"))
        . renderDrawing size size white
        . withTexture texture
        . fillWithMethod method
        . fmap (transform . applyTransformation $
                            rotateCenter (fromIntegral i / 6) (V2 (350) (350)))
        $ concatMap pathToPrimitives command

evenOddTest :: Texture PixelRGBA8 -> IO ()
evenOddTest texture = mapM_ tester [1 :: Int .. 3] where
  command =
    Path (V2 250 75) True
      [ PathLineTo (V2 323 301)
      , PathLineTo (V2 131 161)
      , PathLineTo (V2 369 161)
      , PathLineTo (V2 177 301)
      ]
  tester i =
    writePng (outFolder </> ("even_odd" ++ show i ++ ".png"))
        . renderDrawing 300 300 white
        . withTexture texture
        . fillWithMethod FillEvenOdd
        . fmap (transform . applyTransformation $
                            translate (V2 (-80) (-40))
                            <> rotateCenter (fromIntegral i / 6) (V2 (250) (200)))
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

transparentGradient :: IO ()
transparentGradient =
    writePng (outFolder </> "transparent_gradient.png") $ renderDrawing 400 200 white img
  where img = withTexture (withSampler SamplerPad
                          (linearGradientTexture gradDef
                          (V2 40 40) (V2 130 130))) $
                          fill $ circle (V2 100 100) 100
        gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                  ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                  ,(1, PixelRGBA8 0xFF 0x53 0x73 50)]
  
gradientRadial :: String -> PixelRGBA8 -> IO ()
gradientRadial name back =
    writePng (outFolder </> ("rad_opacity" ++ name ++ ".png")) $
        renderDrawing 500 500 back img
  where img = withTexture (withSampler SamplerRepeat
                          (radialGradientTexture gradDef
                          (V2 250 250) 100)) $
                          fill $ rectangle (V2 0 0) 500 500
        gradDef = 
            [(0  , PixelRGBA8 255 165 0 102)
            ,(0.5, PixelRGBA8 255 165 0 102)
            ,(0.5, PixelRGBA8 255 165 0 102)
            ,(0.525, PixelRGBA8 255 165 0 255)
            ,(0.675, PixelRGBA8 128 128 128 64)
            ,(0.75, PixelRGBA8 0 128 128 255)
            ,(1, PixelRGBA8 0 128 128 255)
            ]
strokeBad :: IO ()
strokeBad =
    writePng (outFolder </> ("bad_stroke_tiger.png")) $
        renderDrawing 500 500 white drawing
  where 
    drawing =
        withTransformation (Transformation { _transformA = 1.6
                                      , _transformC = 0.0
                                      , _transformE = 350.0
                                      , _transformB = 0.0
                                      , _transformD = 1.6
                                      , _transformF = 300.0}) $
            withTexture (uniformTexture (PixelRGBA8 76 0 0 255)) $
                stroke 2.0 (JoinMiter 1.0) (CapStraight 0.0
                                           ,CapStraight 0.0) $
                    CubicBezierPrim <$>
                        [CubicBezier (V2 21.2 63.0)
                                     (V2 21.2 63.0)
                                     (V2 4.200001 55.8)
                                     (V2 (-10.599998) 53.6)
                        ,CubicBezier (V2 (-10.599998) 53.6)
                                     (V2 (-10.599998) 53.6)
                                     (V2 (-27.199999) 51.0)
                                     (V2 (-43.8) 58.199997)
                        ,CubicBezier (V2 (-43.8) 58.199997)
                                     (V2 (-43.8) 58.199997)
                                     (V2 (-56.0) 64.2)
                                     (V2 (-61.4) 74.399994)]
pledgeTest :: IO ()
pledgeTest = do
  (Right (ImageRGBA8 png)) <- readImage "exec-src/test_img.png"
  writePng (outFolder </> "pledge_render.png") .
    renderDrawing 389 89 white $
        drawImage png 0 (V2 0 0)

testSuite :: IO ()
testSuite = do
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
  pledgeTest
  strokeBad 
  evenOddTest uniform
  orientationAxisText 
  complexEvenOddTest 700 uniform
  complexEvenOddTest 350 uniform

  weirdCircle
  dashTest

  strokeCrash
  logoTest uniform ""
  logoTest biGradient "gradient_"
  crash uniform
  transparentGradient 
  gradientRadial "white_opaque" white
  gradientRadial "black_opaque" black
  gradientRadial "white_transparent" (PixelRGBA8 255 255 255 0)
  gradientRadial "white_semi" (PixelRGBA8 255 255 255 128)
  gradientRadial "black_transparent" (PixelRGBA8 0 0 0 0)
  gradientRadial "gray_opaque" (PixelRGBA8 128 128 128 255)
  gradientRadial "gray_transparent" (PixelRGBA8 128 128 128 0)
  gradientRadial "gray_semi" (PixelRGBA8 128 128 128 128)

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

benchTest :: [String] -> IO ()
benchTest args = do
  (config, _) <-
      parseArgs defaultConfig defaultOptions args
  defaultMainWith config (return ())
        [bench "testsuite" testSuite,
         bench "Triangles" Sample.triangles]

main :: IO ()
main = do
    args <- getArgs
    case args of
         "random":_ -> randomTests
         "bench":rest -> benchTest rest
         "prof":_ -> Sample.triangles
         _ -> testSuite

