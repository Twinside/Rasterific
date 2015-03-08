{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( foldMap )
#endif

import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

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
import Graphics.Rasterific.Immediate

import Graphics.Text.TrueType( loadFontFile )
import Codec.Picture
import Arbitrary
import System.Environment( getArgs )
import Criterion.Main.Options( defaultConfig )
import Criterion.Main( defaultMainWith
                     , bench
                     , nfIO
                     )
{-import Text.Groom( groom )-}
import qualified Sample as Sample

type Stroker g =
  (Geometry g) =>
      Float -> Join -> (Cap, Cap) -> g -> Drawing PixelRGBA8 ()

type DashStroker g = DashPattern -> Stroker g

outFolder :: FilePath
outFolder = "test_results"

logo :: Int -> Bool -> Vector -> [Bezier]
logo size inv offset = bezierFromPath . way $ map (^+^ offset)
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

fill :: Geometry g => g -> Drawing PixelRGBA8 ()
fill = fillWithMethod FillWinding

drawBoundingBox :: Geometry g => g -> Drawing PixelRGBA8 ()
drawBoundingBox geom = do
  let prims = toPrimitives geom
      PlaneBound mini maxi = foldMap planeBounds prims
      V2 width height = maxi ^-^ mini
  withTexture (uniformTexture red) $
      R.stroke 2 (JoinMiter 0) (CapStraight 0, CapStraight 0) $
        rectangle mini width height

stroke :: Geometry g
       => Float -> Join -> (Cap, Cap) -> g
       -> Drawing PixelRGBA8 ()
stroke w j cap prims =
    R.stroke w j cap prims -- >> drawBoundingBox prims

dashedStroke :: Geometry g
             => DashPattern -> Float -> Join -> (Cap, Cap) -> g
             -> Drawing PixelRGBA8 ()
dashedStroke p w j c prims =
    R.dashedStroke p w j c prims >> drawBoundingBox prims

dashedStrokeWithOffset
    :: Geometry g
    => Float -> DashPattern -> Float -> Join -> (Cap, Cap) -> g
    -> Drawing PixelRGBA8 ()
dashedStrokeWithOffset o p w j c prims =
    R.dashedStrokeWithOffset o p w j c prims >> drawBoundingBox prims

fillWithMethod :: Geometry g => FillMethod -> g -> Drawing PixelRGBA8 ()
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

cubicTest :: [CubicBezier]
cubicTest = cubicBezierFromPath 
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

strokeTest2 :: (forall g. Stroker g) -> String -> IO ()
strokeTest2 stroker prefix =
    writePng (outFolder </> (prefix ++ "stroke2.png")) img
  where texture = uniformTexture blue
        points = 
            [ V2 10 10, V2 100 100
            , V2 200 20, V2 300 100, V2 450 50]
        
        drawing = withTexture texture . sequence_ . concat $
            [ []
            , [stroker 9 JoinRound (CapRound, CapStraight 0)
                    . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroker 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            ]

        img = renderDrawing 500 500 background drawing

strokeTestCliping :: (forall g. Stroker g) -> String -> IO ()
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
                    . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroker 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            ]
          withTexture (uniformTexture $ PixelRGBA8 255 128 100 128)
                    . fill $ circle (V2 150 150) 40

        img = renderDrawing 500 500 background drawing

strokeLogo :: (forall g. Stroker g) -> String -> IO ()
strokeLogo stroker prefix =
  writePng (outFolder </> (prefix ++ "stroke_logo.png")) img
    where texture = uniformTexture blue
          beziers = logo 40 False $ V2 10 10
          inverse = logo 20 True $ V2 20 20
          img = renderDrawing 100 100 background 
              . withTexture texture
              . stroker 4 JoinRound (CapRound, CapRound)
              $ beziers ++ inverse

strokeQuadraticIntersection
    :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String -> IO ()
strokeQuadraticIntersection stroker texture prefix =
  writePng (outFolder </> (prefix ++ "stroke_quad_intersection.png")) img
    where img = renderDrawing 500 500 background 
              . withTexture texture
              . stroker 40 JoinRound (CapRound, CapRound)
              $ bezierFromPath
                [ V2 30 30
                , V2 150 200
                , V2 450 450

                , V2 450 90
                , V2 30  450
                ]

strokeCubic :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String
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
                    cusp]

            , [stroker 25 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    loop]
            ]

strokeCubicDashed :: (forall g. DashStroker g) -> Texture PixelRGBA8 -> String
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
                    cusp]

            , [stroker dashPattern 25 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    loop]
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
                        printTextAt font (PointSize 12) (V2 20 40) txt

textStrokeTest :: String -> String -> String -> IO ()
textStrokeTest fontName filename txt = do
    putStrLn $ "Rendering " ++ fontName
    fontErr <- loadFontFile $ "C:/Windows/Fonts/" ++ fontName ++ ".ttf"
    case fontErr of
      Left err -> putStrLn err
      Right font -> do
        let drawing = printTextAt font (PointSize 20) (V2 30 30) txt
            orders = drawOrdersOfDrawing 300 300 96 (PixelRGBA8 0 0 0 0) drawing
        writePng (outFolder </> filename) .
            renderDrawing 300 70 white .
                withTexture (uniformTexture black) .
                    mapM_ (mapM_ (stroke 1 (JoinMiter 0) (CapRound, CapRound)
                            ) . _orderPrimitives) $ orders

strokeTest :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String
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
        $ command

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
        $ transform (applyTransformation $
                            translate (V2 (-80) (-40))
                            <> rotateCenter (fromIntegral i / 6) (V2 (250) (200)))
          command

crash :: Texture PixelRGBA8 -> IO ()
crash texture = do
    writePng (outFolder </> "crash00.png") $
        renderDrawing 600 600 background $
            withTexture texture $ fill geom
  where
    geom =
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
            [Line (V2 572.7273 572.7273) (V2 572.7273 27.272766)
            ,Line (V2 572.7273 27.272728) (V2 27.272766 27.272728)
            ,Line (V2 27.272728 27.272728) (V2 27.272728 572.72723)
            ,Line (V2 27.272728 572.7273) (V2 572.72723 572.7273)
            ]
           stroke 5 (JoinMiter 0) (CapStraight 0, CapStraight 0)
            [Line (V2 481.81818 481.81818) (V2 118.18182 481.81818)
            ,Line (V2 118.181816 481.81818) (V2 118.181816 118.18182)
            ,Line (V2 118.181816 118.181816) (V2 481.81818 118.181816)
            ,Line (V2 481.81818 118.181816) (V2 481.81818 481.81818)
            ]

 writePng (outFolder </> "stroke_crash.png") img

dashTest :: IO ()
dashTest = writePng (outFolder </> "dashed_wheel.png")
         . renderDrawing 550 550 white
         $ withTexture (uniformTexture black) drawing
  where
    drawing =
        dashedStrokeWithOffset 0.0 [4.0,4.0] 10.0
            (JoinMiter 0.0) (CapStraight 0.0,CapStraight 0.0) $
                (CubicBezier (V2 525.0 275.0) (V2 525.0 136.92882)
                             (V2 413.0712 25.0) (V2 275.0 25.0))

weirdCircle :: IO ()
weirdCircle = writePng (outFolder </> "bad_circle.png")
            . renderDrawing 400 200 white
            $ withTexture (uniformTexture black) drawing
  where
    drawing =
        fill [CubicBezier (V2 375.0 125.0) (V2 375.0 55.96441)
                          (V2 319.03558 0.0) (V2 250.0 0.0)
             ,CubicBezier (V2 250.0 (-1.4210855e-14)) (V2 180.96442 (-1.8438066e-14))
                          (V2 125.0 55.964405) (V2 125.0 125.0)
             ,CubicBezier (V2 125.0 125.0) (V2 125.0 194.03558)
                          (V2 180.9644 250.0) (V2 250.0 250.0)
             ,CubicBezier (V2 250.0 250.0) (V2 319.03558 250.0)
                          (V2 375.0 194.0356) (V2 375.0 125.0)
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

strokeBad2 :: IO ()
strokeBad2 =
    writePng (outFolder </> ("bad_stroke_letter.png")) $
        renderDrawing 70 70 white drawing
  where 
    drawing =
      withTexture (uniformTexture (PixelRGBA8 76 0 0 255)) $
        stroke 1.0 (JoinMiter 0.0) (CapRound
                                   ,CapRound) $
            BezierPrim <$>
                [
                 Bezier (V2 44.958496 23.413086) (V2 39.9292 23.413086) (V2 34.91211 23.413086)
                ,Bezier (V2 34.91211 23.413086) (V2 34.91211 24.67041) (V2 35.290527 25.610352)
                ,Bezier (V2 35.290527 25.610352) (V2 35.668945 26.538086) (V2 36.328125 27.13623)
                ,Bezier (V2 36.328125 27.13623) (V2 36.96289 27.722168) (V2 37.82959 28.015137)
                ,Bezier (V2 37.82959 28.015137) (V2 38.708496 28.308105) (V2 39.7583 28.308105)
                ,Bezier (V2 39.7583 28.308105) (V2 41.149902 28.308105) (V2 42.55371 27.75879)
                ,Bezier (V2 42.55371 27.75879) (V2 43.969727 27.197266) (V2 44.56787 26.660156)
                ,Bezier (V2 44.56787 26.660156) (V2 44.628906 26.660156) (V2 44.68994 26.660156)
                ,Bezier (V2 44.68994 26.660156) (V2 44.68994 27.91748) (V2 44.68994 29.162598)
                ,Bezier (V2 44.68994 29.162598) (V2 43.530273 29.650879) (V2 42.321777 29.980469)
                ,Bezier (V2 42.321777 29.980469) (V2 41.11328 30.310059) (V2 39.782715 30.310059)
                ,Bezier (V2 39.782715 30.310059) (V2 36.38916 30.310059) (V2 34.484863 28.479004)
                ,Bezier (V2 34.484863 28.479004) (V2 32.580566 26.635742) (V2 32.580566 23.254395)
                ,Bezier (V2 32.580566 23.254395) (V2 32.580566 19.909668) (V2 34.399414 17.944336)
                ,Bezier (V2 34.399414 17.944336) (V2 36.23047 15.979004) (V2 39.208984 15.979004)
                ,Bezier (V2 39.208984 15.979004) (V2 41.967773 15.979004) (V2 43.45703 17.590332)
                -- Bezier (V2 43.45703 17.590332) (V2 44.958496 19.20166) (V2 44.958496 22.167969)
                ,Bezier (V2 44.958496 22.167969) (V2 44.958496 22.790527) (V2 44.958496 23.413086)
                ,Bezier (V2 42.72461 21.655273) (V2 42.712402 19.848633) (V2 41.809082 18.859863)
                ,Bezier (V2 41.809082 18.859863) (V2 40.91797 17.871094) (V2 39.086914 17.871094)
                ,Bezier (V2 39.086914 17.871094) (V2 37.243652 17.871094) (V2 36.14502 18.95752)
                ,Bezier (V2 36.14502 18.95752) (V2 35.058594 20.043945) (V2 34.91211 21.655273)
                ,Bezier (V2 34.91211 21.655273) (V2 38.81836 21.655273) (V2 42.72461 21.655273)
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

shouldBeTheSame :: IO ()
shouldBeTheSame = do
    writePng (outFolder </> "should_be_same_0.png") $ img prim1
    writePng (outFolder </> "should_be_same_1.png") $ img prim2
  where
    drawColor = PixelRGBA8 0 0x86 0xc1 255
    prim1 = CubicBezier (V2  10  10) (V2 210 210)
                        (V2 210 210) (V2  10 410)
    prim2 = CubicBezier (V2  10  10) (V2 210 210)
                        (V2 210 210.1) (V2  10 410)

    img bez = renderDrawing 400 200 white $
      withTexture (uniformTexture drawColor) $
        stroke 4 JoinRound (CapRound, CapRound) bez

clipFail :: IO ()
clipFail = writePng (outFolder </> "cubicbezier_clipError.png") img
  where
    trans = applyTransformation $ translate (V2 0 (-20))
    img = renderDrawing 512 256 white .
            withTexture (uniformTexture red) $ fill geometry

    geometry = transform trans $
      [ CubicBezier (V2 104.707344 88.55418) (V2 153.00671 140.66666)
                    (V2 201.30609 192.77914) (V2 249.60547 244.89162)
      , CubicBezier (V2 249.60547 244.89162) (V2 349.59445 206.46687)
                    (V2 449.58347 168.04214) (V2 549.57245 129.6174)
      , CubicBezier (V2 549.57245 129.6174)  (V2 401.28406 115.92966)
                    (V2 252.99573 102.24192) (V2 104.707344 88.55418)
      ]
  
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
  clipFail
  pledgeTest
  strokeBad 
  strokeBad2
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
  shouldBeTheSame

  let testText =
        "Test of a text! It seems to be; à é è ç, working? () {} [] \" '"

  textAlignStringTest "CONSOLA" "alignedConsola.png" testText
  textAlignStringTest "arial" "alignedArial.png"
        "Just a simple test, gogo !!! Yay ; quoi ?"
  textStrokeTest "verdana" "stroke_verdana.png" "e"
  -- -}

benchTest :: [String] -> IO ()
benchTest _args = do
  defaultMainWith defaultConfig
        [bench "testsuite" $ nfIO testSuite,
         bench "Triangles" $ nfIO Sample.triangles]

main :: IO ()
main = do
    args <- getArgs
    case args of
         "random":_ -> randomTests
         "bench":rest -> benchTest rest
         "prof":_ -> Sample.triangles
         _ -> testSuite

