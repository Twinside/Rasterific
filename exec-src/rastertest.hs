{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

import Data.Monoid( (<>) )
import Graphics.Rasterific hiding ( fill
                                  , dashedStrokeWithOffset
                                  , dashedStroke
                                  , fillWithMethod, stroke)
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear( (^+^), (^-^) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Immediate

import qualified Data.ByteString.Lazy as LB
import Graphics.Text.TrueType( loadFontFile )
import Codec.Picture
import Arbitrary
import System.Environment( getArgs )
{-
import Criterion.Main.Options( defaultConfig )
import Criterion.Main( defaultMainWith
                     , bench
                     , nfIO
                     )
-- -}
{-import Text.Groom( groom )-}
import qualified Sample as Sample

import Debug.Trace

type Stroker g =
  (Geometry g) =>
      Float -> Join -> (Cap, Cap) -> g -> Drawing PixelRGBA8 ()

type DashStroker g = DashPattern -> Stroker g

outFolder :: FilePath
outFolder = "test_results"

sansSerifFont :: FilePath
sansSerifFont = "test_fonts/DejaVuSans.ttf"

monospaceFont :: FilePath
monospaceFont =  "test_fonts/DejaVuSansMono.ttf"

produceImageAtSize :: Int -> Int -> FilePath -> Drawing PixelRGBA8 () -> IO ()
produceImageAtSize width height originalName drawing = do
    putStrLn $ "Producing " <> filename
    writePng filename img
    writePdf $ filename <> ".draw.pdf"
    writeOrderPdf $ filename <> ".order.pdf"
  where
    filename = outFolder </> originalName
    img = renderDrawing width height background drawing

    writeOrderPdf fname =
      LB.writeFile fname .
        renderOrdersAtDpiToPdf width height 92 $
          drawOrdersOfDrawing width height 92 (PixelRGBA8 0 0 0 0) $ drawing

    writePdf fname =
      LB.writeFile fname .
        renderDrawingAtDpiToPDF width height 92 $ drawing

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
yellow = PixelRGBA8 255 255 0 255
white = PixelRGBA8 255 255 255 255
{-brightblue = PixelRGBA8 0 255 255 255-}
{-green =  PixelRGBA8 0 255 0 255-}
{-grey = PixelRGBA8 128 128 128 255-}
{-orange = PixelRGBA8 255 0xA5 0 255-}

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
dashedStroke p w j c prims = do
    drawBoundingBox prims
    R.dashedStroke p w j c prims

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
  produceImageAtSize 100 100 (prefix ++ "logo.png") drawing
  where 
    beziers = logo 40 False $ V2 10 10
    inverse = logo 20 True $ V2 20 20
    drawing = withTexture texture . fill $ beziers ++ inverse

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
  produceImageAtSize 400 400 (prefix ++ "box.png") $
     withTexture texture . fill $ makeBox (V2 10 10) (V2 390 390)

circleTest :: Texture PixelRGBA8 -> String -> IO ()
circleTest texture prefix =
  produceImageAtSize 200 200 (prefix ++ "circle.png") $
      withTexture texture . fill $ circle (V2 100 100) 90

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
cubicTest1 = 
  produceImageAtSize 150 150 "cubic1.png" $
      withTexture (uniformTexture blue) $ fill cubicTest

clipTest :: IO ()
clipTest =
  produceImageAtSize 100 100 "clip.png" $
      withTexture (uniformTexture blue) $ mapM_ fill beziers
  where 
    beziers =
        [ logo 20 False $ V2 (-10) (-10)
        , logo 20 False $ V2 80 80
        , logo 20 False $ V2 0 80
        , logo 20 False $ V2 80 0
        ]

strokeTest2 :: (forall g. Stroker g) -> String -> IO ()
strokeTest2 stroker prefix =
    produceImageAtSize 500 500 (prefix ++ "stroke2.png") drawing
  where
    texture = uniformTexture blue
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

strokeTestCliping :: (forall g. Stroker g) -> String -> IO ()
strokeTestCliping stroker prefix =
  produceImageAtSize 500 500 (prefix ++ "stroke_clipping.png") drawing
  where
    texture = uniformTexture blue
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

strokeLogo :: (forall g. Stroker g) -> String -> IO ()
strokeLogo stroker prefix =
  produceImageAtSize 100 100 (prefix ++ "stroke_logo.png") .
    withTexture (uniformTexture blue) .
      stroker 4 JoinRound (CapRound, CapRound) $ beziers ++ inverse
    where
      beziers = logo 40 False $ V2 10 10
      inverse = logo 20 True $ V2 20 20

strokeQuadraticIntersection
    :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String -> IO ()
strokeQuadraticIntersection stroker texture prefix =
  produceImageAtSize 500 500 (prefix ++ "stroke_quad_intersection.png") $
    withTexture texture $
      stroker 40 JoinRound (CapRound, CapRound) . bezierFromPath $
        [ V2 30 30, V2 150 200, V2 450 450, V2 450 90, V2 30  450 ]

strokeCubic :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String
            -> IO ()
strokeCubic stroker texture prefix =
  produceImageAtSize 500 500 (prefix ++ "cubicStroke.png") drawing
  where
    cusp = CubicBezier (V2 10 230) (V2 350 570) (V2 10 570) (V2 350 230)
    loop = CubicBezier (V2 160 20) (V2 770 500) (V2 140 500) (V2 480 70)

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
    produceImageAtSize 500 500 (prefix ++ "cubicStrokeDashed.png") drawing
  where
    cusp = CubicBezier (V2 10 230) (V2 350 570) (V2 10 570) (V2 350 230)
    loop = CubicBezier (V2 160 20) (V2 770 500) (V2 140 500) (V2 480 70)
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

strokeCubicDashed2 :: (forall g. DashStroker g) -> Texture PixelRGBA8 -> String
                   -> IO ()
strokeCubicDashed2 stroker texture prefix =
    produceImageAtSize 500 500 (prefix ++ "cubicStrokeDashed2.png") drawing
  where
    circ =
        [ CubicBezier (V2 34.0 20.0) (V2 34.0 12.268013) (V2 27.731987 6.0) (V2 20.0 6.0)
        , CubicBezier (V2 20.0 6.0) (V2 12.268013 6.0) (V2 6.0 12.268013) (V2 6.0 20.0)
        , CubicBezier (V2 6.0 20.0) (V2 6.0 27.731987) (V2 12.268013 34.0) (V2 20.0 34.0)
        , CubicBezier (V2 20.0 34.0) (V2 27.731987 34.0) (V2 34.0 27.731987) (V2 34.0 20.0)
        ]

    strokeCircle = stroker [2, 2] 2 (JoinMiter 0) (CapStraight 0, CapStraight 0) circ

    drawing = withTexture texture $ do
      strokeCircle 
      withTransformation (translate (V2 30 30) <> scale 2 2) strokeCircle
      withTransformation (translate (V2 80 80) <> scale 4 4) strokeCircle
      withTransformation (translate (V2 180 180) <> scale 5 5) strokeCircle

textAlignStringTest :: String -> String -> String -> IO ()
textAlignStringTest fontName filename txt = do
    putStrLn $ "Rendering " ++ fontName
    fontErr <- loadFontFile fontName
    case fontErr of
      Left err -> putStrLn err
      Right font ->
        produceImageAtSize 300 70 filename .
            withTexture (uniformTexture black) $
              printTextAt font (PointSize 12) (V2 20 40) txt

textStrokeTest :: String -> String -> String -> IO ()
textStrokeTest fontName filename txt = do
    putStrLn $ "Rendering " ++ fontName
    fontErr <- loadFontFile fontName
    case fontErr of
      Left err -> putStrLn err
      Right font -> do
        let drawing = printTextAt font (PointSize 20) (V2 30 30) txt
            orders = drawOrdersOfDrawing 300 300 96 (PixelRGBA8 0 0 0 0) drawing
        produceImageAtSize 300 70 filename .
            withTexture (uniformTexture black) $
                mapM_ (mapM_ (stroke 1 (JoinMiter 0) (CapRound, CapRound)) . _orderPrimitives) orders

strokeTest :: (forall g. Stroker g) -> Texture PixelRGBA8 -> String
           -> IO ()
strokeTest stroker texture prefix =
    produceImageAtSize 500 500 (prefix ++ "stroke.png") drawing
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

strokeWidthTest :: (forall g. Stroker g)
                -> IO ()
strokeWidthTest stroker =
    produceImageAtSize 500 500 "stroke_width.png"
        $ withTexture (uniformTexture black)
        $ drawing
  where
    drawing = sequence_ $
          [ stroker w JoinRound (CapRound, CapRound) l
          | (w, l) <- zip [0..] ls ]
    ls = [ lineFromPath [ V2 50 (50 * i), V2 450 (50 * i) ]
         | i <- [1..9] ]

strokePixelTest :: (forall g. Stroker g)
                -> IO ()
strokePixelTest stroker =
    produceImageAtSize 10 7 "stroke_pixel.png"
        $ withTexture (uniformTexture black)
        $ drawing
  where
    drawing = sequence_ $
          [ stroker 1 JoinRound (CapStraight 0, CapStraight 0) g
          | g <- gs ]
    l = lineFromPath
    gs = -- line of 1, 2, 3 pixels
         [ [ l [ V2 1 1.5, V2 2 1.5 ] ]
         , [ l [ V2 3 1.5, V2 5 1.5 ] ]
         , [ l [ V2 6 1.5, V2 9 1.5 ] ]
         -- line of halved lines, stroked individually
         , [ l [ V2 1 3.5, V2 1.5 3.5 ] ]
         , [ l [ V2 1.5 3.5, V2 2 3.5 ] ]
         , [ l [ V2 3 3.5, V2 4 3.5 ] ]
         , [ l [ V2 4 3.5, V2 5 3.5 ] ]
         , [ l [ V2 6 3.5, V2 7.5 3.5 ] ]
         , [ l [ V2 7.5 3.5, V2 9 3.5 ] ]
         -- line of halved lines, stroked as one
         , [ l [ V2 1 5.5, V2 1.5 5.5 ], l [ V2 1.5 5.5, V2 2 5.5 ] ]
         , [ l [ V2 3 5.5, V2 4 5.5 ], l [ V2 4 5.5, V2 5 5.5 ] ]
         , [ l [ V2 6 5.5, V2 7.5 5.5 ], l [ V2 7.5 5.5, V2 9 5.5 ] ]
         ]

orientationAxisText :: IO ()
orientationAxisText =
    let trans = translate (V2 200 200) <> toNewXBase (V2 1 (-0.5)) in
    produceImageAtSize 400 400 "axis_transform.png"
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
    produceImageAtSize size size
        ("complex_" ++ name ++ "_" ++ show i ++ "_" ++ show size ++ "px.png")
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
    produceImageAtSize 300 300 ("even_odd" ++ show i ++ ".png")
        . withTexture texture
        . fillWithMethod FillEvenOdd
        $ transform (applyTransformation $
                            translate (V2 (-80) (-40))
                            <> rotateCenter (fromIntegral i / 6) (V2 (250) (200)))
          command

crash :: Texture PixelRGBA8 -> IO ()
crash texture = do
  produceImageAtSize 600 600 "crash00.png" $
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
 produceImageAtSize 600 600 "stroke_crash.png" $
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

dashTest :: IO ()
dashTest = 
  produceImageAtSize 550 550 "dashed_wheel.png" $
     withTexture (uniformTexture black) drawing
  where
    drawing =
        dashedStrokeWithOffset 0.0 [4.0,4.0] 10.0
            (JoinMiter 0.0) (CapStraight 0.0,CapStraight 0.0) $
                (CubicBezier (V2 525.0 275.0) (V2 525.0 136.92882)
                             (V2 413.0712 25.0) (V2 275.0 25.0))

weirdCircle :: IO ()
weirdCircle =
  produceImageAtSize 400 200 "bad_circle.png" $
     withTexture (uniformTexture black) $
        fill [CubicBezier (V2 375.0 125.0) (V2 375.0 55.96441)
                          (V2 319.03558 0.0) (V2 250.0 0.0)
             ,CubicBezier (V2 250.0 (-1.4210855e-14)) (V2 180.96442 (-1.8438066e-14))
                          (V2 125.0 55.964405) (V2 125.0 125.0)
             ,CubicBezier (V2 125.0 125.0) (V2 125.0 194.03558)
                          (V2 180.9644 250.0) (V2 250.0 250.0)
             ,CubicBezier (V2 250.0 250.0) (V2 319.03558 250.0)
                          (V2 375.0 194.0356) (V2 375.0 125.0)
             ]

compositionClip :: IO ()
compositionClip =
  produceImageAtSize 500 500 "composition_clip.png" $ do
    withClipping clipPath $
      withTexture (uniformTexture blue) $
        fill $ rectangle (V2 0 0) 500 500
  where
    clipPath  =
      withClipping (R.fill $ rectangle (V2 0 200) 500 100) .
        R.fill $ ellipse (V2 250 250) 180 170

compositionTransparentGradient :: IO ()
compositionTransparentGradient =
  produceImageAtSize 600 800 "composition_gradient.png" $ do
    row $ texture SamplerPad gradDef
    withTransformation (translate $ V2 0 200) $
        row $ texture SamplerPad opaqueGrad
    withTransformation (translate $ V2 0 400) $
        strokeRow $ texture SamplerRepeat opaqueGrad
    withTransformation (translate $ V2 0 600) $
        strokeRow $ texture SamplerReflect gradDef

  where
    row tx = do
      baseDrawing tx
      withTransformation (translate (V2 200 0)) $ baseDrawing tx
      withTransformation (translate (V2 400 0) <> scale 0.5 0.5) $ baseDrawing tx
      withTransformation (translate (V2 440 100) <> rotate 0.4 <> scale 0.3 0.5) $ baseDrawing tx

    strokeElem tx =
      withTransformation (scale (1 / 2.5) (1/ 2.5)) $
        withTexture tx $
          stroke 40 JoinRound (CapRound, CapRound) . bezierFromPath $
            [ V2 30 30, V2 150 200, V2 450 450, V2 450 90, V2 30  450 ]

    strokeRow tx =
      strokeElem tx

    baseDrawing tx = 
      withTexture tx . fill $ circle (V2 100 100) 100

    texture samp grad =
      withSampler samp $
        linearGradientTexture grad (V2 40 40) (V2 130 130)

    opaqueGrad = [(0, PixelRGBA8 0 0x86 0xc1 255)
                 ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                 ,(1, PixelRGBA8 0xFF 0x53 0x73 255)]

    gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
              ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
              ,(1, PixelRGBA8 0xFF 0x53 0x73 50)]

transparentGradient :: IO ()
transparentGradient =
  produceImageAtSize 400 200 "transparent_gradient.png" $
    withTexture (withSampler SamplerPad
                    (linearGradientTexture gradDef (V2 40 40) (V2 130 130))) $
                          fill $ circle (V2 100 100) 100
  where
    gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
              ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
              ,(1, PixelRGBA8 0xFF 0x53 0x73 50)]
  
gradientRadial :: String -> PixelRGBA8 -> IO ()
gradientRadial name _back =
  produceImageAtSize 500 500 ("rad_opacity" ++ name ++ ".png") $
    withTexture (withSampler SamplerRepeat
                   (radialGradientTexture gradDef (V2 250 250) 100)) $
                          fill $ rectangle (V2 0 0) 500 500
  where
    gradDef = 
      [(0  , PixelRGBA8 255 165 0 102)
      ,(0.5, PixelRGBA8 255 165 0 102)
      ,(0.525, PixelRGBA8 255 165 0 255)
      ,(0.675, PixelRGBA8 128 128 128 64)
      ,(0.75, PixelRGBA8 0 128 128 255)
      ,(1, PixelRGBA8 0 128 128 255)
      ]

strokeBad2 :: IO ()
strokeBad2 =
    produceImageAtSize 70 70 ("bad_stroke_letter.png") $
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
  produceImageAtSize 500 500 "bad_stroke_tiger.png" $
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
  produceImageAtSize 289 89 "pledge_render.png" $ drawImage png 0 (V2 0 0)

shouldBeTheSame :: IO ()
shouldBeTheSame = do
  produceImageAtSize 400 200 "should_be_same_0.png" $ img prim1
  produceImageAtSize 400 200 "should_be_same_1.png" $ img prim2
  where
    drawColor = PixelRGBA8 0 0x86 0xc1 255
    prim1 = CubicBezier (V2  10  10) (V2 210 210)
                        (V2 210 210) (V2  10 410)
    prim2 = CubicBezier (V2  10  10) (V2 210 210)
                        (V2 210 210.1) (V2  10 410)

    img bez =
      withTexture (uniformTexture drawColor) $
        stroke 4 JoinRound (CapRound, CapRound) bez

clipFail :: IO ()
clipFail = 
  produceImageAtSize 512 256 "cubicbezier_clipError.png" $
    withTexture (uniformTexture red) $ fill geometry
  where
    trans = applyTransformation $ translate (V2 0 (-20))

    geometry = transform trans $
      [ CubicBezier (V2 104.707344 88.55418) (V2 153.00671 140.66666)
                    (V2 201.30609 192.77914) (V2 249.60547 244.89162)
      , CubicBezier (V2 249.60547 244.89162) (V2 349.59445 206.46687)
                    (V2 449.58347 168.04214) (V2 549.57245 129.6174)
      , CubicBezier (V2 549.57245 129.6174)  (V2 401.28406 115.92966)
                    (V2 252.99573 102.24192) (V2 104.707344 88.55418)
      ]

doubleCache :: IO ()
doubleCache = 
  produceImageAtSize 200 200  "double_opa.png" $ do
    withTexture (uniformTexture black) $
        fill $ rectangle (V2 0 95) 200 10
    withTexture (uniformTexture red) $
        withGroupOpacity 128 $ do
            withGroupOpacity 128 $
                fill $ circle (V2 70 100) 30
            fill $ circle (V2 120 100) 30
  
clipTestCaching :: IO ()
clipTestCaching = do
  let
    red = PixelRGBA8 255 0 0 255
    blue = PixelRGBA8 0 0 255 255
    rect =
      withTexture (uniformTexture red) $
        R.fill $ rectangle (V2 0 0) 100 100
    clippedRect =
      withClipping
        (R.fill $ rectangle (V2 0 0) 50 50)
        rect
    cachedRect =
      cacheDrawing 50 50 96 clippedRect

  writePng "rect.png" $
    renderDrawing 100 100 blue rect
  writePng "clippedRect.png" $
    renderDrawing 100 100 blue clippedRect
  writePng "cachedRect.png" $
    renderDrawing 100 100 blue cachedRect

strokeTranslation :: IO ()
strokeTranslation = do
    let white     = PixelRGBA8 255 255 255 255
        strokeTex = R.withTexture (uniformTexture (PixelRGBA8 0 0x86 0xc1 255))
        fillTex   = R.withTexture (uniformTexture (PixelRGBA8 0 0x86 0xc1 80))
        geom      = R.circle (V2 0 0) 90
        stroked   = strokeTex $ R.stroke 5.0 JoinRound (CapRound, CapRound) geom
        filled    = fillTex $ fill geom
        xform     = translate (V2 100 100)
    let img = renderDrawing 200 200 white $ do
            withTransformation xform filled
            withTransformation xform stroked
    writePng (outFolder </> "stroke-translate.png") img

badFilling :: IO ()
badFilling = produceImageAtSize 100 40  "bad_raster.png" $ do
  let path  =
        [ Line (V2 2.492557  32.80536)  (V2 2.469756 27.95916)
        , Line (V2 2.469756  27.95916)  (V2 2.461156 27.52476)
        , Line (V2 2.461156  27.52476)  (V2 2.480256 27.30636)
        , Line (V2 2.480256  27.30636)  (V2 2.480256 27.27716)
        , Line (V2 2.480256  27.27716)  (V2 2.480356 27.10316)
        , Line (V2 2.480356  27.10316)  (V2 2.4735565 26.78566)
        , Line (V2 2.4735565 26.78566)  (V2 2.484357 25.43696)
        , Line (V2 2.484357  25.43696)  (V2 2.477557 25.07886)
        , Line (V2 2.477557  25.07886)  (V2 2.476957 24.89426)
        , Line (V2 2.476957  24.89426)  (V2 2.474358 23.948456)
        , Line (V2 2.474358  23.948456) (V2 2.470958 23.33875)
        , Line (V2 2.470958  23.33875)  (V2 2.4672575 22.68765)
        , Line (V2 2.4672575 22.68765)  (V2 2.465958 22.45995)
        , Line (V2 2.465958  22.45995)  (V2 2.468058 21.999954)
        , Line (V2 2.468058  21.999954) (V2 2.457858 10.366356)
        , Line (V2 2.457858  10.366356) (V2 100 10.366356)
        , Line (V2 100 10.366356)      (V2 2.492557 32.80536)
        ]
  withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $ R.fill path


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
  strokeTranslation 
  badFilling 
  createDirectoryIfMissing True outFolder
  doubleCache
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
  compositionTransparentGradient 
  compositionClip 
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
  strokeWidthTest stroke
  strokePixelTest stroke

  strokeQuadraticIntersection stroke uniform ""
  strokeQuadraticIntersection stroke triGradient "gradient_"
  strokeQuadraticIntersection stroke radBiGradient "rad_gradient_"

  strokeTest2 stroke ""

  strokeCubic stroke uniform ""
  strokeCubic stroke bigBiGradient "gradient_"
  strokeCubic stroke radTriGradient "rad_gradient_"

  strokeCubicDashed dashedStroke uniform ""
  strokeCubicDashed2 dashedStroke uniform ""
  shouldBeTheSame

  let testText =
        "Test of a text! It seems to be; à é è ç, working? () {} [] \" '"

  textAlignStringTest monospaceFont "alignedConsola.png" testText
  textAlignStringTest sansSerifFont "alignedArial.png"
        "Just a simple test, gogo !!! Yay ; quoi ?"
  textStrokeTest sansSerifFont "stroke_verdana.png" "e"
  clipTestCaching
  florida
  omitted 
  -- badCircle

florida :: IO ()
florida = go where
  points :: [(Float, Float)]
  points = 
    [(-82.478063,27.92768), (-82.489817,27.9196), (-82.491117,27.9145), (-82.487417,27.895001), (-82.48142,27.889097), (-82.481763,27.878098), (-82.487883,27.872072),
    (-82.488057,27.863566), (-82.480137,27.853246), (-82.471624,27.847342), (-82.46884,27.843295), (-82.47244,27.822559), (-82.475273,27.820991), (-82.489849,27.822607),
    (-82.511193,27.828015), (-82.553946,27.848462), (-82.552918,27.862702), (-82.538618,27.864901), (-82.533218,27.870701), (-82.529918,27.877501), (-82.539318,27.885001),
    (-82.542818,27.890601), (-82.541747,27.893706), (-82.535818,27.898), (-82.531318,27.9039), (-82.533718,27.932999), (-82.541218,27.948998), (-82.553918,27.966998),
    (-82.576003,27.969424), (-82.60865,27.983245), (-82.62959,27.998474), (-82.641487,27.999426), (-82.664806,27.997046), (-82.678606,27.993715), (-82.682414,27.987053),
    (-82.684793,27.971824), (-82.700134,27.960307), (-82.716522,27.958398), (-82.720522,27.955798), (-82.724122,27.948098), (-82.720122,27.936399), (-82.710022,27.928299),
    (-82.691621,27.924899), (-82.685121,27.916299), (-82.671221,27.913), (-82.628063,27.910397), (-82.63422,27.9037), (-82.63212,27.8911), (-82.61002,27.873501),
    (-82.598443,27.857582), (-82.594819,27.843402), (-82.589319,27.835702), (-82.586519,27.816703), (-82.60742,27.798904), (-82.622723,27.779868), (-82.63052,27.753905),
    (-82.62502,27.732706), (-82.62572,27.727006), (-82.63362,27.710607), (-82.63982,27.703907), (-82.652521,27.700307), (-82.662921,27.702307), (-82.671621,27.705907),
    (-82.677321,27.706207), (-82.679251,27.694665), (-82.693748,27.700217), (-82.713629,27.698661), (-82.718822,27.692007), (-82.723022,27.671208), (-82.721622,27.663908),
    (-82.716322,27.651409), (-82.712555,27.646647), (-82.698091,27.638858), (-82.705017,27.62531), (-82.733076,27.612972), (-82.736552,27.617326), (-82.739122,27.636909),
    (-82.738022,27.706807), (-82.740323,27.718206), (-82.746223,27.731306), (-82.753723,27.736306), (-82.760923,27.745205), (-82.770023,27.767904), (-82.783124,27.783804),
    (-82.790224,27.791603), (-82.820433,27.813742), (-82.828561,27.822254), (-82.846526,27.854301), (-82.849126,27.8632), (-82.851126,27.8863), (-82.847826,27.910199),
    (-82.840882,27.937162), (-82.831388,27.962117), (-82.824875,27.960201), (-82.821975,27.956868), (-82.830819,27.930926), (-82.838484,27.909111), (-82.832155,27.909242),
    (-82.820715,27.927268), (-82.805462,27.960201), (-82.792635,28.01116), (-82.792635,28.032307), (-82.782724,28.055894), (-82.783824,28.106292), (-82.781324,28.127591),
    (-82.786624,28.144991), (-82.790724,28.15249), (-82.799024,28.15179), (-82.808474,28.154803), (-82.805097,28.172181), (-82.797762,28.187789), (-82.762643,28.219013),
    (-82.76446,28.220069), (-82.764103,28.244345), (-82.759072,28.25402), (-82.746188,28.261192), (-82.732792,28.291933), (-82.735463,28.30039), (-82.73146,28.325075),
    (-82.715822,28.345501), (-82.706112,28.368057), (-82.706322,28.401325), (-82.697433,28.420166), (-82.684137,28.428019), (-82.677839,28.434367), (-82.674787,28.441956),
    (-82.680396,28.457194), (-82.67241,28.464746), (-82.665055,28.484434), (-82.66447,28.488788), (-82.66639,28.49733), (-82.670146,28.500769), (-82.669416,28.519879),
    (-82.66804,28.528199), (-82.663705,28.530193), (-82.656694,28.544814), (-82.661729,28.549743), (-82.66165,28.554143), (-82.65705,28.568028), (-82.654138,28.590837),
    (-82.664055,28.606584), (-82.656649,28.623727), (-82.671815,28.627604), (-82.675596,28.656475), (-82.668034,28.683285), (-82.668722,28.695658), (-82.712373,28.720921),
    (-82.698281,28.75701), (-82.696906,28.768009), (-82.70103,28.797224), (-82.716497,28.810285), (-82.730245,28.850155), (-82.688657,28.89518), (-82.688864,28.905609),
    (-82.702618,28.932955), (-82.708793,28.935979), (-82.723861,28.953506), (-82.735754,28.973709), (-82.737872,28.995703), (-82.760551,28.993087), (-82.764055,28.999707),
    (-82.759378,29.006619), (-82.753513,29.026496), (-82.759704,29.054192), (-82.783328,29.064619), (-82.780558,29.07358), (-82.816925,29.076215), (-82.823659,29.098902),
    (-82.809483,29.10462), (-82.801166,29.105103), (-82.799117,29.110647), (-82.798876,29.114504), (-82.805703,29.129848), (-82.804736,29.146624), (-82.827073,29.158425),
    (-82.858179,29.162275), (-82.887211,29.161741), (-82.922613,29.169769), (-82.932405,29.167891), (-82.945302,29.167821), (-82.974676,29.17091), (-82.979522,29.171817),
    (-82.987162,29.180094), (-82.991653,29.180664), (-82.996144,29.178074), (-83.018212,29.151417), (-83.019071,29.141324), (-83.030453,29.134023), (-83.053207,29.130839),
    (-83.056867,29.146263), (-83.068249,29.153135), (-83.060947,29.170959), (-83.061162,29.176113), (-83.065242,29.184489), (-83.078986,29.196944), (-83.087839,29.21642),
    (-83.074734,29.247975), (-83.077265,29.255331), (-83.089013,29.266502), (-83.107477,29.268889), (-83.125567,29.278845), (-83.128027,29.282733), (-83.146445,29.289194),
    (-83.149764,29.289768), (-83.16073,29.286611), (-83.169576,29.290355), (-83.176736,29.31422), (-83.17826,29.327916), (-83.176852,29.329269), (-83.175518,29.34469),
    (-83.189581,29.363417), (-83.200702,29.373855), (-83.202446,29.394422), (-83.218075,29.420492), (-83.240509,29.433178), (-83.263965,29.435806), (-83.272019,29.432256),
    (-83.294747,29.437923), (-83.307094,29.459651), (-83.307828,29.468861), (-83.311546,29.475666), (-83.323214,29.476789), (-83.33113,29.475594), (-83.350067,29.489358),
    (-83.356722,29.499901), (-83.370288,29.499901), (-83.379254,29.503558), (-83.383973,29.512995), (-83.400252,29.517242), (-83.401552,29.523291), (-83.39983,29.533042),
    (-83.405256,29.578319), (-83.405068,29.59557), (-83.39948,29.612956), (-83.404081,29.640798), (-83.412768,29.668485), (-83.414701,29.670536), (-83.436259,29.677389),
    (-83.444635,29.677155), (-83.448194,29.675254), (-83.455356,29.676444), (-83.483143,29.690478), (-83.483567,29.698542), (-83.493728,29.708388), (-83.512716,29.71648),
    (-83.537645,29.72306), (-83.547172,29.732223), (-83.554993,29.7426), (-83.566018,29.761434), (-83.578955,29.768378), (-83.584716,29.77608), (-83.586089,29.784644),
    (-83.583045,29.787307), (-83.581903,29.792063), (-83.585899,29.811754), (-83.595493,29.827984), (-83.605244,29.836387), (-83.618568,29.842336), (-83.63798,29.886073),
    (-83.659951,29.899524), (-83.679219,29.918513), (-83.686423,29.923735), (-83.757249,29.957943), (-83.788729,29.976982), (-83.82869,29.983187), (-83.845427,29.998068),
    (-83.93151,30.039068), (-83.933668,30.041152), (-83.931879,30.044175), (-83.933432,30.046305), (-83.95968,30.064943), (-83.991607,30.08392), (-84.000716,30.096209),
    (-84.024274,30.103271), (-84.048715,30.103208), (-84.06299,30.101378), (-84.083057,30.092286), (-84.087034,30.092103), (-84.094725,30.094964), (-84.10273,30.093611),
    (-84.11384,30.085478), (-84.124889,30.090601), (-84.135683,30.083018), (-84.157278,30.072714), (-84.167881,30.071422), (-84.179149,30.073187), (-84.184493,30.077254),
    (-84.182217,30.082359), (-84.177405,30.085452), (-84.174999,30.095763), (-84.177062,30.096107), (-84.179811,30.091982), (-84.18806,30.094045), (-84.19356,30.097826),
    (-84.202152,30.112261), (-84.205589,30.114323), (-84.207652,30.106762), (-84.19853,30.087937), (-84.201585,30.087982), (-84.203349,30.085875), (-84.20801,30.084776),
    (-84.237014,30.08556), (-84.245668,30.093021), (-84.247491,30.10114), (-84.256439,30.103791), (-84.269363,30.09766), (-84.272511,30.092358), (-84.274003,30.083079),
    (-84.270368,30.075469), (-84.270792,30.068094), (-84.277168,30.060263), (-84.289727,30.057197), (-84.297836,30.057451), (-84.315344,30.069492), (-84.342022,30.063858),
    (-84.358923,30.058224), (-84.365882,30.024588), (-84.361962,29.987739), (-84.359986,29.984739), (-84.3477,29.984123), (-84.343041,29.9751), (-84.342046,29.967101),
    (-84.423335,29.983354), (-84.429865,29.982667), (-84.432958,29.981292), (-84.432271,29.972356), (-84.435708,29.964107), (-84.433646,29.962388), (-84.417816,29.9655),
    (-84.384133,29.961375), (-84.339426,29.946007), (-84.336511,29.942508), (-84.333746,29.923721), (-84.335953,29.912962), (-84.343389,29.899539), (-84.349066,29.896812),
    (-84.378937,29.893112), (-84.404958,29.901229), (-84.423834,29.902996), (-84.434287,29.906328), (-84.443652,29.913785), (-84.451705,29.929085), (-84.470323,29.924524),
    (-84.494562,29.913957), (-84.511996,29.916574), (-84.535873,29.910092), (-84.57744,29.887828), (-84.603303,29.876117), (-84.613154,29.867984), (-84.647958,29.847104),
    (-84.656318,29.837943), (-84.65645,29.834277), (-84.669728,29.82891), (-84.683934,29.831327), (-84.692053,29.829059), (-84.730327,29.8069), (-84.755595,29.78854),
    (-84.762998,29.788846), (-84.824197,29.758288), (-84.837168,29.755926), (-84.868271,29.742454), (-84.881777,29.733882), (-84.888031,29.722406), (-84.892493,29.72266),
    (-84.901781,29.735723), (-84.890066,29.755802), (-84.877111,29.772888), (-84.893992,29.785176), (-84.90413,29.786279), (-84.91511,29.783303), (-84.920917,29.772901),
    (-84.93837,29.750211), (-84.946595,29.745032), (-84.964007,29.742422), (-84.968841,29.72708), (-84.977004,29.721209), (-84.993264,29.714961), (-85.037212,29.711074),
    (-85.072123,29.719027), (-85.101682,29.718748), (-85.121473,29.715854), (-85.153238,29.708231), (-85.177284,29.700193), (-85.22745,29.693633), (-85.259719,29.681296),
    (-85.29074,29.684081), (-85.319215,29.681494), (-85.343619,29.672004), (-85.347711,29.66719), (-85.344768,29.654793), (-85.352615,29.659787), (-85.369419,29.681048),
    (-85.380303,29.698485), (-85.397871,29.740498), (-85.413983,29.799865), (-85.417971,29.828855), (-85.416548,29.842628), (-85.413575,29.85294), (-85.405815,29.865817),
    (-85.392469,29.870914), (-85.39874,29.859267), (-85.405011,29.830151), (-85.405907,29.80193), (-85.395528,29.762368), (-85.37796,29.709621), (-85.3638,29.693526),
    (-85.353885,29.684765), (-85.344986,29.685015), (-85.317661,29.691286), (-85.31139,29.697557), (-85.301331,29.797117), (-85.302591,29.808094), (-85.304877,29.811096),
    (-85.31142,29.814373), (-85.314547,29.822279), (-85.314783,29.830575), (-85.312911,29.832273), (-85.317464,29.838894), (-85.325008,29.844966), (-85.332289,29.845905),
    (-85.336654,29.849295), (-85.347044,29.871981), (-85.363731,29.898915), (-85.38473,29.920949), (-85.405052,29.938487), (-85.425956,29.949888), (-85.460488,29.959579),
    (-85.469425,29.957788), (-85.487764,29.961227), (-85.509148,29.971466), (-85.541176,29.995791), (-85.571907,30.02644), (-85.58139,30.037783), (-85.591048,30.048874),
    (-85.601178,30.056342), (-85.618254,30.065481), (-85.637285,30.073319), (-85.653251,30.077839), (-85.69681,30.09689), (-85.730054,30.118153), (-85.74993,30.136537),
    (-85.775405,30.15629), (-85.811219,30.17832), (-85.878138,30.215619), (-85.9226,30.238024), (-85.999937,30.27078), (-86.089963,30.303569), (-86.222561,30.343585),
    (-86.2987,30.363049), (-86.364175,30.374524), (-86.412076,30.380346), (-86.470849,30.3839), (-86.50615,30.3823), (-86.529067,30.386896), (-86.632953,30.396299),
    (-86.750906,30.391881), (-86.850625,30.380967), (-86.909679,30.372423), (-87.155392,30.327748), (-87.206254,30.320943), (-87.267827,30.31548), (-87.282787,30.318744),
    (-87.295422,30.323503), (-87.319518,30.317814), (-87.350486,30.313064), (-87.419859,30.297128), (-87.518324,30.280435), (-87.51838,30.283901), (-87.50548,30.287101),
    (-87.49998,30.287901), (-87.452378,30.300201), (-87.450078,30.3111), (-87.455578,30.3102), (-87.459578,30.3083), (-87.462978,30.3078), (-87.465778,30.3076),
    (-87.468678,30.3082), (-87.475879,30.3079), (-87.481879,30.306001), (-87.483679,30.304801), (-87.494879,30.305001), (-87.50278,30.307301), (-87.50468,30.308901),
    (-87.50578,30.3125), (-87.505943,30.319396), (-87.504701,30.324039), (-87.502572,30.327405), (-87.49998,30.328957), (-87.491879,30.3309), (-87.475579,30.3314),
    (-87.464878,30.3333), (-87.462978,30.334), (-87.459978,30.3363), (-87.452278,30.344099), (-87.450962,30.346262), (-87.451978,30.360299), (-87.451878,30.364999),
    (-87.451378,30.367199), (-87.449078,30.370399), (-87.441823,30.376304), (-87.438678,30.380798), (-87.438678,30.382098), (-87.440878,30.386698), (-87.441178,30.388598),
    (-87.440678,30.391498), (-87.437278,30.395898), (-87.434278,30.397498), (-87.431778,30.403198), (-87.429578,30.406498), (-87.426177,30.409198), (-87.422677,30.410098),
    (-87.419177,30.410198), (-87.413177,30.408998), (-87.408877,30.408798), (-87.403477,30.410198), (-87.401777,30.411398), (-87.398776,30.415098), (-87.395676,30.417597),
    (-87.386376,30.420497), (-87.382076,30.422897), (-87.371169,30.43049), (-87.368191,30.433407), (-87.366591,30.436648), (-87.366939,30.44048), (-87.36868,30.444631),
    (-87.370768,30.446865), (-87.381176,30.450097), (-87.391976,30.451597), (-87.396877,30.450597), (-87.399877,30.450997), (-87.404677,30.452897), (-87.407877,30.456396),
    (-87.414677,30.457296), (-87.425078,30.465596), (-87.429578,30.470596), (-87.430578,30.476596), (-87.431578,30.477696), (-87.434678,30.479196), (-87.435578,30.480496),
    (-87.432978,30.484896), (-87.430578,30.491096), (-87.431178,30.495795), (-87.438269,30.505357), (-87.43969,30.506649), (-87.44322,30.506782), (-87.444714,30.507494),
    (-87.447702,30.510458), (-87.447782,30.511913), (-87.447305,30.512629), (-87.446499,30.513569), (-87.445182,30.51398), (-87.444944,30.514943), (-87.446427,30.520306),
    (-87.446586,30.527068), (-87.43544,30.54914), (-87.434963,30.549599), (-87.431441,30.550263), (-87.427891,30.554159), (-87.426037,30.560073), (-87.423362,30.561425),
    (-87.422805,30.561379), (-87.422408,30.560439), (-87.420925,30.560668), (-87.418647,30.561837), (-87.41666,30.566306), (-87.416951,30.568003), (-87.418513,30.569561),
    (-87.418354,30.570043), (-87.416261,30.572448), (-87.414513,30.573456), (-87.412712,30.573227), (-87.408736,30.583701), (-87.406558,30.599928), (-87.404597,30.603389),
    (-87.401178,30.604397), (-87.39927,30.605611), (-87.397308,30.608728), (-87.395026,30.615281), (-87.395053,30.6159), (-87.39643,30.616909), (-87.39643,30.617734),
    (-87.395659,30.623372), (-87.394479,30.625192), (-87.393775,30.627006), (-87.393588,30.63088), (-87.395941,30.643968), (-87.397185,30.648117), (-87.396177,30.650454),
    (-87.397262,30.654351), (-87.400177,30.657217), (-87.400707,30.657148), (-87.405874,30.666616), (-87.407118,30.671796), (-87.406561,30.674019), (-87.406958,30.675165),
    (-87.412739,30.678055), (-87.419527,30.679981), (-87.424883,30.683374), (-87.430372,30.688645), (-87.436021,30.688668), (-87.439814,30.690479), (-87.44228,30.692679),
    (-87.44358,30.694604), (-87.449362,30.698913), (-87.451404,30.699806), (-87.456948,30.69756), (-87.466338,30.700835), (-87.467717,30.701683), (-87.470397,30.705281),
    (-87.474429,30.706586), (-87.479579,30.712865), (-87.479819,30.71495), (-87.481225,30.716508), (-87.487036,30.7185), (-87.496772,30.720353), (-87.497515,30.720123),
    (-87.502317,30.72159), (-87.505153,30.726313), (-87.511729,30.733535), (-87.523613,30.738306), (-87.532607,30.743489), (-87.535365,30.749775), (-87.535416,30.75476),
    (-87.536528,30.761383), (-87.537085,30.76253), (-87.54226,30.767504), (-87.54616,30.77202), (-87.545364,30.774105), (-87.545044,30.778666), (-87.552051,30.786254),
    (-87.552954,30.786941), (-87.554838,30.787125), (-87.559484,30.790447), (-87.560068,30.792258), (-87.564209,30.796246), (-87.56814,30.799088), (-87.572043,30.800532),
    (-87.576849,30.808163), (-87.581869,30.812403), (-87.58787,30.815037), (-87.594297,30.816984), (-87.600486,30.820627), (-87.60163,30.82514), (-87.60357,30.828624),
    (-87.605776,30.831304), (-87.610982,30.832632), (-87.615923,30.834693), (-87.615367,30.837031), (-87.617281,30.840353), (-87.624137,30.845713), (-87.626075,30.846494),
    (-87.627323,30.847961), (-87.626497,30.85188), (-87.62538,30.854355), (-87.626228,30.857127), (-87.628245,30.860131), (-87.634938,30.865886), (-87.629987,30.877686),
    (-87.629454,30.880115), (-87.6244,30.884696), (-87.622062,30.885408), (-87.620788,30.887494), (-87.620922,30.889923), (-87.622519,30.89368), (-87.622203,30.897508),
    (-87.620715,30.89893), (-87.616013,30.901453), (-87.614951,30.904226), (-87.614209,30.908536), (-87.611847,30.914541), (-87.6102,30.916628), (-87.608262,30.9219),
    (-87.607811,30.92449), (-87.602684,30.934277), (-87.600691,30.937074), (-87.598299,30.938793), (-87.59689,30.941131), (-87.592055,30.951492), (-87.589187,30.964464),
    (-87.590917,30.969414), (-87.593046,30.972966), (-87.594111,30.976335), (-87.594164,30.977572), (-87.592676,30.98014), (-87.593395,30.982959), (-87.596722,30.98761),
    (-87.599172,30.995722), (-87.598928,30.997457), (-87.571281,30.99787), (-87.548543,30.997927), (-87.51952,30.997586), (-87.480243,30.998202), (-87.479703,30.998197),
    (-87.478706,30.998213), (-87.466879,30.998178), (-87.466827,30.998178), (-87.461783,30.998201), (-87.461638,30.998202), (-87.458658,30.998386), (-87.455705,30.998318),
    (-87.449811,30.998272), (-87.432292,30.998205), (-87.425774,30.99809), (-87.367842,30.998292), (-87.364011,30.998218), (-87.355656,30.998244), (-87.333973,30.998272),
    (-87.312183,30.998435), (-87.30403,30.998191), (-87.301567,30.998434), (-87.290995,30.998352), (-87.288905,30.998345), (-87.265564,30.998267), (-87.26054,30.998195),
    (-87.259689,30.998172), (-87.25796,30.998263), (-87.257002,30.998194), (-87.255592,30.998216), (-87.25498,30.998285), (-87.237685,30.996393), (-87.224746,30.997169),
    (-87.162614,30.999055), (-87.140755,30.999532), (-87.124969,30.998802), (-87.118873,30.999427), (-87.068633,30.999143), (-87.064063,30.999191), (-87.053737,30.999131),
    (-87.039989,30.999594), (-87.036366,30.999348), (-87.027107,30.999255), (-87.004359,30.999316), (-86.998477,30.998661), (-86.92781,30.997704), (-86.888135,30.997577),
    (-86.872989,30.997631), (-86.831934,30.997378), (-86.830497,30.997401), (-86.785918,30.996978), (-86.74524,30.99629), (-86.728392,30.996739), (-86.727293,30.996882),
    (-86.725379,30.996872), (-86.706261,30.994703), (-86.678383,30.994537), (-86.664681,30.994534), (-86.567586,30.995109), (-86.563436,30.995223), (-86.519938,30.993245),
    (-86.512834,30.9937), (-86.49995,30.99334), (-86.458319,30.993998), (-86.454704,30.993791), (-86.404912,30.994049), (-86.391937,30.994172), (-86.388647,30.994181),
    (-86.374545,30.994474), (-86.36927,30.994477), (-86.364907,30.994455), (-86.304596,30.994029), (-86.289247,30.993798), (-86.256448,30.993853), (-86.238335,30.99437),
    (-86.180232,30.994005), (-86.175204,30.993798), (-86.168979,30.993706), (-86.162886,30.993682), (-86.116918,30.992917), (-86.056213,30.993133), (-86.052462,30.993247),
    (-86.035039,30.99332), (-85.998643,30.99287), (-85.893543,30.993467), (-85.749932,30.994837), (-85.749619,30.995292), (-85.498272,30.996928), (-85.243632,31.000884),
    (-85.154452,31.000835), (-85.152218,31.000834), (-85.152085,31.000888), (-85.145835,31.000695), (-85.057534,31.000585), (-85.054802,31.000585), (-85.052088,31.000585),
    (-85.031155,31.000647), (-85.030107,31.000653), (-85.027512,31.00067), (-85.024108,31.000681), (-85.002368,31.000682), (-85.0019,31.000681), (-85.001819,30.997889),
    (-85.00254,30.986899), (-85.005934,30.979804), (-85.005931,30.97704), (-85.005105,30.974704), (-85.004026,30.973468), (-84.999928,30.971186), (-84.999828,30.971486),
    (-84.997628,30.971186), (-84.988027,30.968786), (-84.984827,30.967486), (-84.982527,30.965586), (-84.980127,30.961286), (-84.979627,30.958986), (-84.979627,30.954686),
    (-84.982227,30.946886), (-84.983027,30.942586), (-84.983627,30.936986), (-84.983127,30.934786), (-84.980627,30.932687), (-84.975226,30.930787), (-84.971026,30.928187),
    (-84.969426,30.921987), (-84.966726,30.917287), (-84.956125,30.907587), (-84.952325,30.902287), (-84.949625,30.897387), (-84.942525,30.888488), (-84.941325,30.887688),
    (-84.939974,30.886728), (-84.938087,30.885627), (-84.936828,30.884683), (-84.935413,30.882481), (-84.93557,30.878707), (-84.937615,30.875876), (-84.938401,30.873045),
    (-84.937772,30.870528), (-84.935728,30.86754), (-84.934627,30.865495), (-84.933997,30.863293), (-84.934627,30.86062), (-84.935413,30.858418), (-84.935256,30.854328),
    (-84.933224,30.851488), (-84.930065,30.848824), (-84.928807,30.846779), (-84.928335,30.844263), (-84.928335,30.842532), (-84.929436,30.840331), (-84.931953,30.837499),
    (-84.934155,30.834039), (-84.935256,30.830894), (-84.93557,30.824603), (-84.936042,30.820671), (-84.935413,30.81721), (-84.930923,30.810489), (-84.928323,30.80509),
    (-84.927923,30.80279), (-84.929023,30.79729), (-84.928323,30.79309), (-84.926723,30.79019), (-84.918023,30.77809), (-84.917423,30.77589), (-84.918023,30.77209),
    (-84.920123,30.76599), (-84.915022,30.761191), (-84.914322,30.753591), (-84.913522,30.752291), (-84.911122,30.751191), (-84.906322,30.750591), (-84.903122,30.751791),
    (-84.900222,30.751891), (-84.897622,30.751391), (-84.896122,30.750591), (-84.887522,30.741791), (-84.885221,30.734991), (-84.883821,30.732591), (-84.875421,30.727491),
    (-84.869752,30.721897), (-84.864693,30.711542), (-84.836324,30.710709), (-84.644815,30.701992), (-84.606386,30.699865), (-84.606249,30.699872), (-84.53937,30.696775),
    (-84.535042,30.696523), (-84.474409,30.692793), (-84.374905,30.689794), (-84.28121,30.685256), (-84.2499,30.684145), (-84.124895,30.678046), (-84.107699,30.676818),
    (-84.057228,30.674705), (-84.046605,30.6742), (-84.04181,30.673878), (-84.039707,30.673819), (-84.007391,30.672097), (-83.880317,30.665807), (-83.88022,30.665832),
    (-83.855216,30.664412), (-83.820886,30.662612), (-83.810536,30.66188), (-83.676773,30.654905), (-83.674058,30.654747), (-83.611667,30.651255), (-83.499876,30.645671),
    (-83.448895,30.64241), (-83.440021,30.642023), (-83.429584,30.641496), (-83.429477,30.641519), (-83.390062,30.639333), (-83.37946,30.63868), (-83.341011,30.636346),
    (-83.311647,30.634577), (-83.309455,30.634417), (-83.187391,30.627223), (-83.174411,30.626444), (-83.163309,30.625895), (-83.15617,30.625504), (-83.13137,30.623583),
    (-82.878779,30.609082), (-82.877259,30.609024), (-82.698902,30.598271), (-82.698618,30.598232), (-82.689271,30.597719), (-82.569237,30.590965), (-82.565476,30.590622),
    (-82.553159,30.589934), (-82.545055,30.589361), (-82.536233,30.588885), (-82.524899,30.588189), (-82.459544,30.584272), (-82.374844,30.579004), (-82.287343,30.573458),
    (-82.2581,30.571559), (-82.249841,30.570863), (-82.214839,30.568591), (-82.214385,30.566958), (-82.218579,30.564403), (-82.223025,30.56321), (-82.227254,30.561041),
    (-82.231916,30.55627), (-82.235603,30.544885), (-82.23582,30.537187), (-82.234952,30.533066), (-82.230752,30.526758), (-82.229399,30.520823), (-82.230377,30.517339),
    (-82.226933,30.510281), (-82.225026,30.50783), (-82.218514,30.504187), (-82.212852,30.498751), (-82.206445,30.491877), (-82.201416,30.485164), (-82.200938,30.474438),
    (-82.204614,30.468868), (-82.207708,30.460503), (-82.207522,30.456928), (-82.20604,30.455507), (-82.204823,30.45184), (-82.203975,30.444507), (-82.206486,30.437081),
    (-82.20987,30.432818), (-82.210291,30.42459), (-82.204151,30.40133), (-82.19294,30.378779), (-82.189847,30.375938), (-82.183797,30.373712), (-82.180018,30.368625),
    (-82.171508,30.359869), (-82.170054,30.358929), (-82.165192,30.358035), (-82.161757,30.357851), (-82.158109,30.359913), (-82.143282,30.363393), (-82.116385,30.367335),
    (-82.104834,30.368319), (-82.1025,30.367823), (-82.101416,30.366556), (-82.101798,30.365336), (-82.094687,30.360781), (-82.081106,30.358806), (-82.068533,30.359184),
    (-82.060034,30.360328), (-82.050069,30.362338), (-82.047917,30.363265), (-82.040746,30.370158), (-82.036825,30.377884), (-82.035871,30.385287), (-82.041164,30.396841),
    (-82.04199,30.403266), (-82.041164,30.409966), (-82.039971,30.41428), (-82.034005,30.422357), (-82.034464,30.428048), (-82.037209,30.434518), (-82.036203,30.43846),
    (-82.030064,30.444853), (-82.028212,30.447396), (-82.025457,30.457755), (-82.023734,30.467289), (-82.017779,30.475081), (-82.016982,30.478779), (-82.017297,30.487638),
    (-82.018222,30.492085), (-82.015892,30.495499), (-82.01477,30.513009), (-82.015826,30.518166), (-82.01699,30.519358), (-82.018868,30.523828), (-82.018361,30.531184),
    (-82.013216,30.550091), (-82.005477,30.563495), (-82.008091,30.577018), (-82.012109,30.593773), (-82.015708,30.601704), (-82.016503,30.602484), (-82.026941,30.606153),
    (-82.027338,30.606726), (-82.026541,30.613303), (-82.028499,30.621829), (-82.033927,30.629603), (-82.037609,30.633271), (-82.039941,30.637144), (-82.039092,30.641132),
    (-82.039595,30.643309), (-82.042271,30.649452), (-82.046114,30.651767), (-82.049507,30.655548), (-82.050432,30.676266), (-82.041812,30.692376), (-82.036426,30.706585),
    (-82.037563,30.71864), (-82.039154,30.723178), (-82.04101,30.72508), (-82.043795,30.729641), (-82.041168,30.734248), (-82.040026,30.737548), (-82.039634,30.747727),
    (-82.038967,30.749262), (-82.035964,30.750998), (-82.032645,30.750674), (-82.0284,30.750981), (-82.017917,30.755263), (-82.01266,30.761289), (-82.011597,30.763122),
    (-82.017881,30.775844), (-82.024035,30.783156), (-82.023848,30.786685), (-82.022866,30.787991), (-82.017051,30.791657), (-82.007865,30.792937), (-82.004973,30.791744),
    (-81.994972,30.786073), (-81.990855,30.781611), (-81.988605,30.780056), (-81.981273,30.776767), (-81.979061,30.776415), (-81.973856,30.778487), (-81.962534,30.796526),
    (-81.961989,30.800443), (-81.962441,30.808441), (-81.962739,30.813636), (-81.962175,30.818001), (-81.959759,30.821168), (-81.949787,30.827493), (-81.943168,30.827434),
    (-81.938381,30.825745), (-81.935444,30.821131), (-81.934655,30.820424), (-81.924448,30.817566), (-81.910926,30.815889), (-81.906279,30.817015), (-81.903745,30.818986),
    (-81.902337,30.820817), (-81.89938,30.821662), (-81.89572,30.821098), (-81.892904,30.819268), (-81.891281,30.815945), (-81.882725,30.805124), (-81.876882,30.799516),
    (-81.868608,30.792754), (-81.852626,30.794439), (-81.846286,30.790548), (-81.842058,30.78712), (-81.840375,30.786384), (-81.827014,30.788933), (-81.808529,30.790014),
    (-81.806652,30.789683), (-81.792769,30.784432), (-81.78435,30.77359), (-81.782653,30.769937), (-81.779171,30.768062), (-81.775021,30.76833), (-81.772611,30.769535),
    (-81.770468,30.772481), (-81.768192,30.773954), (-81.763372,30.77382), (-81.759338,30.771377), (-81.755074,30.768319), (-81.751283,30.767082), (-81.747572,30.766455),
    (-81.746312,30.765891), (-81.745035,30.765039), (-81.744183,30.763868), (-81.743438,30.762271), (-81.743094,30.759912), (-81.742736,30.759201), (-81.732227,30.749634),
    (-81.727127,30.746934), (-81.719927,30.744634), (-81.694778,30.748414), (-81.692815,30.7471), (-81.691818,30.74399), (-81.69099,30.742841), (-81.688925,30.741434),
    (-81.672824,30.738935), (-81.670124,30.740235), (-81.669324,30.741335), (-81.668275,30.744643), (-81.667336,30.74566), (-81.664598,30.746599), (-81.662173,30.746521),
    (-81.656541,30.745113), (-81.652123,30.742435), (-81.651723,30.740235), (-81.652161,30.735648), (-81.65177,30.732284), (-81.65044,30.729703), (-81.649188,30.728686),
    (-81.646137,30.727591), (-81.633266,30.729603), (-81.629609,30.732407), (-81.625098,30.733017), (-81.621929,30.731188), (-81.620822,30.729535), (-81.619613,30.724849),
    (-81.617663,30.722046), (-81.609495,30.720705), (-81.607667,30.721924), (-81.605716,30.725337), (-81.60401,30.727287), (-81.601206,30.728141), (-81.593648,30.725459),
    (-81.58682,30.723735), (-81.573719,30.722336), (-81.571419,30.721636), (-81.566219,30.717836), (-81.561706,30.715597), (-81.552566,30.716974), (-81.549186,30.715972),
    (-81.546932,30.714345), (-81.544679,30.713969), (-81.542675,30.713593), (-81.540923,30.713343), (-81.539295,30.713468), (-81.537668,30.714345), (-81.535539,30.716348),
    (-81.534517,30.717936), (-81.534037,30.719853), (-81.532785,30.721606), (-81.530531,30.722858), (-81.528278,30.723359), (-81.521417,30.722536), (-81.516116,30.722236),
    (-81.507216,30.722936), (-81.489537,30.7261), (-81.487332,30.726081), (-81.483786,30.723891), (-81.475754,30.714754), (-81.472597,30.713312), (-81.464465,30.711045),
    (-81.459978,30.710434), (-81.448718,30.709353), (-81.444124,30.709714), (-81.432725,30.703017), (-81.42742,30.69802), (-81.430843,30.669393), (-81.443099,30.600938),
    (-81.442564,30.555189), (-81.434064,30.522569), (-81.447087,30.503679), (-81.440108,30.497678), (-81.42601,30.496739), (-81.410809,30.482039), (-81.407008,30.42204),
    (-81.397422,30.400626), (-81.39736,30.396967), (-81.389789,30.397422), (-81.397067,30.379511), (-81.396407,30.34004), (-81.391606,30.303441), (-81.385505,30.273841),
    (-81.355591,30.162563), (-81.308978,29.96944), (-81.295268,29.928614), (-81.288955,29.91518), (-81.27654,29.90046), (-81.270442,29.883106), (-81.264693,29.858212),
    (-81.263396,29.820663), (-81.256711,29.784693), (-81.240924,29.739218), (-81.229015,29.714693), (-81.211565,29.667085), (-81.163581,29.55529), (-81.123896,29.474465),
    (-81.046678,29.307856), (-80.966176,29.14796), (-80.944376,29.110861), (-80.907275,29.064262), (-80.893675,29.036163), (-80.878275,29.010563), (-80.787021,28.875266),
    (-80.709725,28.756692), (-80.647288,28.677875), (-80.61679,28.634561), (-80.583884,28.597705), (-80.574868,28.585166), (-80.567361,28.562353), (-80.560973,28.530736),
    (-80.536115,28.478647), (-80.525094,28.459454), (-80.526732,28.451705), (-80.562877,28.437779), (-80.574136,28.427764), (-80.587813,28.410856), (-80.596174,28.390682),
    (-80.603374,28.363983), (-80.606874,28.336484), (-80.608074,28.311285), (-80.604214,28.257733), (-80.589975,28.17799), (-80.566432,28.09563), (-80.547675,28.048795),
    (-80.508871,27.970477), (-80.446973,27.861954), (-80.447179,27.859731), (-80.383695,27.740045), (-80.351717,27.642623), (-80.350553,27.628361), (-80.34437,27.616226),
    (-80.330956,27.597541), (-80.324699,27.569178), (-80.311757,27.524625), (-80.30117,27.500314), (-80.293171,27.500314), (-80.265535,27.420542), (-80.253665,27.37979),
    (-80.233538,27.341307), (-80.226753,27.322736), (-80.19309,27.249546), (-80.16147,27.192814), (-80.153375,27.169308), (-80.159554,27.163325), (-80.14982,27.143557),
    (-80.138605,27.111517), (-80.116772,27.072397), (-80.093909,27.018587), (-80.046263,26.859238), (-80.031362,26.796339), (-80.03212,26.77153), (-80.036362,26.77124),
    (-80.037462,26.76634), (-80.032862,26.715242), (-80.032862,26.700842), (-80.035763,26.676043), (-80.035363,26.612346), (-80.038863,26.569347), (-80.050363,26.509549),
    (-80.060564,26.444652), (-80.070564,26.336455), (-80.072264,26.335356), (-80.075264,26.318656), (-80.079865,26.264358), (-80.085565,26.249259), (-80.089365,26.231859),
    (-80.101366,26.147762), (-80.105266,26.096264), (-80.109566,26.087165), (-80.117778,25.986369), (-80.117904,25.915772), (-80.12087,25.883152), (-80.119684,25.841043),
    (-80.122056,25.817913), (-80.127394,25.791224), (-80.127987,25.772245), (-80.137476,25.750301), (-80.144,25.740812), (-80.152896,25.702855), (-80.154082,25.683283),
    (-80.152303,25.676759), (-80.154972,25.66549), (-80.160903,25.664897), (-80.176916,25.685062), (-80.170392,25.710565), (-80.164461,25.721833), (-80.166241,25.72895),
    (-80.172765,25.737847), (-80.184626,25.745557), (-80.197674,25.74437), (-80.229107,25.732509), (-80.240376,25.724206), (-80.244528,25.717089), (-80.250459,25.688028),
    (-80.265879,25.658373), (-80.267065,25.651849), (-80.277147,25.637022), (-80.288416,25.630498), (-80.296719,25.622195), (-80.301464,25.613299), (-80.305615,25.593134),
    (-80.305615,25.575342), (-80.302057,25.567632), (-80.313918,25.539164), (-80.324594,25.535605), (-80.328746,25.53264), (-80.339421,25.499427), (-80.339421,25.478669),
    (-80.337049,25.465621), (-80.328152,25.443084), (-80.320442,25.437153), (-80.326373,25.422919), (-80.32578,25.39801), (-80.320442,25.391486), (-80.31036,25.389707),
    (-80.306801,25.384369), (-80.31036,25.3731), (-80.335269,25.338701), (-80.352469,25.329805), (-80.361662,25.327433), (-80.374116,25.31735), (-80.383013,25.301337),
    (-80.37969,25.288463), (-80.393438,25.271965), (-80.394469,25.253061), (-80.412686,25.248593), (-80.418872,25.235532), (-80.432621,25.235876), (-80.442588,25.24275),
    (-80.462833,25.236247), (-80.467824,25.23254), (-80.469842,25.230443), (-80.481136,25.226046), (-80.4887,25.226936), (-80.49315,25.225157), (-80.49315,25.218482),
    (-80.485865,25.211281), (-80.488035,25.206942), (-80.495341,25.199463), (-80.498644,25.20015), (-80.508113,25.206719), (-80.512928,25.216719), (-80.520359,25.220788),
    (-80.52319,25.22008), (-80.530207,25.216207), (-80.53464,25.211252), (-80.535197,25.207915), (-80.532416,25.19846), (-80.536309,25.197348), (-80.554107,25.209027),
    (-80.556888,25.207915), (-80.556888,25.197348), (-80.563006,25.191786), (-80.569124,25.190117), (-80.584771,25.200665), (-80.609609,25.181901), (-80.619024,25.177328),
    (-80.624185,25.183443), (-80.605275,25.190674), (-80.605275,25.195123), (-80.610837,25.19846), (-80.645876,25.189005), (-80.655331,25.184556), (-80.659224,25.17955),
    (-80.654219,25.177881), (-80.63364,25.181775), (-80.63399,25.176829), (-80.640275,25.17662), (-80.645822,25.174029), (-80.649251,25.168708), (-80.648657,25.157859),
    (-80.651994,25.151185), (-80.650326,25.147292), (-80.650326,25.144511), (-80.669236,25.137837), (-80.67591,25.13728), (-80.677578,25.142286), (-80.661449,25.155078),
    (-80.655887,25.161752), (-80.656942,25.168216), (-80.67985,25.166751), (-80.678273,25.172042), (-80.681611,25.173293), (-80.697462,25.162865), (-80.695037,25.157588),
    (-80.70127,25.146683), (-80.703718,25.139115), (-80.709141,25.144511), (-80.712896,25.151185), (-80.717901,25.154522), (-80.723324,25.152019), (-80.719153,25.148682),
    (-80.721885,25.145101), (-80.742877,25.142646), (-80.755991,25.150038), (-80.756642,25.155904), (-80.764464,25.159163), (-80.768374,25.157859), (-80.765767,25.148082),
    (-80.769678,25.137002), (-80.777499,25.135047), (-80.780758,25.139609), (-80.775544,25.149386), (-80.782062,25.161118), (-80.791186,25.16177), (-80.795097,25.157207),
    (-80.795097,25.144824), (-80.798356,25.140913), (-80.802266,25.140913), (-80.804874,25.1546), (-80.796218,25.17245), (-80.798356,25.179367), (-80.807481,25.181975),
    (-80.809436,25.178064), (-80.806177,25.17285), (-80.815183,25.164959), (-80.82653,25.160478), (-80.830034,25.168094), (-80.838227,25.174791), (-80.846395,25.17706),
    (-80.858167,25.176576), (-80.87546,25.174321), (-80.900066,25.162034), (-80.901617,25.153803), (-80.898911,25.147652), (-80.900577,25.139669), (-80.915924,25.141301),
    (-80.943216,25.134443), (-80.957427,25.135704), (-80.971765,25.133958), (-80.999176,25.124222), (-81.009598,25.125403), (-81.022989,25.129393), (-81.050505,25.128273),
    (-81.079859,25.118797), (-81.094524,25.127054), (-81.111943,25.14547), (-81.120616,25.152302), (-81.133567,25.156295), (-81.141024,25.163868), (-81.142278,25.183),
    (-81.146737,25.193139), (-81.155481,25.208098), (-81.172044,25.222276), (-81.170907,25.245857), (-81.168307,25.253178), (-81.16207,25.289833), (-81.159293,25.298595),
    (-81.1523,25.305543), (-81.148915,25.318067), (-81.151916,25.324766), (-81.148103,25.332793), (-81.140099,25.341117), (-81.133913,25.342996), (-81.12141,25.33875),
    (-81.118208,25.34522), (-81.117265,25.354953), (-81.128492,25.380511), (-81.141395,25.381358), (-81.150508,25.387255), (-81.150656,25.399206), (-81.147144,25.404297),
    (-81.146765,25.407577), (-81.168652,25.463848), (-81.179406,25.475427), (-81.191924,25.484745), (-81.208201,25.504937), (-81.210149,25.516888), (-81.203175,25.53416),
    (-81.204389,25.538908), (-81.209321,25.548611), (-81.225557,25.55847), (-81.232705,25.573366), (-81.233051,25.586587), (-81.240519,25.599041), (-81.240677,25.613629),
    (-81.253951,25.638181), (-81.268924,25.656927), (-81.277374,25.66498), (-81.290328,25.687506), (-81.328935,25.717233), (-81.335037,25.715649), (-81.346078,25.721473),
    (-81.345972,25.736536), (-81.343984,25.747668), (-81.346767,25.754029), (-81.355116,25.76039), (-81.359489,25.766354), (-81.361875,25.772715), (-81.344779,25.782257),
    (-81.340406,25.786631), (-81.341598,25.794582), (-81.349152,25.816847), (-81.352731,25.822015), (-81.362272,25.824401), (-81.386127,25.839906), (-81.394476,25.851834),
    (-81.417536,25.864954), (-81.424295,25.867737), (-81.429066,25.865351), (-81.441391,25.863761), (-81.458487,25.868929), (-81.471607,25.881652), (-81.473992,25.888411),
    (-81.48751,25.888411), (-81.501027,25.884037), (-81.508979,25.884037), (-81.512955,25.886423), (-81.511762,25.89676), (-81.515738,25.899941), (-81.527665,25.901531),
    (-81.541183,25.900338), (-81.577363,25.889206), (-81.584519,25.888808), (-81.614735,25.893977), (-81.623482,25.897158), (-81.644553,25.897953), (-81.654493,25.893579),
    (-81.663821,25.885605), (-81.672633,25.856654), (-81.678287,25.845301), (-81.6848,25.847205), (-81.68954,25.85271), (-81.713172,25.897568), (-81.717687,25.902039),
    (-81.727086,25.907207), (-81.73195,25.931506), (-81.738118,25.942009), (-81.745579,25.949643), (-81.749724,25.960463), (-81.747834,25.994273), (-81.750668,25.998425),
    (-81.757463,26.000374), (-81.762439,26.00607), (-81.801663,26.088227), (-81.808833,26.152246), (-81.81461,26.173167), (-81.81681,26.207166), (-81.820675,26.236735),
    (-81.833142,26.294518), (-81.844555,26.327712), (-81.868983,26.378648), (-81.90191,26.410859), (-81.90271,26.416159), (-81.91171,26.427158), (-81.923611,26.436658),
    (-81.938411,26.445058), (-81.956611,26.452358), (-81.964212,26.457957), (-81.967112,26.462857), (-81.966212,26.465057), (-81.969509,26.476505), (-81.980712,26.480957),
    (-81.997012,26.484856), (-82.008961,26.484052), (-82.01368,26.490829), (-82.00908,26.505203), (-82.024604,26.512677), (-82.043577,26.519577), (-82.06715,26.513252),
    (-82.07175,26.492554), (-82.094748,26.48393), (-82.105672,26.48393), (-82.111996,26.54085), (-82.118896,26.560973), (-82.122345,26.579371), (-82.137869,26.637441),
    (-82.149943,26.654115), (-82.181565,26.681712), (-82.17984,26.696661), (-82.173516,26.701836), (-82.151668,26.704136), (-82.139019,26.702986), (-82.125795,26.699536),
    (-82.118896,26.690912), (-82.106247,26.667339), (-82.099922,26.662739), (-82.093023,26.665614), (-82.086698,26.685162), (-82.084974,26.702411), (-82.079799,26.716784),
    (-82.066575,26.742657), (-82.061401,26.774279), (-82.061401,26.789228), (-82.055076,26.802452), (-82.057951,26.822), (-82.058526,26.838674), (-82.056801,26.858797),
    (-82.059101,26.876621), (-82.066575,26.88237), (-82.090723,26.888694), (-82.093023,26.906518), (-82.090148,26.923191), (-82.083249,26.927791), (-82.067725,26.927791),
    (-82.061976,26.931241), (-82.061401,26.938715), (-82.063126,26.950214), (-82.076349,26.958263), (-82.107972,26.957688), (-82.117171,26.954239), (-82.124645,26.945615),
    (-82.137294,26.926066), (-82.162017,26.925491), (-82.169491,26.923191), (-82.175241,26.916867), (-82.172941,26.897319), (-82.156267,26.851898), (-82.147068,26.789803),
    (-82.151093,26.783479), (-82.172941,26.778879), (-82.17869,26.772555), (-82.221812,26.77198), (-82.233311,26.784054), (-82.241935,26.774279), (-82.251134,26.755881),
    (-82.259867,26.717398), (-82.263804,26.725644), (-82.264682,26.756836), (-82.269499,26.784674), (-82.289086,26.827784), (-82.301736,26.841588), (-82.351649,26.908384),
    (-82.400618,26.984937), (-82.419218,27.020736), (-82.445718,27.060634), (-82.460319,27.099933), (-82.465319,27.110732), (-82.46889,27.113612), (-82.477019,27.141231),
    (-82.512319,27.207528), (-82.539719,27.254326), (-82.54512,27.261026), (-82.55902,27.268826), (-82.569754,27.279452), (-82.569248,27.298588), (-82.57602,27.309324),
    (-82.597629,27.335754), (-82.623863,27.362206), (-82.642821,27.38972), (-82.675121,27.424318), (-82.691821,27.437218), (-82.691004,27.444331), (-82.707821,27.487615),
    (-82.714521,27.500415), (-82.724522,27.513614), (-82.743017,27.531086), (-82.745748,27.538834), (-82.742437,27.53936), (-82.708121,27.523514), (-82.710621,27.501715),
    (-82.706821,27.498415), (-82.690421,27.496415), (-82.686421,27.497215), (-82.686921,27.508015), (-82.683621,27.513115), (-82.674621,27.519614), (-82.66202,27.522814),
    (-82.65072,27.523115), (-82.646014,27.53354), (-82.632053,27.551908), (-82.612019,27.571231), (-82.613003,27.582837), (-82.611717,27.585283), (-82.596488,27.594045),
    (-82.584629,27.596021), (-82.570607,27.608882), (-82.565667,27.615713), (-82.558538,27.638678), (-82.537146,27.672933), (-82.514265,27.705588), (-82.494891,27.718963),
    (-82.482449,27.719886), (-82.477638,27.723004), (-82.476297,27.729929), (-82.477129,27.735216), (-82.482305,27.742649), (-82.478339,27.74625), (-82.457543,27.752571),
    (-82.434635,27.764355), (-82.43198,27.768092), (-82.433981,27.774087), (-82.419066,27.793767), (-82.418401,27.803187), (-82.410837,27.810868), (-82.402857,27.812671),
    (-82.393383,27.837519), (-82.397463,27.851631), (-82.402615,27.882602), (-82.413915,27.901401), (-82.432316,27.901301), (-82.451591,27.907506), (-82.453731,27.908546),
    (-82.461914,27.908431), (-82.462459,27.920248), (-82.461055,27.938161), (-82.478063,27.92768)]

  toTransform = scale 40 40 <>  translate (V2 88 (-25))
  path = transform (applyTransformation toTransform) $ Path
    { _pathOriginPoint = uncurry V2 $ head points
    , _pathClose = True
    , _pathCommand = PathLineTo . uncurry V2 <$> points
    }

  greyN n = PixelRGBA8 n n n 255

  go = do
    putStrLn "Producing florida"
    writePng (outFolder </> "florida.png") $
      renderDrawing 350 250 (greyN 200) $
        withTexture (uniformTexture blue) $
          -- R.fill path
          -- {-
          R.stroke
            1
            (JoinMiter 0)
            (CapStraight 0, CapStraight 0)
            path -- -}

{-
benchTest :: [String] -> IO ()
benchTest _args = do
  defaultMainWith defaultConfig
        [bench "testsuite" $ nfIO testSuite,
         bench "Triangles" $ nfIO Sample.triangles]
         -- -}

omitted :: IO ()
omitted = do
    writePng (outFolder </> "IssueSample2.png") $ go 300
  where
    (imgx, imgy) = (1280.0, 720.0)
    greyN n = PixelRGBA8 n n n 255
    go a =
      renderDrawing (floor imgx) (floor imgy) (greyN 200) $
        withTransformation (translate (V2 (imgx/2) (imgy/2))) $
            withTexture (uniformTexture (greyN 100)) $ R.fill . toPrimitives $ 
                Path (V2 0 0) True [PathCubicBezierCurveTo (V2 a a) (V2 a (-a)) (V2 0 0)]


badCircle :: IO ()
badCircle = do
  putStrLn "Bad Circle"
  print $ circle (V2 0 0) 1e50
  let drawColor = PixelRGBA8 0 0x86 0xc1 255
      img = renderDrawing 400 200 white $
         withTexture (uniformTexture drawColor) $ do
            fill $ circle (V2 0 0) 1e50 -- overflows to Infinity :: Float -> boom
  writePng (outFolder </> "bug.png") img

main :: IO ()
main = do
    args <- getArgs
    case args of
         "random":_ -> randomTests
         -- "bench":rest -> benchTest rest
         "prof":_ -> Sample.triangles
         _ -> testSuite

