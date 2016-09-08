{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( foldMap )
import Control.Applicative( (<$>) )
#endif

import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

import Control.Monad.ST( ST, runST )
import Data.Monoid( (<>) )
import Graphics.Rasterific hiding ( fill
                                  , dashedStrokeWithOffset
                                  , dashedStroke
                                  , fillWithMethod, stroke)
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear( (^+^), (^-^), (^*) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Patch

import qualified Data.ByteString.Lazy as LB
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


background, blue, black, yellow, red, green, orange, white :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255
red = PixelRGBA8 255 0 0 255
green =  PixelRGBA8 0 255 0 255
black = PixelRGBA8 0 0 0 255
{-grey = PixelRGBA8 128 128 128 255-}
orange = PixelRGBA8 255 0xA5 0 255
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
  
drawImm :: FilePath -> Int -> Int -> (forall s. DrawContext (ST s) PixelRGBA8 ()) -> IO ()
drawImm path w h d = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white d

drawPatchDebug :: FilePath -> Int -> Int -> CoonPatch PixelRGBA8 -> IO ()
drawPatchDebug path w h p = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white $ do
    renderCoonPatch p
    mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ debugDrawCoonPatch defaultDebug p

drawTensorDebug :: FilePath -> Int -> Int -> TensorPatch PixelRGBA8 -> IO ()
drawTensorDebug path w h p = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white $ do
    renderTensorPatch p
    mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ (debugDrawTensorPatch defaultDebug) p


coonTest :: IO ()
coonTest = do
  draw "coon_outline.png" $ drawCoonPatchOutline patch
  drawImm "coon_render_small.png" 200 200 $ renderCoonPatch $ patch'
  drawImm "coon_render.png" 800 800 $ renderCoonPatch $ patch
  drawPatchDebug "coon_render_debug.png" 800 800 patch
  where
    draw path p = do
      putStrLn $ "Rendering " ++ path
      writePng path . renderDrawing 800 800 (PixelRGBA8 255 255 255 255) $
          withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) p

    cc a b c d e f = PathCubicBezierCurveTo (V2 a b) (V2 c d) (V2 e f)
    [CubicBezierPrim c1, CubicBezierPrim c2, CubicBezierPrim c3, CubicBezierPrim c4] =
        toPrimitives . transform (\p -> (p ^+^ (V2 0 (-852.36))) * 4) $ Path (V2 13.21 869.2) False
          [cc 49.67 838.5 145.1 878.4 178.2 (880.7 :: Float)
          ,cc 193.9 944.6 109.2 950.4 167.5 1021
          ,cc 117.7 1025 73.48 1043 18.21 1033
          ,cc 2.253 974.9 13.98 923.5 13.21 869.2
          ]
    [CubicBezierPrim c1', CubicBezierPrim c2', CubicBezierPrim c3', CubicBezierPrim c4'] =
        toPrimitives . transform (\p -> (p ^+^ (V2 0 (-852.36)))) $ Path (V2 13.21 869.2) False
          [cc 49.67 838.5 145.1 878.4 178.2 (880.7 :: Float)
          ,cc 193.9 944.6 109.2 950.4 167.5 1021
          ,cc 117.7 1025 73.48 1043 18.21 1033
          ,cc 2.253 974.9 13.98 923.5 13.21 869.2
          ]

    patch = CoonPatch c1 c2 c3 c4 
              (ParametricValues (PixelRGBA8 255 0 0 255)
                                (PixelRGBA8 0 255 0 255)
                                (PixelRGBA8 0 0 255 255)
                                (PixelRGBA8 255 255 0 255))

    patch' = CoonPatch c1' c2' c3' c4'
              (ParametricValues (PixelRGBA8 255 0 0 255)
                                (PixelRGBA8 0 255 0 255)
                                (PixelRGBA8 0 0 255 255)
                                (PixelRGBA8 255 255 0 255))

coonTensorTest :: IO ()
coonTensorTest = do
  drawImm "compare_tensor.png" 800 800 $ renderTensorPatch $ tensorPatch
  drawTensorDebug "compare_tensor_debug.png" 900 800 tensorPatch
  drawImm "compare_coon.png" 800 800 $ renderCoonPatch $ coonPatch
  where
    [ c00, c01, c02, c03
      , c10, c11, c12, c13
      , c20, c21, c22, c23
      , c30, c31, c32, c33
      ] = fmap (\p -> (p ^+^ (V2 0 (-852.36))) * 4)
       {- [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
          ,(V2 13.98 923.5), (V2 90 950),     (V2 117 950),      (V2 193.9 944.6)
          ,(V2 2.253 974.9), (V2 90 1000),    (V2 117 1000),     (V2 109.2 950.4)
          ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
          ] -}
          [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
          ,(V2 13.98 923.5), (V2 140 990),     (V2 147 1000),      (V2 193.9 944.6)
          ,(V2 2.253 974.9), (V2 140 1000),    (V2 147 1005),     (V2 109.2 950.4)
          ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
          ]
    coonPatch = CoonPatch
        { _north = CubicBezier c00 c01 c02 c03
        , _east  = CubicBezier c03 c13 c23 c33
        , _south = CubicBezier c33 c32 c31 c30
        , _west  = CubicBezier c30 c20 c10 c00
        , _coonValues = colors
        }


    tensorPatch = TensorPatch
      { _curve0 = CubicBezier c00 c01 c02 c03
      , _curve1 = CubicBezier c10 c11 c12 c13
      , _curve2 = CubicBezier c20 c21 c22 c23
      , _curve3 = CubicBezier c30 c31 c32 c33
      , _tensorValues = colors
      }

    colors = ParametricValues blue red red red

tensorSplit :: IO ()
tensorSplit = do
  drawImm "split_tensor_orig.png" 800 800 $ renderTensorPatch $ tensorPatch
  drawTensorDebug "split_tensor_orig_debug.png" 800 800 tensorPatch
  drawTensorSubdivDebug "split_tensor_orig_subH.png" 800 800 tensorPatch [patchWest, patchEast]
  drawTensorSubdivDebug "split_tensor_orig_subHVR.png" 800 800 tensorPatch [patchWest, patchNorthEast, patchSouthEast]
  where
    drawTensorSubdivDebug path w h p ps = do
        putStrLn $ "Rendering " ++ path
        writePng path $ runST $ runDrawContext w h white $ do
            renderTensorPatch p
            mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ mapM_ (debugDrawTensorPatch defaultDebug) ps

    [ c00, c01, c02, c03
     , c10, c11, c12, c13
     , c20, c21, c22, c23
     , c30, c31, c32, c33
     ] = fmap (\p -> (p ^+^ (V2 30 (-802.36))) * 3)
        [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
        ,(V2 13.98 923.5), (V2 120 950),     (V2 147 950),      (V2 193.9 944.6)
        ,(V2 2.253 974.9), (V2 120 1000),    (V2 147 1000),     (V2 220.2 950.4)
        ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
        ]

    tensorPatch = TensorPatch
      { _curve0 = CubicBezier c00 c01 c02 c03
      , _curve1 = CubicBezier c10 c11 c12 c13
      , _curve2 = CubicBezier c20 c21 c22 c23
      , _curve3 = CubicBezier c30 c31 c32 c33
      , _tensorValues = colors
      }

    (patchWest, patchEast) = horizontalTensorSubdivide tensorPatch { _tensorValues = parametricBase }
    (patchNorthEast, patchSouthEast) = horizontalTensorSubdivide $ transposePatch patchEast
    frontColor = PixelRGBA8 0 0x86 0xc1 255
    accentColor = PixelRGBA8 0xff 0xf4 0xc1 255
    accent2Color = PixelRGBA8 0xFF 0x53 0x73 255

    colors = ParametricValues frontColor accentColor accent2Color frontColor

coonTestColorStop :: IO ()
coonTestColorStop = do
  drawImm "coon_render_color.png" 800 800 $ renderCoonPatch patch
  drawPatchDebug "coon_render_color_debug.png" 800 800 patch
  where
    cc a b c d e f = PathCubicBezierCurveTo (V2 a b) (V2 c d) (V2 e f)
    [CubicBezierPrim c1, CubicBezierPrim c2, CubicBezierPrim c3, CubicBezierPrim c4] =
        toPrimitives . transform (\p -> (p ^+^ (V2 0 (-852.36))) * 4) $ Path (V2 13.21 869.2) False
          [cc 49.67 838.5 145.1 878.4 178.2 (880.7 :: Float)
          ,cc 193.9 944.6 109.2 950.4 167.5 1021
          ,cc 117.7 1025 73.48 1043 18.21 1033
          ,cc 2.253 974.9 13.98 923.5 13.21 869.2
          ]
    patch = CoonPatch c1 c2 c3 c4 
              (ParametricValues (PixelRGBA8 255 20 0 255)
                          red
                          red
                          red)

toCoon :: V2 Float -> ParametricValues px -> [[V2 Float]] -> CoonPatch px
toCoon st values = build . go st where
  build [n, e, s, w] = CoonPatch n e s w values
  build _ = error "toCoon"

  go _ [] = []
  go p [lst] = case toAbsolute p lst of
    [c1, c2] -> [CubicBezier p c1 c2 st]
    _ -> error "Mouh"
  go p (x : xs) = case toAbsolute p x of
    [c1, c2, c3] -> CubicBezier p c1 c2 c3 : go c3 xs
    _ -> error "Mouh"

  toAbsolute p = fmap (p ^+^)


coonTestWild :: IO ()
coonTestWild = do
  drawImm "coon_render_wild.png" 800 800 $ renderCoonPatch patch
  drawPatchDebug "coon_render_wild_debug.png" 800 800 patch
  where
    patch = toCoon (V2 50 130 ^* 2)
        (ParametricValues red yellow orange green) $
        fmap (^* 2) <$>
        [ [V2 150  0, V2 300 (-100), V2 120 (-100)]
        , [V2 0  100, V2  40   200 , V2  40  220]
        , [V2 (-250) (-100), V2 50 60, V2 (-160) 0]
        , [V2 (-20) (-80), V2 20 (-40)]
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

  textAlignStringTest monospaceFont "alignedConsola.png" testText
  textAlignStringTest sansSerifFont "alignedArial.png"
        "Just a simple test, gogo !!! Yay ; quoi ?"
  textStrokeTest sansSerifFont "stroke_verdana.png" "e"

  coonTest
  coonTestColorStop 
  coonTestWild 
  coonTensorTest
  tensorSplit 

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

