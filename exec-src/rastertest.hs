{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )
import Data.Word( Word8 )

import Control.Applicative( (<$>), liftA2 )
import Control.Monad( forM_ )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType

import Data.Binary( decodeFile )

import qualified Data.Vector.Unboxed as VU
import Codec.Picture
import Linear( V2( .. )
             , (^+^)
             {-, (^*)-}
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


background, blue, black, brightblue, yellow, red, white :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255
red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
{-grey = PixelRGBA8 128 128 128 255-}
yellow = PixelRGBA8 255 255 0 255
brightblue = PixelRGBA8 0 255 255 255
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
    img = renderContext 100 100 background drawing

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
    img = renderContext 400 400 background drawing

circleTest :: Texture PixelRGBA8 -> String -> IO ()
circleTest texture prefix =
    writePng (outFolder </> (prefix ++ "circle.png")) img
  where 
    drawing = withTexture texture . fill $ circle (V2 100 100) 90
    img = renderContext 200 200 background drawing

cubicTest :: [Primitive]
cubicTest = map CubicBezierPrim $ cubicBezierFromPath 
    [ V2 50 20 -- zig zag first part
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
        img = renderContext 150 150 background drawing

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
        img = renderContext 100 100 background drawing

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

        img = renderContext 500 500 background drawing

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
          withTexture (uniformTexture $ PixelRGBA8 255 128 100 64)
                    . fill $ circle (V2 150 150) 40

        img = renderContext 500 500 background drawing

strokeLogo :: Stroker -> String -> IO ()
strokeLogo stroker prefix =
  writePng (outFolder </> (prefix ++ "stroke_logo.png")) img
    where texture = uniformTexture blue
          beziers = logo 40 False $ V2 10 10
          inverse = logo 20 True $ V2 20 20
          img = renderContext 100 100 background 
              . withTexture texture
              . stroker 4 JoinRound (CapRound, CapRound)
              $ beziers ++ inverse

strokeQuadraticIntersection ::
    Stroker -> Texture PixelRGBA8 -> String -> IO ()
strokeQuadraticIntersection stroker texture prefix =
  writePng (outFolder </> (prefix ++ "stroke_quad_intersection.png")) img
    where img = renderContext 500 500 background 
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
  where img = renderContext 500 500 background drawing
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
  where img = renderContext 500 500 background drawing
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

textTest :: String -> IO ()
textTest fontName = do
    putStrLn $ "Rendering " ++ fontName
    font <- decodeFile $ "C:/Windows/Fonts/" ++ fontName ++ ".ttf"
    writePng (outFolder </> ("charArray_" ++ fontName ++ ".png")) $ renderer $ img font
  where
    backColor = white
    strokeColor = uniformTexture black
    sizePerChar = 50
    xCount = 20
    yCount = 20
    renderer = 
        renderContext (xCount * sizePerChar) (yCount * sizePerChar) backColor
            . withTexture strokeColor
    (^*^) = liftA2 (*)

    indices = 
        [(x,y, y * xCount + x) | y <- [0 .. yCount - 1], x <- [0 .. xCount - 1] ]

    img font = forM_ indices $ \(xp, yp, charId) -> do
      let curves = getGlyphIndexCurvesAtPointSize font 90 36 charId
          boxSize = V2 (fromIntegral sizePerChar) (fromIntegral sizePerChar)
          vector = (V2 (fromIntegral xp) (fromIntegral yp) ^*^ boxSize) ^+^ V2 10 10
          beziers = concat
              [map BezierPrim . bezierFromPath 
                              . map (\(x,y) -> V2 x y ^+^ vector)
                              $ VU.toList c | c <- curves]
      fill $ beziers

textStringTest :: String -> String -> String -> IO ()
textStringTest fontName filename txt = do
    putStrLn $ "Rendering " ++ fontName
    font <- decodeFile $ "C:/Windows/Fonts/" ++ fontName ++ ".ttf"
    writePng (outFolder </> filename) $ renderer $ img font
  where
    backColor = white
    strokeColor = uniformTexture black
    sizePerChar = 50
    xCount = 20
    yCount = 4
    renderer = 
        renderContext (xCount * sizePerChar) (yCount * sizePerChar) backColor
            . withTexture strokeColor
    (^*^) = liftA2 (*)

    indices = 
        [(x, y, c)
            | ((x,y), c) <- zip [(x,y) | y <- [0 .. yCount - 1], x <- [0 .. xCount - 1]] txt
            ]

    img font = forM_ indices $ \(xp, yp, charId) -> do
      let curves = getCharCurvesAtPointSize font 90 36 charId
          boxSize = V2 (fromIntegral sizePerChar) (fromIntegral sizePerChar)
          vector = (V2 (fromIntegral xp) (fromIntegral yp) ^*^ boxSize) ^+^ V2 10 10
          beziers = concat
              [map BezierPrim . bezierFromPath 
                              . map (\(x,y) -> V2 x y ^+^ vector)
                              $ VU.toList c | c <- curves]
      fill $ beziers


textSizeTest :: String -> IO ()
textSizeTest fontName = do
    putStrLn $ "Rendering " ++ fontName
    font <- decodeFile $ "C:/Windows/Fonts/" ++ fontName ++ ".ttf"
    writePng (outFolder </> ("charArray_" ++ fontName ++ "_size.png")) $ renderer $ img font
  where
    backColor = 255 :: Word8
    strokeColor = uniformTexture 0
    sizePerChar = 50
    xCount = 20
    yCount = 20
    renderer = 
        renderContext (xCount * sizePerChar) (yCount * sizePerChar) backColor
    (^*^) = liftA2 (*)

    indices = 
        [(x,y, 1 * xCount + x) | y <- [0 .. yCount - 1], x <- [0 .. xCount - 1] ]

    img font = forM_ indices $ \(xp, yp, charId) -> do
      let curves = getGlyphIndexCurvesAtPointSize font 90 (yp * 3) charId
          boxSize = V2 (fromIntegral sizePerChar) (fromIntegral sizePerChar)
          vector = (V2 (fromIntegral xp) (fromIntegral yp) ^*^ boxSize) ^+^ V2 10 10
          beziers = concat
              [map BezierPrim . bezierFromPath 
                              . map (\(x,y) -> V2 x y ^+^ vector)
                              $ VU.toList c | c <- curves]
      withTexture strokeColor $ fill beziers

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
        img = renderContext 500 500 background drawing

debugStroke :: Stroker
debugStroke =
    strokeDebug (uniformTexture brightblue) (uniformTexture yellow)


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
        radialGradientWithFocusTexture triColor (V2 200 200) 70 (V2 230 200)

  createDirectoryIfMissing True outFolder
  logoTest uniform ""
  logoTest biGradient "gradient_"

  bigBox uniform ""
  bigBox biGradient "gradient_"
  bigBox bigBiGradient "gradient_big_"
  bigBox triGradient "gradient_tri_"
  bigBox radBiGradient "rad_gradient_"
  bigBox radTriGradient "rad_trigradient_"
  bigBox radFocusTriGradient "rad_focus_trigradient_"

  circleTest uniform ""
  strokeTestCliping stroke ""

  cubicTest1
  clipTest
  strokeTest stroke uniform ""
  strokeTest stroke bigBiGradient "gradient_"
  strokeTest stroke radTriGradient "rad_gradient_"
  strokeTest debugStroke uniform "debug_"

  strokeQuadraticIntersection stroke uniform ""
  strokeQuadraticIntersection stroke triGradient "gradient_"
  strokeQuadraticIntersection stroke radBiGradient "rad_gradient_"
  strokeQuadraticIntersection debugStroke uniform "debug_"

  strokeTest2 stroke ""
  strokeTest2 debugStroke "debug_"

  strokeLogo debugStroke "debug_"

  strokeCubic stroke uniform ""
  strokeCubic stroke bigBiGradient "gradient_"
  strokeCubic stroke radTriGradient "rad_gradient_"
  strokeCubic debugStroke uniform "debug_"

  strokeCubicDashed dashedStroke uniform ""
  textTest "consola"
  textSizeTest "consola"
  textTest "arial"
  textSizeTest "arial"
  textTest "comic"
  textSizeTest "comic"
  textTest "verdana"
  textTest "webdings"
  textTest "webdings"
  textTest "wingding"
  textTest "WINGDNG2"
  textTest "WINGDNG3"
  textTest "PAPYRUS"
  textTest "COLONNA"
  textTest "BRUSHSCI"
  textTest "BROADW"
  textTest "cour"
  textTest "courbd"

  let testText =  "Test of a text! It seems to be; à é è ç, working? () {} [] \" '"
  textStringTest "consola" "basicTestConsolas.png" testText
  textStringTest "comic" "basicTestComic.png" testText
  textStringTest "arial" "basicTestArial.png" testText

