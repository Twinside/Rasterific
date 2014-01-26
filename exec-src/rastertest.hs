{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import System.FilePath( (</>) )
import System.Directory( createDirectoryIfMissing )

import Control.Applicative( (<$>) )
import Graphics.Rasterific
import Graphics.Rasterific.Line
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Codec.Picture
import Linear( V2( .. ), (^+^), (^*) )

type Stroker s =
    Texture PixelRGBA8 -> Float -> Join -> (Cap, Cap) -> [Primitive]
        -> DrawContext s PixelRGBA8 ()

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


background, blue, black, grey, yellow :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255
black = PixelRGBA8 0 0 0 255
grey = PixelRGBA8 128 128 128 255
yellow = PixelRGBA8 255 255 0 255
brightblue = PixelRGBA8 0 255 255 255

logoTest :: IO ()
logoTest = writePng (outFolder </> "logo.png") img
  where texture = uniformTexture blue
        beziers = logo 40 False $ V2 10 10
        inverse = logo 20 True $ V2 20 20
        drawing = fill texture $ beziers ++ inverse
        img = renderContext 100 100 background drawing

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
        drawing = fill texture cubicTest
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

        drawing = mapM_ (fill texture) beziers
        img = renderContext 100 100 background drawing

strokeTest2 :: (forall s. Stroker s) -> String -> IO ()
strokeTest2 stroker prefix =
    writePng (outFolder </> (prefix ++ "stroke2.png")) img
  where texture = uniformTexture blue
        points = 
            [ V2 10 10, V2 100 100
            , V2 200 20, V2 300 100, V2 450 50]
        
        drawing = sequence_ . concat $
            [ []
            , [stroker texture 9 JoinRound (CapRound, CapStraight 0)
                    . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroker texture 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . map LinePrim . lineFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            ]

        img = renderContext 500 500 background drawing

strokeLogo :: (forall s. Stroker s) -> String -> IO ()
strokeLogo stroker prefix =
  writePng (outFolder </> (prefix ++ "stroke_logo.png")) img
    where texture = uniformTexture blue
          beziers = logo 40 False $ V2 10 10
          inverse = logo 20 True $ V2 20 20
          img = renderContext 100 100 background 
              $ stroker texture 4 JoinRound (CapRound, CapRound)
              $ beziers ++ inverse

strokeQuadraticIntersection :: (forall s. Stroker s) -> String -> IO ()
strokeQuadraticIntersection stroker prefix =
  writePng (outFolder </> (prefix ++ "stroke_quad_intersection.png")) img
    where texture = uniformTexture blue
          img = renderContext 500 500 background 
              $ stroker texture 40 JoinRound (CapRound, CapRound)
              $ map BezierPrim
              $ bezierFromPath
                [ V2 30 30
                , V2 150 200
                , V2 450 450

                , V2 450 90
                , V2 30  450
                ]

strokeCubic :: (forall s. Stroker s) -> String -> IO ()
strokeCubic stroker prefix =
    writePng (outFolder </> (prefix ++ "cubicStroke.png")) img
  where texture = uniformTexture blue
        img = renderContext 500 500 background drawing
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

        drawing = sequence_ . concat $
            [ []
            , [stroker texture 4 JoinRound (CapRound, CapRound)
                    $ take 1 cubicTest ]

            , [stroker texture 15 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim cusp]]

            , [stroker texture 25 (JoinMiter 0)
                    (CapStraight 0, CapStraight 0)
                    [CubicBezierPrim loop]]
            ]

strokeTest :: (forall s. Stroker s) -> String -> IO ()
strokeTest stroker prefix =
    writePng (outFolder </> (prefix ++ "stroke.png")) img
  where texture = uniformTexture blue
        beziers base = take 1 <$>
            take 3 [ logo 100 False $ V2 ix ix | ix <- [base, base + 20 ..] ]
        drawing = sequence_ . concat $
          [ []
          , [stroker texture (6 + ix) (JoinMiter ix)
                    (CapStraight 0, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 10)]
          , [stroker texture ix
                    (JoinMiter 1) (CapRound, CapStraight 1) b
                    | (ix, b) <- zip [1 ..] (beziers 60)]
          , [stroker texture ix (JoinMiter 1) (CapRound, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 110)]
          , [stroker texture 15
                    (JoinMiter 1) (CapStraight 1, CapStraight 0)
                    . take 1 $
                    logo 150 False $ V2 200 200]
          , [stroker texture 5
                    (JoinMiter 1) (CapStraight 0, CapStraight 0) $
                   logo 100 False $ V2 240 240]
          ]
        img = renderContext 500 500 background drawing

debugStroke :: Stroker s
debugStroke =
    strokeDebug (uniformTexture brightblue) (uniformTexture yellow)

main :: IO ()
main = do
  createDirectoryIfMissing True outFolder
  logoTest
  cubicTest1
  clipTest
  strokeTest stroke ""
  strokeTest debugStroke "debug_"

  strokeQuadraticIntersection stroke ""
  strokeQuadraticIntersection debugStroke "debug_"

  strokeTest2 stroke ""
  strokeTest2 debugStroke "debug_"

  strokeLogo debugStroke "debug_"

  strokeCubic stroke ""
  strokeCubic debugStroke "debug_"

