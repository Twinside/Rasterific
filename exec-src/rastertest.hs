import Control.Applicative( (<$>) )
import Graphics.Rasterific
import Graphics.Rasterific.Polygon
import Graphics.Rasterific.Types
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.CubicBezier
import Codec.Picture
import Linear( V2( .. ), (^+^), (^*) )

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


background, blue :: PixelRGBA8
background = (PixelRGBA8 255 255 255 0)
blue = (PixelRGBA8 0 120 250 255)

logoTest :: IO ()
logoTest = writePng "logo.png" img
  where texture = uniformTexture blue
        beziers = logo 40 False $ V2 10 10
        inverse = logo 20 True $ V2 20 20
        drawing = fill texture $ beziers ++ inverse
        img = renderContext 100 100 background drawing

cubicTest :: [CubicBezier]
cubicTest = cubicBezierFromPath 
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
cubicTest1 = writePng "cubic1.png" img
  where texture = uniformTexture blue
        drawing = fill texture cubicTest
        img = renderContext 150 150 background drawing

clipTest :: IO ()
clipTest = writePng "clip.png" img
  where texture = uniformTexture blue
        beziers =
            [ logo 20 False $ V2 (-10) (-10)
            , logo 20 False $ V2 80 80
            , logo 20 False $ V2 0 80
            , logo 20 False $ V2 80 0
            ]

        drawing = mapM_ (fill texture) beziers
        img = renderContext 100 100 background drawing

strokeTest2 :: IO ()
strokeTest2 = writePng "stroke2.png" img
  where texture = uniformTexture blue
        points = 
            [ V2 10 10, V2 100 100
            , V2 200 20, V2 300 100, V2 450 50]
        
        drawing = sequence_ . concat $
            [ []
            , [stroke texture 9 JoinRound (CapRound, CapStraight 0)
                    . polygonFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [-5 .. -1] ]
            , [stroke texture 9 (JoinMiter $ ix * 3) (CapStraight 0, CapRound)
                . polygonFromPath $
                (^+^ (V2 15 (20 * (ix + 5)))) <$> points
                    | ix <- [0 .. 5] ]
            {-, [strokePolygonShape texture 8 5 (CapRound ) $-}
                {-(^+^ (V2 15 (20 * (ix + 20)))) <$> points-}
                {-| ix <- [-10 .. 0]]-}
            ]

        img = renderContext 500 500 background drawing

strokeTest :: IO ()
strokeTest = writePng "stroke.png" img
  where texture = uniformTexture blue
        beziers base = take 1 <$>
            take 3 [ logo 100 False $ V2 ix ix | ix <- [base, base + 20 ..] ]
        drawing = sequence_ . concat $
          [ []
          , [stroke texture (6 + ix) (JoinMiter ix)
                    (CapStraight 0, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 10)]
          , [stroke texture ix
                    (JoinMiter 1) (CapRound, CapStraight 1) b
                    | (ix, b) <- zip [1 ..] (beziers 60)]
          , [stroke texture ix (JoinMiter 1) (CapRound, CapRound) b
                    | (ix, b) <- zip [1 ..] (beziers 110)]
          , [stroke texture 15
                    (JoinMiter 1) (CapStraight 1, CapStraight 0)
                    . take 1 $
                    logo 150 False $ V2 200 200]
          , [stroke texture 5
                    (JoinMiter 1) (CapStraight 0, CapStraight 0) $
                    logo 100 False $ V2 240 240]
          ]
        img = renderContext 500 500 background drawing

main :: IO ()
main = do
  logoTest
  cubicTest1
  clipTest
  strokeTest
  strokeTest2

