import Control.Applicative( (<$>) )
import Graphics.Rasterific
import Codec.Picture
import Linear( V2( .. ), (^+^) )

pathize :: [V2 Float] -> [Bezier]
pathize (a:b:rest@(c:_)) = Bezier a b c : pathize rest
pathize _ = []

logo :: Int -> Bool -> V2 Float -> [Bezier]
logo size inv offset = pathize . way $ map (^+^ offset)
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

logoTest :: IO ()
logoTest = writePng "logo.png" img
  where texture = uniformTexture black
        beziers = logo 40 False $ V2 10 10
        inverse = logo 20 True $ V2 20 20
        drawing = fillBezierShape texture $ beziers ++ inverse
        white = (PixelRGBA8 255 255 255 0)
        black = (PixelRGBA8 0 120 250 255)
        img = renderContext 100 100 white drawing

clipTest :: IO ()
clipTest = writePng "clip.png" img
  where texture = uniformTexture black
        beziers =
            [ logo 20 False $ V2 (-10) (-10)
            , logo 20 False $ V2 80 80
            , logo 20 False $ V2 0 80
            , logo 20 False $ V2 80 0
            ]

        drawing = mapM_ (fillBezierShape texture) beziers
        white = (PixelRGBA8 255 255 255 0)
        black = (PixelRGBA8 0 120 250 255)
        img = renderContext 100 100 white drawing

strokeTest :: IO ()
strokeTest = writePng "stroke.png" img
  where texture = uniformTexture black
        beziers base = take 1 <$>
            take 3 [ logo 100 False $ V2 ix ix | ix <- [base, base + 20 ..] ]
        drawing = sequence_ . concat $
          [ []
          , [strokeBezierShape texture (6 + ix) ix ix b
                    | (ix, b) <- zip [1 ..] (beziers 10)]
          , [strokeBezierShape texture ix 1 1 b
                    | (ix, b) <- zip [1 ..] (beziers 60)]
          , [strokeBezierShape texture ix 1 (-1) b
                    | (ix, b) <- zip [1 ..] (beziers 110)]
          , [strokeBezierShape texture 15 1 0 . take 1 $
                    logo 150 False $ V2 200 200]
          , [strokeBezierShape texture 5 1 0 $
                    logo 100 False $ V2 240 240]
          ]
            
        white = (PixelRGBA8 255 255 255 0)
        black = (PixelRGBA8 0 120 250 255)
        img = renderContext 500 500 white drawing

main :: IO ()
main = do
  logoTest
  clipTest
  strokeTest

