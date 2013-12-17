import Graphics.Rasterific
import Codec.Picture
import Linear( V2( .. ), (^+^) )

pathize :: [V2 Float] -> [Bezier]
pathize (a:b:rest@(c:_)) = Bezier a b c : pathize rest
pathize _ = []

circle :: Int -> Bool -> V2 Float -> [Bezier]
circle size inv offset = pathize . way $ map (^+^ offset)
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

circleTest :: IO ()
circleTest = writePng "circle.png" img
  where texture = uniformTexture black
        beziers = circle 40 False $ V2 10 10
        inverse = circle 20 True $ V2 20 20
        drawing :: DrawContext s () PixelRGBA8
        drawing = fillBezierShape texture $ beziers ++ inverse
        white = (PixelRGBA8 255 255 255 0)
        black = (PixelRGBA8 0 120 250 255)
        img = renderContext 100 100 white drawing

main :: IO ()
main = circleTest

