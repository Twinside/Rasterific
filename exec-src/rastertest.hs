import Graphics.Rasterific
import Codec.Picture
import Linear( V2( .. ), (^+^) )

pathize :: [V2 Float] -> [Bezier]
pathize (a:b:rest@(c:_)) = Bezier a b c : pathize rest
pathize _ = []

circle :: Int -> V2 Float -> [Bezier]
circle size offset = pathize $ map (^+^ offset)
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

circleTest :: IO ()
circleTest = do
    mapM_ print beziers
    putStrLn "========================"
    {-let clipped = circle 20 >>= clipBezier (V2 0 0) (V2 100 100)-}
    {-mapM_ print clipped-}
    putStrLn "========================"
    {-let decomposed = clipped >>= decomposeBeziers-}
    {-mapM_ print $ decomposed-}
    putStrLn "========================"
    writePng "circle.png" img
  where texture = uniformTexture black
        beziers = circle 20 $ V2 10 10
        drawing = fillBezierShape texture beziers
        white = (PixelRGBA8 255 255 255 255)
        black = (PixelRGBA8 0 0 16 255)
        img = renderContext 100 100 white drawing

main :: IO ()
main = circleTest

