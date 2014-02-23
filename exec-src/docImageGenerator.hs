
import Control.Applicative( (<$>) )
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Directory( createDirectoryIfMissing )
import System.FilePath( (</>) )

import Linear( V2( .. ) )


backgroundColor :: PixelRGBA8
backgroundColor = PixelRGBA8 255 255 255 255

frontTexture, accentTexture, accent2Texture :: Texture PixelRGBA8
frontTexture = uniformTexture $ PixelRGBA8 0 0x86 0xc1 255
accentTexture = uniformTexture $ PixelRGBA8 0xff 0xf4 0xc1 255
accent2Texture = uniformTexture $ PixelRGBA8 0xFF 0x53 0x73 255

produceDocImage :: FilePath -> Drawing PixelRGBA8 () -> IO ()
produceDocImage filename drawing = writePng filename img
  where
    img = renderDrawing 200 200 backgroundColor
        $ withTexture frontTexture drawing

capTester :: (FilePath, Cap) -> IO ()
capTester (filename, cap) =
    produceDocImage filename $ do
        stroke 30 JoinRound (cap, cap) base_stroke
        withTexture accentTexture $
            stroke 2 JoinRound (cap, cap) base_stroke
  where 
    base_stroke = line (V2 0 200) (V2 100 100)

joinTester :: (FilePath, Join) -> IO ()
joinTester (filename, join) =
    produceDocImage filename $ do
        stroke 30 join (CapRound, CapRound) base_stroke
        withTexture accentTexture $
            stroke 2 join (CapRound, CapRound) base_stroke
  where 
    base_stroke = LinePrim <$>
        [ Line (V2 0 200) (V2 100 100)
        , Line (V2 100 100) (V2 200 200)
        ]

main :: IO ()
main = do
    let outFolder = "docimages"
        addFolder (p, v) = (outFolder </> p, v)
    createDirectoryIfMissing True outFolder

    mapM_ (capTester . addFolder)
        [ ("cap_straight.png", CapStraight 0)
        , ("cap_straight_1.png", CapStraight 1)
        , ("cap_round.png", CapRound)
        ]

    mapM_ (joinTester . addFolder)
        [ ("join_round.png", JoinRound)
        , ("join_miter.png", JoinMiter 0)
        , ("join_miter_5.png", JoinMiter 5)
        ]

    produceDocImage (outFolder </> "fill_circle.png") $
        fill $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "stroke_circle.png") $
        stroke 5 JoinRound (CapRound, CapRound) $ circle (V2 100 100) 75 

    produceDocImage (outFolder </> "dashed_stroke.png") $
        dashedStroke [5, 10, 5] 3 JoinRound (CapRound, CapStraight 0) $
            line (V2 0 100) (V2 200 100)

    produceDocImage (outFolder </> "fill_rect.png") $
        fill $ rectangle (V2 30 30) 150 100

    produceDocImage (outFolder </> "with_texture.png") $
      withTexture frontTexture $ do
          fill $ circle (V2 50 50) 20
          fill $ circle (V2 100 100) 20
          withTexture accent2Texture $
               fill $ circle (V2 150 150) 20

    produceDocImage (outFolder </> "with_clipping.png") $
      withClipping (fill $ circle (V2 100 100) 75) $
          mapM_ (stroke 7 JoinRound (CapRound, CapRound))
            [line (V2 0 yf) (V2 200 (yf + 10)) 
                           | y <- [5 :: Int, 17 .. 200]
                           , let yf = fromIntegral y ]

    produceDocImage (outFolder </> "stroke_line.png") $
      stroke 17 JoinRound (CapRound, CapRound) $
        line (V2 10 10) (V2 180 170)

    produceDocImage (outFolder </> "linear_gradient.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (linearGradientTexture gradDef (V2 40 40) (V2 130 130)) $
            fill $ circle (V2 100 100) 100)

    produceDocImage (outFolder </> "radial_gradient.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (radialGradientTexture gradDef (V2 100 100) 75) $
            fill $ circle (V2 100 100) 100)

    produceDocImage (outFolder </> "radial_gradient_focus.png") $
       (let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
                      ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
                      ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
        withTexture (radialGradientWithFocusTexture gradDef
                        (V2 100 100) 75 (V2 70 70)) $
            fill $ circle (V2 100 100) 100)
