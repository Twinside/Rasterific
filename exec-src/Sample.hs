{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sample where

import Data.Monoid
import Codec.Picture
import Codec.Picture.Gif
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

triangles :: IO ()
triangles =
    case writeGifImages "triangles.gif" LoopingForever images of
        Left err -> putStrLn err
        Right v -> v

  where
    frameCount = 140
    images = 
        [(greyPalette, 3, go (i * pi * 2 * (1 / frameCount)))
                | i <- [0 .. frameCount] ]
    go angle = renderDrawing 400 400 0 $
       mapM_ (render angle) [1 .. 25]

    render angle n =
      withTransformation
        ( translate (V2 200 180) <>
          scale (1 / n + 1) (1 / n + 1) <>
          rotate (angle + 0.1 * angle * n) ) $

         withTexture (uniformTexture (155 + 4 * floor n)) $
          stroke 2 (JoinMiter 0)
              (CapStraight 0, CapStraight 0)
              triangle


    triangle =
        Path (V2 0 50) True
          [ PathLineTo (V2 50 (-30))
          , PathLineTo (V2 (-50) (-30))
          ]

main :: IO ()
main = do
  triangles
