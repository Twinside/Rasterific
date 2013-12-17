module Graphics.Rasterific.Texture
    ( Texture
    , uniformTexture
    ) where

type Texture px = Int -> Int -> px

uniformTexture :: px -> Texture px
uniformTexture px _ _ = px

