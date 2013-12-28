-- | Module describing the various filling method of the
-- geometric primitives.
module Graphics.Rasterific.Texture
    ( Texture
    , uniformTexture
    ) where

-- | A texture is just a function which given pixel coordinate
-- give back a pixel.
type Texture px = Int -> Int -> px

-- | The uniform texture is the simplest texture of all:
-- an uniform color.
uniformTexture :: px -> Texture px
uniformTexture px _ _ = px

