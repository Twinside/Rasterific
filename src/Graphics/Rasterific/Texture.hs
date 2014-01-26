{-# LANGUAGE FlexibleContexts #-}
-- | Module describing the various filling method of the
-- geometric primitives.
module Graphics.Rasterific.Texture
    ( Texture
    , Gradient
    , uniformTexture
    , linearGradientTexture
    ) where

import Linear( V2( .. ), (^-^), (^/), dot )
import qualified Data.Vector as V

import Codec.Picture.Types( Pixel( .. ) )
import Graphics.Rasterific.Types( Point )
import Graphics.Rasterific.Compositor
    ( Modulable( clampCoverage ), compositionAlpha )

-- | A texture is just a function which given pixel coordinate
-- give back a pixel.
type Texture px = Int -> Int -> px

-- | The uniform texture is the simplest texture of all:
-- an uniform color.
uniformTexture :: px -> Texture px
uniformTexture px _ _ = px

type Gradient px = [(Float, px)]
type GradientArray px = V.Vector (Float, px)

gradientColorAt :: (Pixel px, Modulable (PixelBaseComponent px))
                => GradientArray px -> Float -> px
gradientColorAt grad at
    | at <= 0 = snd $ V.head grad
    | at >= 1.0 = snd $ V.last grad
    | otherwise = go (0, snd $ V.head grad) 0
  where
    maxi = V.length grad
    go (prevCoeff, prevValue) ix
      | ix >= maxi = snd $ V.last grad
      | at < coeff = compositionAlpha cov icov prevValue px
      | otherwise = go value $ ix + 1
      where value@(coeff, px) = grad `V.unsafeIndex` ix
            zeroToOne = (at - prevCoeff) / (coeff - prevCoeff)
            (cov, icov) = clampCoverage zeroToOne

linearGradientTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                      => Gradient px -> Point -> Point -> Texture px
linearGradientTexture gradient start end =
    \x y -> colorAt $ ((V2 (fi x) (fi y)) `dot` d) - s00
  where
    fi = fromIntegral
    colorAt = gradientColorAt gradArray
    gradArray = V.fromList gradient
    vector = end ^-^ start
    d = vector ^/ (vector `dot` vector)
    s00 = start `dot` d

