{-# LANGUAGE FlexibleContexts #-}
-- | Module describing the various filling method of the
-- geometric primitives.
module Graphics.Rasterific.Texture
    ( Texture
    , Gradient
    , uniformTexture
    , linearGradientTexture
    , radialGradientTexture
    , radialGradientWithFocusTexture
    ) where

{-import Control.Applicative( liftA )-}
import Linear( V2( .. )
             , (^-^)
             , (^+^)
             , (^/)
             , (^*)
             , dot
             , norm
             )
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

radialGradientTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                      => Gradient px -> Point -> Float -> Texture px
radialGradientTexture gradient center radius =
    \x y -> colorAt $ norm ((V2 (fi x) (fi y)) ^-^ center) / radius
  where
    fi = fromIntegral
    colorAt = gradientColorAt gradArray
    gradArray = V.fromList gradient

repeatGradient :: Float -> Float
repeatGradient v = v - fromIntegral (floor v :: Int)

radialGradientWithFocusTexture
    :: (Pixel px, Modulable (PixelBaseComponent px))
    => Gradient px -> Point -> Float -> Point -> Texture px
radialGradientWithFocusTexture gradient center radius focus =
    \x y -> colorAt . go $ V2 (fromIntegral x) (fromIntegral y)
  where
   vectorToFocus = focus ^-^ center
   gradArray = V.fromList gradient
   colorAt = gradientColorAt gradArray
   rSquare = radius * radius

   go point | distToFocus > 0 = distToFocus / (t + dist)
            | otherwise = 0
     where
       focusToPoint = point ^-^ focus
       distToFocus = sqrt $ focusToPoint `dot` focusToPoint
       directionVector = focusToPoint ^/ distToFocus

       t = directionVector `dot` vectorToFocus
       closestPoint = directionVector ^* t ^+^ focus
       vectorToClosest = (closestPoint ^-^ center)
       distToCircleSquared = vectorToClosest `dot` vectorToClosest
       dist = sqrt $ rSquare + distToCircleSquared

