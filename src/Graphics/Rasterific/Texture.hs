{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module describing the various filling method of the
-- geometric primitives.
--
-- All points coordinate given in this module are expressed
-- final image pixel coordinates.
module Graphics.Rasterific.Texture
    ( Texture
    , Gradient
    , uniformTexture
    , linearGradientTexture
    , radialGradientTexture
    , radialGradientWithFocusTexture
    , imageTexture
    , modulateTexture
    ) where

{-import Control.Applicative( liftA )-}
import Data.Fixed( mod' )
import Linear( V2( .. )
             , (^-^)
             , (^+^)
             , (^/)
             , (^*)
             , dot
             , norm
             )
import qualified Data.Vector as V

import Codec.Picture.Types( Pixel( .. )
                          , Image( .. )
                          )
import Graphics.Rasterific.Types( Point, SamplerRepeat( .. ) )
import Graphics.Rasterific.Compositor
    ( Modulable( clampCoverage, modulate ), compositionAlpha )

-- | A texture is just a function which given pixel coordinate
-- give back a pixel.
-- The float coordinate type allow for transformations
-- to happen in the pixel space.
type Texture px = Float -> Float -> px

-- | The uniform texture is the simplest texture of all:
-- an uniform color.
uniformTexture :: px -- ^ The color used for all the texture.
               -> Texture px
uniformTexture px _ _ = px

-- | A gradient definition is just a list of stop
-- and pixel values. For instance for a simple gradient
-- of black to white, the finition would be :
--
-- > [(0, PixelRGBA8 0 0 0 255), (1, PixelRGBA8 255 255 255 255)]
-- 
-- the first stop value must be zero and the last, one.
--
type Gradient px = [(Float, px)]
type GradientArray px = V.Vector (Float, px)

repeatGradient :: Float -> Float
repeatGradient s = s - fromIntegral (floor s :: Int)

reflectGradient :: Float -> Float
reflectGradient s =
    abs (abs (s - 1) `mod'` 2 - 1)
   
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

gradientColorAtRepeat :: (Pixel px, Modulable (PixelBaseComponent px))
                      => SamplerRepeat -> GradientArray px -> Float -> px
gradientColorAtRepeat SamplerPad grad = gradientColorAt grad
gradientColorAtRepeat SamplerRepeat grad =
    gradientColorAt grad . repeatGradient
gradientColorAtRepeat SamplerReflect grad =
    gradientColorAt grad . reflectGradient

-- | Linear gradient texture.
--
-- > let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
-- >               ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
-- >               ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
-- > withTexture (linearGradientTexture SamplerPad gradDef
-- >                        (V2 40 40) (V2 130 130)) $
-- >    fill $ circle (V2 100 100) 100
--
-- <<docimages/linear_gradient.png>>
--
linearGradientTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                      => SamplerRepeat
                      -> Gradient px -- ^ Gradient description.
                      -> Point       -- ^ Linear gradient start point.
                      -> Point       -- ^ Linear gradient end point.
                      -> Texture px
linearGradientTexture repeating gradient start end =
    \x y -> colorAt $ ((V2 x y) `dot` d) - s00
  where
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient
    vector = end ^-^ start
    d = vector ^/ (vector `dot` vector)
    s00 = start `dot` d

-- | Use another image as a texture for the filling.
imageTexture :: forall px. (Pixel px) => Image px -> Texture px
imageTexture img x y =
    unsafePixelAt rawData $ (clampedY * w + clampedX) * compCount
  where
   clampedX = min (w - 1) . max 0 $ floor x
   clampedY = min (h - 1) . max 0 $ floor y
   compCount = componentCount (undefined :: px)
   w = imageWidth img
   h = imageHeight img
   rawData = imageData img

-- | Radial gradient texture
--
-- > let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
-- >               ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
-- >               ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
-- > withTexture (radialGradientTexture SamplerPad gradDef
-- >                    (V2 100 100) 75) $
-- >    fill $ circle (V2 100 100) 100
--
-- <<docimages/radial_gradient.png>>
--
radialGradientTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                      => SamplerRepeat
                      -> Gradient px -- ^ Gradient description
                      -> Point       -- ^ Radial gradient center
                      -> Float       -- ^ Radial gradient radius
                      -> Texture px
radialGradientTexture repeating gradient center radius =
    \x y -> colorAt $ norm ((V2 x y) ^-^ center) / radius
  where
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient


-- | Radial gradient texture with a focus point.
--
-- > let gradDef = [(0, PixelRGBA8 0 0x86 0xc1 255)
-- >               ,(0.5, PixelRGBA8 0xff 0xf4 0xc1 255)
-- >               ,(1, PixelRGBA8 0xFF 0x53 0x73 255)] in
-- > withTexture (radialGradientWithFocusTexture SamplerPad gradDef
-- >                    (V2 100 100) 75 (V2 70 70) ) $
-- >    fill $ circle (V2 100 100) 100
--
-- <<docimages/radial_gradient_focus.png>>
--
radialGradientWithFocusTexture
    :: (Pixel px, Modulable (PixelBaseComponent px))
    => SamplerRepeat
    -> Gradient px -- ^ Gradient description
    -> Point      -- ^ Radial gradient center
    -> Float      -- ^ Radial gradient radius
    -> Point      -- ^ Radial gradient focus point
    -> Texture px
radialGradientWithFocusTexture repeating gradient center radius focusScreen =
    \x y -> colorAt . go $ (V2 x y) ^-^ center
  where
    focus@(V2 origFocusX origFocusY) = focusScreen ^-^ center
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient
    radiusSquared = radius * radius
    dist = sqrt $ focus `dot` focus
    clampedFocus@(V2 focusX focusY)
        | dist <= r = focus
        | otherwise = V2 (r * cos a) (r * sin a)
           where a = atan2 origFocusY origFocusX
                 r = radius * 0.99
    trivial = sqrt $ radiusSquared - focusX * focusY

    solutionOf (V2 x y) | x == focusX =
        V2 focusX (if y > focusY then trivial else negate trivial)
    solutionOf (V2 x y) = V2 xSolution $ slope * xSolution + yint
      where
        slope = (y - focusY) / (x - focusX)
        yint = y - (slope * x)

        a = slope * slope + 1
        b = 2 * slope * yint
        c = yint * yint - radiusSquared
        det = sqrt $ b * b - 4 * a * c
        xSolution = (-b + (if x < focusX then negate det else det)) / (2 * a)

    go pos = sqrt $ curToFocus / distSquared
      where
        solution = solutionOf pos ^-^ clampedFocus
        toFocus = pos ^-^ clampedFocus
        distSquared = solution `dot` solution
        curToFocus = toFocus `dot` toFocus

-- | Perform a multiplication operation between a full color texture
-- and a greyscale one, used for clip-path implementation.
modulateTexture :: (Pixel px, Modulable (PixelBaseComponent px))
                => Texture px                       -- ^ The full blown texture.
                -> Texture (PixelBaseComponent px)  -- ^ A greyscale modulation texture.
                -> Texture px                       -- ^ The resulting texture.
modulateTexture fullTexture modulator x y =
    colorMap (modulate modulation) $ fullTexture x y
  where modulation = modulator x y

