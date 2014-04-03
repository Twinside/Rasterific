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
    , withSampler
    , uniformTexture
      -- * Texture kind
    , linearGradientTexture
    , radialGradientTexture
    , radialGradientWithFocusTexture
    , imageTexture
    , sampledImageTexture

      -- * Texture manipulation
    , modulateTexture
    , transformTexture 
    ) where

import Data.Fixed( mod' )
import Linear( V2( .. )
             , (^-^)
             , (^/)
             , dot
             , norm
             )

import qualified Data.Vector as V

import Codec.Picture.Types( Pixel( .. )
                          , Image( .. )
                          , Pixel8
                          , PixelRGBA8
                          )
import Graphics.Rasterific.Types( Point, SamplerRepeat( .. ) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Compositor
    ( Modulable( clampCoverage, modulate, alphaOver ), compositionAlpha )

-- | A texture is just a function which given pixel coordinate
-- give back a pixel.
-- The float coordinate type allow for transformations
-- to happen in the pixel space.
type Texture px = SamplerRepeat -> Float -> Float -> px

-- | Set the repeat pattern of the texture (if any).
-- With padding:
--
-- > withTexture (sampledImageTexture textureImage) $
-- >   fill $ rectangle (V2 0 0) 200 200
--
-- <<docimages/sampled_texture_pad.png>>
--
-- With repeat:
--
-- > withTexture (withSampler SamplerRepeat $
-- >                 sampledImageTexture textureImage) $
-- >     fill $ rectangle (V2 0 0) 200 200
--
-- <<docimages/sampled_texture_repeat.png>>
--
-- With reflect:
--
-- > withTexture (withSampler SamplerReflect $
-- >                 sampledImageTexture textureImage) $
-- >     fill $ rectangle (V2 0 0) 200 200
--
-- <<docimages/sampled_texture_reflect.png>>
--
withSampler :: SamplerRepeat -> Texture px -> Texture px
withSampler repeating texture _ = texture repeating

-- | Transform the coordinates used for texture before applying
-- it, allow interesting transformations.
--
-- > withTexture (withSampler SamplerRepeat $
-- >             transformTexture (rotateCenter 1 (V2 0 0) <> 
-- >                               scale 0.5 0.25)
-- >             $ sampledImageTexture textureImage) $
-- >     fill $ rectangle (V2 0 0) 200 200
--
-- <<docimages/sampled_texture_scaled.png>>
--
transformTexture :: Transformation -> Texture px -> Texture px
transformTexture trans tx samp x y = tx samp x' y'
  where
    (V2 x' y') = applyTransformation trans (V2 x y)

-- | The uniform texture is the simplest texture of all:
-- an uniform color.
uniformTexture :: px -- ^ The color used for all the texture.
               -> Texture px
uniformTexture px _ _ _ = px

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
{-# SPECIALIZE
 	gradientColorAt :: GradientArray PixelRGBA8 -> Float -> PixelRGBA8 #-}
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
{-# SPECIALIZE INLINE
	gradientColorAtRepeat ::
		SamplerRepeat -> GradientArray PixelRGBA8 -> Float -> PixelRGBA8 #-}
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
                      => Gradient px -- ^ Gradient description.
                      -> Point       -- ^ Linear gradient start point.
                      -> Point       -- ^ Linear gradient end point.
                      -> Texture px
{-# SPECIALIZE
	linearGradientTexture
		:: Gradient PixelRGBA8 -> Point -> Point
        -> Texture PixelRGBA8 #-}
linearGradientTexture gradient start end repeating =
    \x y -> colorAt $ ((V2 x y) `dot` d) - s00
  where
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient
    vector = end ^-^ start
    d = vector ^/ (vector `dot` vector)
    s00 = start `dot` d

-- | Use another image as a texture for the filling.
-- Contrary to `imageTexture`, this function perform a bilinear
-- filtering on the texture.
--
sampledImageTexture :: forall px.
                       ( Pixel px, Modulable (PixelBaseComponent px))
                    => Image px -> Texture px
{-# SPECIALIZE
 	sampledImageTexture :: Image Pixel8 -> Texture Pixel8 #-}
{-# SPECIALIZE
 	sampledImageTexture :: Image PixelRGBA8 -> Texture PixelRGBA8 #-}
sampledImageTexture img sampling x y =
  (at px  py `interpX` at pxn py)
             `interpY`
  (at px pyn `interpX` at pxn pyn)
  where
   coordSampler SamplerPad maxi v =
      min (maxi - 1) . max 0 $ floor v
   coordSampler SamplerReflect maxi v =
      floor $ abs (abs (v - maxif - 1) `mod'` (2 * maxif) - maxif - 1)
        where maxif = fromIntegral maxi
   coordSampler SamplerRepeat maxi v = floor v `mod` maxi

   w = fromIntegral $ imageWidth img
   h = fromIntegral $ imageHeight img

   clampedX = coordSampler sampling w
   clampedY = coordSampler sampling h

   px = clampedX x
   pxn = clampedX $ x + 1
   py = clampedY y
   pyn = clampedY $ y + 1

   dx, dy :: Float
   dx = x - fromIntegral (floor x :: Int)
   dy = y - fromIntegral (floor y :: Int)

   at :: Int -> Int -> px
   at xx yy =
        unsafePixelAt rawData $ (yy * w + xx) * compCount

   (covX, icovX) = clampCoverage dx
   (covY, icovY) = clampCoverage dy

   interpX = mixWith (const $ alphaOver covX icovX)
   interpY = mixWith (const $ alphaOver covY icovY)

   compCount = componentCount (undefined :: px)
   rawData = imageData img

-- | Use another image as a texture for the filling.
-- This texture use the "nearest" filtering, AKA no
-- filtering at all.
imageTexture :: forall px. (Pixel px) => Image px -> Texture px
{-# SPECIALIZE
	imageTexture :: Image PixelRGBA8 -> Texture PixelRGBA8 #-}
{-# SPECIALIZE
	imageTexture :: Image Pixel8 -> Texture Pixel8 #-}
imageTexture img _ x y =
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
                      => Gradient px -- ^ Gradient description
                      -> Point       -- ^ Radial gradient center
                      -> Float       -- ^ Radial gradient radius
                      -> Texture px
radialGradientTexture gradient center radius repeating =
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
    => Gradient px -- ^ Gradient description
    -> Point      -- ^ Radial gradient center
    -> Float      -- ^ Radial gradient radius
    -> Point      -- ^ Radial gradient focus point
    -> Texture px
radialGradientWithFocusTexture gradient center radius focusScreen repeating =
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
    trivial = sqrt $ radiusSquared - origFocusX * origFocusX

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
modulateTexture fullTexture modulator repeating = \x y ->
    colorMap (modulate $ modulationTexture x y) $ full x y
  where modulationTexture = modulator repeating
        full = fullTexture repeating

