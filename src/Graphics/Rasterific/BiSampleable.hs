{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Module to describe bi-sampleable types
module Graphics.Rasterific.BiSampleable
    ( BiSampleable( .. )
    , bilinearInterpolation
    , sampledImageShader
    ) where

import Data.Fixed( mod' )
import Codec.Picture
    ( Image( .. )
    , Pixel8
    , Pixel( .. )
    , PixelRGBA8( .. ) )

import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Command
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.Transformations

-- | Interpolate a 2D point in a given type
class BiSampleable sampled px | sampled -> px where
  -- | The interpolation function
  interpolate :: sampled -> Float -> Float -> px

-- | Basic bilinear interpolator
instance  (Pixel px, Modulable (PixelBaseComponent px))
    => BiSampleable (ParametricValues px) px where
  {-# INLINE interpolate #-}
  interpolate = bilinearPixelInterpolation

-- | Bicubic interpolator
instance ( InterpolablePixel px
         , Num (Holder px Float)
         ) => BiSampleable (CubicCoefficient px) px where
  {-# INLINE interpolate #-}
  interpolate = bicubicInterpolation

-- | Bilinear interpolation of an image
instance BiSampleable (ImageMesh PixelRGBA8) PixelRGBA8 where
  {-# INLINE interpolate #-}
  interpolate imesh xb yb = sampledImageShader (_meshImage imesh) SamplerPad x y
    where (V2 x y) = applyTransformation (_meshTransform imesh) (V2 xb yb)

-- | Use another image as a texture for the filling.
-- Contrary to `imageTexture`, this function perform a bilinear
-- filtering on the texture.
--
sampledImageShader :: forall px. RenderablePixel px
                   => Image px -> SamplerRepeat -> ShaderFunction px
{-# SPECIALIZE
     sampledImageShader :: Image Pixel8 -> SamplerRepeat
                        -> ShaderFunction Pixel8 #-}
{-# SPECIALIZE
     sampledImageShader :: Image PixelRGBA8 -> SamplerRepeat
                        -> ShaderFunction PixelRGBA8 #-}
sampledImageShader img _ _ _
  | imageWidth img == 0 || imageHeight img == 0 = emptyPx
sampledImageShader img sampling x y =
  (at px  py `interpX` at pxn py)
             `interpY`
  (at px pyn `interpX` at pxn pyn)
  where
   coordSampler SamplerPad maxi v = min (maxi - 1) . max 0 $ floor v
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
   !dx = x - fromIntegral (floor x :: Int)
   !dy = y - fromIntegral (floor y :: Int)

   at :: Int -> Int -> px
   at !xx !yy =
        unsafePixelAt rawData $ (yy * w + xx) * compCount

   (covX, icovX) = clampCoverage dx
   (covY, icovY) = clampCoverage dy

   interpX = mixWith (const $ alphaOver covX icovX)
   interpY = mixWith (const $ alphaOver covY icovY)

   compCount = componentCount (undefined :: px)
   rawData = imageData img

bilinearPixelInterpolation :: (Pixel px, Modulable (PixelBaseComponent px))
                           => ParametricValues px -> Float -> Float -> px
{-# SPECIALIZE INLINE
    bilinearPixelInterpolation :: ParametricValues PixelRGBA8 -> Float -> Float -> PixelRGBA8
  #-}
bilinearPixelInterpolation (ParametricValues { .. }) !dx !dy = 
  mixWith (const $ alphaOver covY icovY)
        (mixWith (const $ alphaOver covX icovX) _northValue _eastValue)
        (mixWith (const $ alphaOver covX icovX) _westValue _southValue)
  where
   (!covX, !icovX) = clampCoverage dx
   (!covY, !icovY) = clampCoverage dy

bilinearInterpolation :: InterpolablePixel px
                      => ParametricValues px -> Float -> Float -> px
{-# INLINE bilinearInterpolation #-}
bilinearInterpolation ParametricValues { .. } u v = fromFloatPixel $ lerp v uBottom uTop where
  -- The arguments are flipped, because the lerp function from Linear is...
  -- inversed in u v
  !uTop = lerp u (toFloatPixel _eastValue) (toFloatPixel _northValue)
  !uBottom = lerp u (toFloatPixel _southValue) (toFloatPixel _westValue)


bicubicInterpolation :: forall px . (InterpolablePixel px, Num (Holder px Float))
                     => CubicCoefficient px -> Float -> Float -> px
bicubicInterpolation params x y =
  fromFloatPixel . fmap clamp $ af ^+^ bf ^+^ cf ^+^ df
  where
    ParametricValues a b c d = getCubicCoefficients params
    maxi = maxRepresentable (Proxy :: Proxy px)
    clamp = max 0 . min maxi
    xv, vy, vyy, vyyy :: V4 Float
    xv = V4 1 x (x*x) (x*x*x)
    vy = xv ^* y
    vyy = vy ^* y
    vyyy = vyy ^* y

    v1 ^^*^ v2 = (^*) <$> v1 <*> v2

    V4 af bf cf df = (a ^^*^ xv) ^+^ (b ^^*^ vy) ^+^ (c ^^*^ vyy) ^+^ (d ^^*^ vyyy)

