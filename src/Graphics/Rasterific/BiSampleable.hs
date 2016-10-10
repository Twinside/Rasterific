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
module Graphics.Rasterific.BiSampleable( BiSampleable( .. ) ) where

import Codec.Picture( PixelRGBA8( .. ) )

import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Shading
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.Transformations

import Codec.Picture( Pixel( .. ) )

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

