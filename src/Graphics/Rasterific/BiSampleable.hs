{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Rasterific.BiSampleable
    ( BiSampleable( .. ) )
    where

import Codec.Picture( PixelRGBA8( .. ) )

import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Shading
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.Transformations

class BiSampleable sampled px where
  interpolate :: sampled -> UV -> px

-- | Basic bilinear interpolator
instance {-# INCOHERENT #-} InterpolablePixel px => BiSampleable (ParametricValues px) px where
  interpolate = bilinearInterpolation

-- | Bicubic interpolator
instance {-# INCOHERENT #-}
         ( InterpolablePixel px
         , Num (Holder px Float)
         , v ~ Holder px Float
         ) => BiSampleable (CubicCoefficient px) px where
  interpolate = bicubicInterpolation

instance BiSampleable (ImageMesh PixelRGBA8) PixelRGBA8 where
  interpolate imesh p = sampledImageShader (_meshImage imesh) SamplerPad x y
    where (V2 x y) = applyTransformation (_meshTransform imesh) p

bilinearInterpolation :: InterpolablePixel px
                      => ParametricValues px -> UV -> px
bilinearInterpolation ParametricValues { .. } (V2 u v) = fromFloatPixel $ lerp v uBottom uTop where
  -- The arguments are flipped, because the lerp function from Linear is...
  -- inversed in u v
  uTop = lerp u (toFloatPixel _eastValue) (toFloatPixel _northValue)
  uBottom = lerp u (toFloatPixel _southValue) (toFloatPixel _westValue)

bicubicInterpolation :: forall px . (InterpolablePixel px, Num (Holder px Float))
                     => CubicCoefficient px -> UV -> px
bicubicInterpolation params  (V2 x y) =
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

