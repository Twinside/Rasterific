{-# LANGUAGE FlexibleContexts #-}
-- | Compositor handle the pixel composition, which
-- leads to texture composition.
-- Very much a work in progress
module Graphics.Rasterific.Compositor
    ( Compositor
    , Modulable( .. )
    , compositionDestination
    , compositionAlpha
    ) where

import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word32 )

import Codec.Picture.Types( Pixel( .. ) )

type Compositor px =
    (PixelBaseComponent px) ->
        (PixelBaseComponent px) -> px -> px -> px

-- | Typeclass intented at pixel value modulation.
-- May be throwed out soon.
class Ord a => Modulable a where
  emptyValue :: a
  fullValue  :: a
  clampCoverage :: Float -> (a, a)
  modulate :: a -> a -> a
  alphaOver :: a -> a -> a -> a -> a

instance Modulable Word8 where
  emptyValue = 0
  fullValue = 255
  clampCoverage f = (fromIntegral c, fromIntegral $ 255 - c)
     where c = toWord8 f

  modulate c a = fromIntegral $ v `unsafeShiftR` 8
    where fi :: Word8 -> Word32
          fi = fromIntegral
          v = fi c * fi a

  alphaOver c ic b a = fromIntegral $ (v + (v `unsafeShiftR` 8)) `unsafeShiftR` 8
    where fi :: Word8 -> Word32
          fi = fromIntegral
          v = fi c * fi a + fi b * fi ic + 128


toWord8 :: Float -> Int
toWord8 r = floor $ r * 255 + 0.5

compositionDestination :: (Pixel px, Modulable (PixelBaseComponent px))
                       => Compositor px
compositionDestination c _ _ a = colorMap (modulate c) $ a

compositionAlpha :: (Pixel px, Modulable (PixelBaseComponent px))
                 => Compositor px
compositionAlpha c ic 
    | c == emptyValue = const
    | c == fullValue = \_ n -> n
    | otherwise = mixWith (\_ -> alphaOver c ic)

