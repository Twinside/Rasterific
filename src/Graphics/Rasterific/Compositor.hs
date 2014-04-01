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
class (Ord a, Num a) => Modulable a where
  emptyValue :: a
  fullValue  :: a
  clampCoverage :: Float -> (a, a)
  modulate :: a -> a -> a
  alphaOver :: a -> a -> a -> a -> a
  alphaCompose :: a -> a -> a -> a -> a
  coverageModulate :: a -> a -> (a, a)

instance Modulable Word8 where
  emptyValue = 0
  fullValue = 255
  clampCoverage f = (fromIntegral c, fromIntegral $ 255 - c)
     where c = toWord8 f

  modulate c a = fromIntegral $ (v + (v `unsafeShiftR` 8)) `unsafeShiftR` 8
    where fi :: Word8 -> Word32
          fi = fromIntegral
          v = fi c * fi a + 128

  coverageModulate c a = (clamped, fullValue - clamped)
    where clamped = modulate a c

  alphaCompose coverage inverseCoverage backgroundAlpha _ =
      fromIntegral $ (v + (v `unsafeShiftR` 8)) `unsafeShiftR` 8
        where fi :: Word8 -> Word32
              fi = fromIntegral
              v = fi coverage * 255
                + fi backgroundAlpha * fi inverseCoverage + 128

  alphaOver coverage inverseCoverage background painted =
      fromIntegral $ (v + (v `unsafeShiftR` 8)) `unsafeShiftR` 8
    where fi :: Word8 -> Word32
          fi = fromIntegral
          v = fi coverage * fi painted + fi background * fi inverseCoverage + 128


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
    | otherwise = mixWithAlpha (\_ -> alphaOver c ic)
                               (alphaCompose c ic)

