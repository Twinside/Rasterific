{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- | Compositor handle the pixel composition, which
-- leads to texture composition.
-- Very much a work in progress
module Graphics.Rasterific.Compositor
    ( Compositor
    , Modulable( .. )
    , InterpolablePixel( .. )
    , maxDistance
    , RenderablePixel
    , ModulablePixel
    , compositionDestination
    , compositionAlpha
    , emptyPx
    ) where

import Data.Kind ( Type )

import Foreign.Storable( Storable )
import Data.Bits( unsafeShiftR )
import Data.Word( Word8, Word32 )

import Codec.Picture.Types
    ( Pixel( .. )
    , PixelRGB8( .. )
    , PixelRGBA8( .. )
    , PackeablePixel( .. ) )

import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types

type Compositor px =
    PixelBaseComponent px ->
        PixelBaseComponent px -> px -> px -> px

-- | Used for Coon patch rendering
class ( Applicative (Holder a)
      , Functor  (Holder a)
      , Foldable (Holder a)
      , Additive (Holder a) ) => InterpolablePixel a where
  type Holder a :: Type -> Type
  toFloatPixel :: a -> Holder a Float
  fromFloatPixel :: Holder a Float -> a
  maxRepresentable :: Proxy a -> Float

maxDistance :: InterpolablePixel a => a -> a -> Float
maxDistance p1 p2 = maximum $ abs <$> (toFloatPixel p1 ^-^ toFloatPixel p2)

instance InterpolablePixel Float where
  type Holder Float = V1
  toFloatPixel = V1
  fromFloatPixel (V1 f) = f
  maxRepresentable Proxy = 1

instance InterpolablePixel Word8 where
  type Holder Word8 = V1
  toFloatPixel = V1 . fromIntegral
  fromFloatPixel (V1 f) = floor f
  maxRepresentable Proxy = 255

instance InterpolablePixel PixelRGB8 where
  type Holder PixelRGB8 = V3
  toFloatPixel (PixelRGB8 r g b) = V3 (to r) (to g) (to b) where to n = fromIntegral n
  fromFloatPixel (V3 r g b) = PixelRGB8 (to r) (to g) (to b) where to = floor
  maxRepresentable Proxy = 255

instance InterpolablePixel PixelRGBA8 where
  type Holder PixelRGBA8 = V4
  toFloatPixel (PixelRGBA8 r g b a) = V4 (to r) (to g) (to b) (to a)
    where to n = fromIntegral n
  fromFloatPixel (V4 r g b a) = PixelRGBA8 (to r) (to g) (to b) (to a)
    where to = floor
  maxRepresentable Proxy = 255

-- | This constraint ensure that a type is a pixel
-- and we're allowed to modulate it's color components
-- generically.
type ModulablePixel px =
    ( Pixel px
    , PackeablePixel px
    , InterpolablePixel px
    , InterpolablePixel (PixelBaseComponent px)
    , Storable (PackedRepresentation px)
    , Modulable (PixelBaseComponent px))

-- | This constraint tells us that pixel component
-- must also be pixel and be the "bottom" of component,
-- we cannot go further than a PixelBaseComponent level.
--
-- Tested pixel types are PixelRGBA8 & Pixel8
type RenderablePixel px =
    ( ModulablePixel px
    , Pixel (PixelBaseComponent px)
    , PackeablePixel (PixelBaseComponent px)
    , Num (PackedRepresentation px)
    , Num (PackedRepresentation (PixelBaseComponent px))
    , Num (Holder px Float)
    , Num (Holder (PixelBaseComponent px) Float)
    , Storable (PackedRepresentation (PixelBaseComponent px))
    , PixelBaseComponent (PixelBaseComponent px)
            ~ (PixelBaseComponent px)
    )

-- | Typeclass intented at pixel value modulation.
-- May be throwed out soon.
class (Ord a, Num a) => Modulable a where
  -- | Empty value representing total transparency for the given type.
  emptyValue :: a
  -- | Full value representing total opacity for a given type.
  fullValue  :: a
  -- | Given a Float in [0; 1], return the coverage in [emptyValue; fullValue]
  -- The second value is the inverse coverage
  clampCoverage :: Float -> (a, a)

  -- | Modulate two elements, staying in the [emptyValue; fullValue] range.
  modulate :: a -> a -> a

  -- | Implement a division between two elements.
  modiv :: a -> a -> a

  alphaOver :: a -- ^ coverage
            -> a -- ^ inverse coverage
            -> a -- ^ background
            -> a -- ^ foreground
            -> a
  alphaCompose :: a -> a -> a -> a -> a

  -- | Like modulate but also return the inverse coverage.
  coverageModulate :: a -> a -> (a, a)
  {-# INLINE coverageModulate #-}
  coverageModulate c a = (clamped, fullValue - clamped)
    where clamped = modulate a c

instance Modulable Float where
  emptyValue = 0
  fullValue = 1
  clampCoverage f = (f, 1 - f)
  modulate = (*)
  modiv = (/)
  alphaCompose coverage inverseCoverage backAlpha _ =
      coverage + backAlpha * inverseCoverage
  alphaOver coverage inverseCoverage background painted =
      coverage * painted + background * inverseCoverage

div255 :: Word32 -> Word32
{-# INLINE div255 #-}
div255 v = (v + (v `unsafeShiftR` 8)) `unsafeShiftR` 8

instance Modulable Word8 where
  {-# INLINE emptyValue #-}
  emptyValue = 0
  {-# INLINE fullValue #-}
  fullValue = 255
  {-# INLINE clampCoverage #-}
  clampCoverage f = (fromIntegral c, fromIntegral $ 255 - c)
     where c = toWord8 f

  {-# INLINE modulate #-}
  modulate c a = fromIntegral . div255 $ fi c * fi a + 128
    where fi :: Word8 -> Word32
          fi = fromIntegral

  {-# INLINE modiv #-}
  modiv c 0 = c
  modiv c a = fromIntegral . min 255 $ (fi c * 255) `div` fi a
    where fi :: Word8 -> Word32
          fi = fromIntegral

  {-# INLINE alphaCompose #-}
  alphaCompose coverage inverseCoverage backgroundAlpha _ =
      fromIntegral $ div255 v
        where fi :: Word8 -> Word32
              fi = fromIntegral
              v = fi coverage * 255
                + fi backgroundAlpha * fi inverseCoverage + 128

  {-# INLINE alphaOver #-}
  alphaOver coverage inverseCoverage background painted =
      fromIntegral $ div255 v
    where fi :: Word8 -> Word32
          fi = fromIntegral
          v = fi coverage * fi painted + fi background * fi inverseCoverage + 128


toWord8 :: Float -> Int
{-# INLINE toWord8 #-}
toWord8 r = floor $ r * 255 + 0.5

compositionDestination :: (Pixel px, Modulable (PixelBaseComponent px))
                       => Compositor px
compositionDestination c _ _ = colorMap (modulate c)

compositionAlpha :: (Pixel px, Modulable (PixelBaseComponent px))
                 => Compositor px
{-# INLINE compositionAlpha #-}
compositionAlpha c ic
    | c == emptyValue = const
    | c == fullValue = \_ n -> n
    | otherwise = \bottom top ->
        let bottomOpacity = pixelOpacity bottom
            alphaOut = alphaCompose c ic bottomOpacity (pixelOpacity top)
            colorComposer _ back fore =
                alphaOver c ic (back `modulate` bottomOpacity) fore
                    `modiv` alphaOut
        in
        mixWithAlpha colorComposer (\_ _ -> alphaOut) bottom top

emptyPx :: (RenderablePixel px) => px
-- | Really need a "builder" function for pixel
emptyPx = colorMap (const emptyValue) $ unpackPixel 0

