{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Rasterific.PackeableWrite( PackeablePixel( .. )
                                         ) where

import Control.Monad.ST( ST )
import Data.Bits( (.|.), unsafeShiftL )
import Data.Word( Word16, Word32, Word64 )
{-import qualified Foreign as F-}
{-import System.IO.Unsafe( unsafePerformIO )-}
import Foreign.ForeignPtr( castForeignPtr )
import qualified Data.Vector.Storable.Mutable as M
import Codec.Picture.Types( Pixel( componentCount )
                          , Pixel8
                          , Pixel16
                          , Pixel32
                          , PixelF
                          , PixelYA8   ( .. )
                          , PixelYA16  ( .. )
                          , PixelRGBA8 ( .. )
                          , PixelRGBA16( .. )
                          , PixelCMYK8 ( .. )
                          , PixelCMYK16( .. )
                          , MutableImage( mutableImageData )
                          )

class PackeablePixel a where
    type PackedRepresentation a
    packPixel :: a -> PackedRepresentation a

instance PackeablePixel Pixel8 where
    type PackedRepresentation Pixel8 = Pixel8
    packPixel = id
    {-# INLINE packPixel #-}

instance PackeablePixel Pixel16 where
    type PackedRepresentation Pixel16 = Pixel16
    packPixel = id
    {-# INLINE packPixel #-}

instance PackeablePixel Pixel32 where
    type PackedRepresentation Pixel32 = Pixel32
    packPixel = id
    {-# INLINE packPixel #-}

instance PackeablePixel PixelF where
    type PackedRepresentation PixelF = PixelF
    packPixel = id
    {-# INLINE packPixel #-}


instance PackeablePixel PixelRGBA8 where
    type PackedRepresentation PixelRGBA8 = Word32
    packPixel (PixelRGBA8 r g b a) =
        (fi r `unsafeShiftL` (3 * bitCount)) .|.
        (fi g `unsafeShiftL` (2 * bitCount)) .|.
        (fi b `unsafeShiftL` (1 * bitCount)) .|.
        (fi a `unsafeShiftL` (0 * bitCount))
      where fi = fromIntegral
            bitCount = 8

instance PackeablePixel PixelRGBA16 where
    type PackedRepresentation PixelRGBA16 = Word64
    packPixel (PixelRGBA16 r g b a) =
        (fi r `unsafeShiftL` (3 * bitCount)) .|.
        (fi g `unsafeShiftL` (2 * bitCount)) .|.
        (fi b `unsafeShiftL` (1 * bitCount)) .|.
        (fi a `unsafeShiftL` (0 * bitCount))
      where fi = fromIntegral
            bitCount = 16

instance PackeablePixel PixelCMYK8 where
    type PackedRepresentation PixelCMYK8 = Word32
    packPixel (PixelCMYK8 c m y k) =
        (fi c `unsafeShiftL` (3 * bitCount)) .|.
        (fi m `unsafeShiftL` (2 * bitCount)) .|.
        (fi y `unsafeShiftL` (1 * bitCount)) .|.
        (fi k `unsafeShiftL` (0 * bitCount))
      where fi = fromIntegral
            bitCount = 8

instance PackeablePixel PixelCMYK16 where
    type PackedRepresentation PixelCMYK16 = Word64
    packPixel (PixelCMYK16 c m y k) =
        (fi c `unsafeShiftL` (3 * bitCount)) .|.
        (fi m `unsafeShiftL` (2 * bitCount)) .|.
        (fi y `unsafeShiftL` (1 * bitCount)) .|.
        (fi k `unsafeShiftL` (0 * bitCount))
      where fi = fromIntegral
            bitCount = 16

instance PackeablePixel PixelYA16 where
    type PackedRepresentation PixelYA16 = Word32
    packPixel (PixelYA16 y a) =
        (fi y `unsafeShiftL` 16) .|. fi a
      where fi = fromIntegral

instance PackeablePixel PixelYA8 where
    type PackedRepresentation PixelYA8 = Word16
    packPixel (PixelYA8 y a) =
        (fi y `unsafeShiftL` 8) .|. fi a
      where fi = fromIntegral

{-Pixel PixelYCbCr8-}
{-Pixel PixelRGBF-}
{-Pixel PixelRGB16-}
{-Pixel PixelRGB8-}

{-
-- | from data-binary
fromFloat :: (F.Storable word, F.Storable float) => float -> word
fromFloat float = unsafePerformIO $ F.alloca $ \buf -> do
	F.poke (F.castPtr buf) float
	F.peek buf
	-- -}

writePixelBetweenAt :: ( Pixel px, PackeablePixel px
                       , M.Storable (PackedRepresentation px))
                    => Int -> Int -> MutableImage s px -> px
                    -> ST s ()
writePixelBetweenAt start count img px = M.set converted packed
  where
    packed = packPixel px
    pixelData = mutableImageData img
    compCount = componentCount px
    startOffset = start `div` compCount
    packedCount = count `div` compCount

    toSet = M.slice startOffset packedCount pixelData
    (ptr, s, s2) = M.unsafeToForeignPtr toSet
    packedPtr = castForeignPtr ptr
    converted =
        M.unsafeFromForeignPtr
            packedPtr
            (s `div` compCount)
            (s2 `div` compCount)

