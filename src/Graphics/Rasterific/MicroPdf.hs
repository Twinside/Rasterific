{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rasterific.MicroPdf where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable, foldMap )
import Data.Monoid( mempty )
import Control.Applicative( pure )
#endif

import Control.Monad.State( State )
import Data.Monoid( (<>) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.ByteString.Builder( byteString
                              , intDec
                              , floatDec
                              {-, charUtf8-}
                              , toLazyByteString
                              , Builder )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Shading
import Text.Printf


type PdfCommand = B.ByteString
type PdfPage = B.ByteString
type Outlines = B.ByteString
type PdfId = Int
type PdfContent = B.ByteString

class ToPdf a where
  toPdf :: a -> Builder

instance ToPdf B.ByteString where
  toPdf = byteString

data PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: !(M.Map B.ByteString B.ByteString)
  , _pdfStream   :: !B.ByteString
  }

buildToStrict :: Builder -> B.ByteString
buildToStrict = LB.toStrict . toLazyByteString

instance ToPdf (M.Map B.ByteString B.ByteString) where
  toPdf dic = tp "<< " <> foldMap dicToPdf (M.assocs dic) <> tp ">>\n"
    where
      tp = toPdf :: B.ByteString -> Builder
      dicToPdf (k, el) =
        tp "/" <> toPdf k <> tp " " <> toPdf el <> tp "\n"

instance ToPdf PdfObject where
  toPdf obj = intDec (_pdfId obj)
           <> tp " "
           <> intDec (_pdfRevision obj)
           <> tp " obj\n"
           <> toPdf dic
           <> tp "stream\n"
           <> toPdf (_pdfStream obj)
           <> tp "\nendstream\n"
    where
      tp = toPdf :: B.ByteString -> Builder
      bSize = buildToStrict . intDec . B.length $ _pdfStream obj
      dic = M.insert "Length" bSize $ _pdfAnnot obj

glength :: Foldable f => f a -> Int
glength = F.foldr (const (+ 1)) 0

outlinesObject :: Foldable f => PdfId -> f Outlines -> PdfObject
outlinesObject pid outlines = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = M.fromList
        [ ("Types", "/Outlines")
        , ("Count", buildToStrict . intDec $ glength outlines)
        ]
  , _pdfStream   = mempty
  }

pagesObject :: Foldable f => PdfId -> f PdfPage -> PdfObject
pagesObject pid outlines = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = M.fromList
        [ ("Types", "/Pages")
        , ("Kids", buildToStrict $ tp "[ " <> foldMap kidsToBuild outlines <> tp "]")
        , ("Count", buildToStrict . intDec $ glength outlines)
        ]
  , _pdfStream   = mempty
  }
  where
    tp = toPdf :: B.ByteString -> Builder
    kidsToBuild k = toPdf k <> tp " "

{-pageObject :: PdfId -}

instance ToPdf Point where
  toPdf (V2 x y) = floatDec x <> tp " " <> floatDec y
    where tp = toPdf :: B.ByteString -> Builder

instance ToPdf Primitive where
  toPdf p = case p of
    LinePrim (Line _p0 p1) -> tp p1 <> " l"
    BezierPrim (Bezier _p0 p1 p2) ->
      tp p1 <> " " <> tp p1 <> " " <> tp p2 <> " c"
    CubicBezierPrim (CubicBezier _p0 p1 p2 p3) ->
      tp p1 <> " " <> tp p2 <> " " <> tp p3 <> " c"
    where
      tp = toPdf

instance Foldable f => ToPdf (f Primitive) where
  toPdf ps = case F.toList ps of
    [] -> mempty
    p:_ -> toPdf (firstPointOf p) <> foldMap toPdf ps

colorToPdf :: PixelRGBA8 -> PdfCommand
colorToPdf (PixelRGBA8 r g b _a) =
    toNum r <> " " <> toNum g <> " " <> toNum b
  where
    toNum c = 
      B.pack . printf "%g" $ (fromIntegral c / 255 :: Float)

textureToPdf :: Texture PixelRGBA8 -> State PdfObject (Either String PdfCommand)
textureToPdf tex = case tex of
  RawTexture img -> textureToPdf (SampledTexture img)
  SolidTexture px ->
    -- at this point we(re only doing "fill" operations.
    pure . pure $ colorToPdf px <> " rg"
  LinearGradientTexture _grad _line ->
    fail "Unsupported linear gradient in PDF output."
  RadialGradientTexture _grad _center _radius ->
    fail "Unsupported radial gradient in PDF output."
  RadialGradientWithFocusTexture _grad _center _rad _focus ->
    fail "Unsupported radial gradient with focus in PDF output."
  WithSampler _ tx -> textureToPdf tx
  WithTextureTransform _trans tx -> textureToPdf tx
  SampledTexture _img -> fail "Unsupported raw image in PDF output."
  ShaderTexture  _f -> fail "Unsupported shader function in PDF output."
  ModulateTexture _tx _modulation -> fail "Unsupported modulation in PDF output."

