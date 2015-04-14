{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rasterific.MicroPdf where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable, foldMap )
import Data.Monoid( mempty )
import Control.Applicative( pure )
#endif

import Control.Monad.State( State )
import Data.Monoid( (<>) )
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Shading
import Text.Printf


type PdfCommand = B.ByteString
type PdfPage = B.ByteString
type Outlines = B.ByteString
type PdfId = Int

type PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: !M.Map B.ByteString PdfCommand
  , _pdfStream   :: !B.ByteString
  }

glength :: Foldable f => f a -> Int
glength = F.foldr (+ 1) 0

outlinesObject :: PdfId -> f Outlines -> PdfObject
outlinesObject pid outlines = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = M.fromList
        [ ("Types", "/Outlines")
        , ("Count", B.pack . printf "%d" $ glength outlines)
        ]
  , _pdfStream   = mempty
  }

pagesObject :: PdfId -> f PdfPage -> PdfObject
pagesObject pid outlines = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = M.fromList
        [ ("Types", "/Pages")
        , ("Kids", "[ " <> foldMap (<> " ") <> "]")
        , ("Count", B.pack . printf "%d" $ glength outlines)
        ]
  , _pdfStream   = mempty
  }

{-pageObject :: PdfId -}

pointToPdf :: Point -> PdfCommand
pointToPdf (V2 x y) =
  B.pack (printf "%4g" x) <> " " <> B.pack (printf "%4g" y)

primToPdf :: Primitive -> PdfCommand
primToPdf p = case p of
  LinePrim (Line _p0 p1) -> pp p1 <> " l"
  BezierPrim (Bezier _p0 p1 p2) ->
    pp p1 <> " " <> pp p1 <> " " <> pp p2 <> " c"
  CubicBezierPrim (CubicBezier _p0 p1 p2 p3) ->
    pp p1 <> " " <> pp p2 <> " " <> pp p3 <> " c"
  where
    pp = pointToPdf

firstPointToPdf :: Primitive -> PdfCommand
firstPointToPdf = pointToPdf . firstPointOf

colorToPdf :: PixelRGBA8 -> PdfCommand
colorToPdf (PixelRGBA8 r g b _a) =
    toNum r <> " " <> toNum g <> " " <> toNum b
  where
    toNum c = 
      B.pack . printf "%g" $ (fromIntegral c / 255 :: Float)

primsToPdf :: Foldable t => t Primitive -> PdfCommand
primsToPdf = foldMap primToPdf

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

