{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rasterific.MicroPdf( drawOrdersToPdf ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable, foldMap )
import Data.Monoid( mempty )
import Control.Applicative( (<$>), pure )
#endif

import Control.Monad.State( State, runState )
import Data.Monoid( (<>) )
import qualified Data.Foldable as F
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
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.QuadraticBezier
import Graphics.Rasterific.Line
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Operators
import Text.Printf


type PdfCommand = B.ByteString
type Outlines = B.ByteString
type PdfId = Int

class ToPdf a where
  toPdf :: a -> Builder

instance ToPdf B.ByteString where
  toPdf = byteString

data PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: ![(B.ByteString, B.ByteString)]
  , _pdfStream   :: !B.ByteString
  }

buildToStrict :: Builder -> B.ByteString
buildToStrict = LB.toStrict . toLazyByteString

instance ToPdf [(B.ByteString, B.ByteString)] where
  toPdf [] = mempty
  toPdf dic = tp "<< " <> foldMap dicToPdf dic <> tp ">>\n"
    where
      dicToPdf (k, el) =
        tp "/" <> toPdf k <> tp " " <> toPdf el <> tp "\n"

tp :: B.ByteString -> Builder
tp = toPdf

instance ToPdf PdfObject where
  toPdf obj = intDec (_pdfId obj)
           <> tp " "
           <> intDec (_pdfRevision obj)
           <> tp " obj\n"
           <> toPdf dic
           <> stream
           <> tp "endobj\n"
    where
      bSize = buildToStrict . intDec . B.length $ _pdfStream obj
      hasntStream = B.null $ _pdfStream obj

      dic
        | hasntStream = _pdfAnnot obj
        | otherwise = _pdfAnnot obj <> [("Length", bSize)]

      stream
        | hasntStream && null dic = tp "[/PDF]\n"
        | hasntStream = mempty
        | otherwise = tp "stream\n"
                   <> toPdf (_pdfStream obj)
                   <> tp "\nendstream\n"

glength :: Foldable f => f a -> Int
glength = F.foldr (const (+ 1)) 0

outlinesObject :: Foldable f => PdfId -> f Outlines -> PdfObject
outlinesObject pid outlines = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    =
        [ ("Type", "/Outlines")
        , ("Count", buildToStrict . intDec $ glength outlines)
        ]
  , _pdfStream   = mempty
  }

pagesObject :: Foldable f => PdfId -> f PdfId -> PdfObject
pagesObject pid pages = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    =
        [ ("Type", "/Pages")
        , ("Kids", buildToStrict $ tp "[ " <> foldMap kidsToBuild pages <> tp "]")
        , ("Count", buildToStrict . intDec $ glength pages)
        ]
  , _pdfStream   = mempty
  }
  where kidsToBuild idx = intDec idx <> tp " 0 R "

pdfSignature :: B.ByteString
pdfSignature = "%PDF-1.4\n"

catalogObject :: PdfId -> PdfId -> PdfId -> PdfObject
catalogObject pid pagesId outlineId = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    =
        [ ("Type", "/Catalog")
        , ("Outlines", buildToStrict $ intDec outlineId <> " 0 R")
        , ("Pages", buildToStrict $ intDec pagesId <> " 0 R")
        ]
  , _pdfStream   = mempty
  }


pageObject :: Int -> Int -> PdfId -> PdfId -> PdfId -> PdfId -> PdfObject
pageObject width height pid parentId contentId resourceId = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    =
        [ ("Type", "/Page")
        , ("Parent", buildToStrict $ intDec parentId <> " 0 R")
        , ("MediaBox", buildToStrict box)
        , ("Contents", buildToStrict $ intDec contentId <> " 0 R")
        , ("Resources", buildToStrict $ tp "<< /ProcSet " <> intDec resourceId <> tp " 0 R >>")
        ]
  , _pdfStream   = mempty
  }
  where
    box = tp "[0 0 " <> intDec width <> tp " " <> intDec height <> tp "]"

contentObject :: PdfId -> B.ByteString -> PdfObject
contentObject pid content = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = []
  , _pdfStream   = content
  }

instance ToPdf Point where
  toPdf (V2 x y) = floatDec x <> tp " " <> floatDec y

instance ToPdf Bezier where
  toPdf = toPdf . cubicFromQuadraticBezier 

instance ToPdf CubicBezier where
  toPdf (CubicBezier _p0 p1 p2 p3) =
     toPdf p1 <> tp " " <> toPdf p2 <> tp " " <> toPdf p3 <> tp " c\n"

instance ToPdf Line where
  toPdf (Line _p0 p1) = toPdf p1 <> tp " l\n"

instance ToPdf Primitive where
  toPdf p = case p of
    LinePrim l -> toPdf l
    BezierPrim b -> toPdf b
    CubicBezierPrim c -> toPdf c

pathToPdf :: Int -> [Primitive] -> Builder
pathToPdf height ps = case ps of
    [] -> mempty
    p:_ ->
      toPdf (vswap $ firstPointOf p) <> tp " m\n" <> foldMap (toPdf . verticalSwap) ps <> "\n"
  where
    vswap (V2 x y) = V2 x (fromIntegral height - y)
    verticalSwap = transform vswap

colorToPdf :: PixelRGBA8 -> PdfCommand
colorToPdf (PixelRGBA8 r g b _a) =
    toNum r <> " " <> toNum g <> " " <> toNum b
  where
    toNum c = 
      B.pack . printf "%g" $ (fromIntegral c / 255 :: Float)

textureToPdf :: Texture PixelRGBA8 -> PdfEnv (Either String PdfCommand)
textureToPdf tex = case tex of
  RawTexture img -> textureToPdf (SampledTexture img)
  SolidTexture px ->
    -- at this point we(re only doing "fill" operations.
    pure . pure $ colorToPdf px <> " rg\n"
  LinearGradientTexture _grad _line ->
    return $ Left "Unsupported linear gradient in PDF output."
  RadialGradientTexture _grad _center _radius ->
    return $ Left "Unsupported radial gradient in PDF output."
  RadialGradientWithFocusTexture _grad _center _rad _focus ->
    return $ Left "Unsupported radial gradient with focus in PDF output."
  WithSampler _ tx -> textureToPdf tx
  WithTextureTransform _trans tx -> textureToPdf tx
  SampledTexture _img -> return $ Left "Unsupported raw image in PDF output."
  ShaderTexture  _f -> return $ Left "Unsupported shader function in PDF output."
  ModulateTexture _tx _modulation -> return $ Left "Unsupported modulation in PDF output."

type PdfEnv = State PdfContext

data PdfContext = PdfContext {}

resplit :: [Primitive] -> [[Primitive]]
resplit = uncurry (:) . go where
  go [] = ([], [])
  go (x:xs@(y:_)) | firstPointOf y `isDistingableFrom` lastPointOf x =
      ([x], after:rest) where (after, rest) = go xs
  go (x:xs) = (x:curr, rest) where (curr, rest) = go xs

fillCommandOfOrder :: DrawOrder a -> Builder
fillCommandOfOrder = go . _orderFillMethod where
  go FillWinding = tp "f\n"
  go FillEvenOdd = tp "f*\n"


isPrimitivePoint :: Primitive -> Bool
isPrimitivePoint p = case p of
  LinePrim l -> isLinePoint l
  BezierPrim b -> isBezierPoint b
  CubicBezierPrim c -> isCubicBezierPoint c

removeDegeneratePrimitive :: [Primitive] -> [Primitive]
removeDegeneratePrimitive = filter (not . isPrimitivePoint)

orderToPdf :: Int -> DrawOrder PixelRGBA8 -> PdfEnv Builder
orderToPdf height order = do
  etx <- textureToPdf $ _orderTexture order
  let processPath =
        foldMap (pathToPdf height) . resplit . removeDegeneratePrimitive
      geometryCode =
        foldMap processPath $ _orderPrimitives order
  case etx of
    Left _ -> pure mempty
    Right tx -> pure $ toPdf tx <> geometryCode <> fillCommandOfOrder order

buildXRefTable :: [Int] -> Builder
buildXRefTable lst = tp "xref\n0 " <> intDec (glength lst) <> tp "\n"
                   <> foldMap build lst where
  build 0 = "0000000000 65535 f\n"
  build ix = toPdf . B.pack $ printf "%010d 00000 n\n" ix

buildTrailer :: Foldable f => f a -> PdfId -> Builder
buildTrailer objs startId =
    tp "trailer\n<< /Size " <> intDec (glength objs + 1)
            <> tp "\n /Root " <> intDec startId <> tp " 0 R\n"
            <> tp ">>\n"

drawOrdersToPdf :: Int -> Int -> [DrawOrder PixelRGBA8] -> LB.ByteString
drawOrdersToPdf width height orders = toLazyByteString $
  foldMap byteString objs
    <> xref
    <> buildTrailer objects catalogId
    <> xrefPosition 
    <> tp "%%EOF"
  where
  (catalogId : outlineId : pagesId : pageId : contentId : endObjId : _firstFreeId :  _) = [1..]
  (content, _endContext) =
      runState (F.fold <$> mapM (orderToPdf height) orders) $ PdfContext {}

  objects =
    [ catalogObject  catalogId pagesId outlineId
    , outlinesObject outlineId []
    , pagesObject    pagesId [pageId]
    , pageObject     width height pageId pagesId contentId endObjId
    , contentObject  contentId $ buildToStrict content
    , contentObject  endObjId mempty
    ]

  (indexes, objs) = unzip $ prepareObjects objects
  lastIndex = last indexes
  xrefIndex = lastIndex + B.length (last objs)

  xrefPosition = "startxref\n" <> intDec xrefIndex <> tp "\n"

  xref = buildXRefTable indexes


prepareObjects :: [PdfObject] -> [(Int, B.ByteString)]
prepareObjects = scanl go (0, pdfSignature) where
  go (ix, prev) obj = (ix + B.length prev, buildToStrict $ toPdf obj)

