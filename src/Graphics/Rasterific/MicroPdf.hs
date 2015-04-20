{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rasterific.MicroPdf( drawOrdersToPdf ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable, foldMap )
import Data.Monoid( mempty )
import Control.Applicative( (<$>), pure )
#endif

import Control.Monad.State( State, get, put, runState, modify )
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
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Operators
import Graphics.Rasterific.PlaneBoundable
import Text.Printf


type PdfCommand = B.ByteString
type PdfId = Int

data PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: ![(B.ByteString, B.ByteString)]
  , _pdfStream   :: !B.ByteString
  }

--------------------------------------------------
----       Monadic generation types
--------------------------------------------------
type PdfEnv = State PdfContext

data PdfContext = PdfContext
    { _pdfFreeIndex        :: !Int
    , _pdfPatternIndex     :: !Int
    , _generatedPdfObjects :: ![PdfObject]
    , _patternAssoc        :: ![(B.ByteString, B.ByteString)]
    }

generatePattern :: PdfEnv Builder
generatePattern = do
  ctxt <- get
  let idx = _pdfPatternIndex ctxt
  put $ ctxt { _pdfPatternIndex = idx + 1 }
  return $ tp "Sh" <> intDec idx

namePatternObject :: PdfId -> PdfEnv (Builder, (B.ByteString, B.ByteString))
namePatternObject pid = do
  patId <- generatePattern 
  let patAssoc = (buildToStrict patId, refOf pid)
  modify $ \ctxt -> ctxt { _patternAssoc = patAssoc : _patternAssoc ctxt }
  return ("/" <> patId, patAssoc)

generateObject :: (PdfId -> PdfObject) -> PdfEnv PdfId
generateObject f = do
  ctxt <- get
  let idx = _pdfFreeIndex ctxt
  put $ ctxt
    { _pdfFreeIndex = idx + 1
    , _generatedPdfObjects = f idx : _generatedPdfObjects ctxt
    }
  return idx

emptyContext :: PdfId -> PdfContext
emptyContext idx = PdfContext
  { _pdfFreeIndex = idx
  , _pdfPatternIndex = 1
  , _generatedPdfObjects = []
  , _patternAssoc = []
  }


--------------------------------------------------
----            ToPdf class & instances
--------------------------------------------------
class ToPdf a where
  toPdf :: a -> Builder

instance ToPdf B.ByteString where
  toPdf = byteString


instance ToPdf [(B.ByteString, B.ByteString)] where
  toPdf [] = mempty
  toPdf dic = tp "<< " <> foldMap dicToPdf dic <> tp ">> "
    where
      dicToPdf (_, el) | B.null el = mempty
      dicToPdf (k, el) =
        tp "/" <> toPdf k <> tp " " <> toPdf el <> tp "\n"

instance ToPdf PdfObject where
  toPdf obj = intDec (_pdfId obj)
           <> tp " "
           <> intDec (_pdfRevision obj)
           <> tp " obj\n"
           <> toPdf dic <> tp "\n"
           <> stream
           <> tp "endobj\n"
    where
      bSize = buildToStrict . intDec . B.length $ _pdfStream obj
      hasntStream = B.null $ _pdfStream obj

      dic
        | hasntStream = _pdfAnnot obj
        | otherwise = _pdfAnnot obj <> [("Length", bSize)]

      stream
        | hasntStream = mempty
        | otherwise = tp "stream\n"
                   <> toPdf (_pdfStream obj)
                   <> tp "\nendstream\n"

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


--------------------------------------------------
----            Helper functions
--------------------------------------------------
buildToStrict :: Builder -> B.ByteString
buildToStrict = LB.toStrict . toLazyByteString

tp :: B.ByteString -> Builder
tp = toPdf

glength :: Foldable f => f a -> Int
glength = F.foldr (const (+ 1)) 0

pdfSignature :: B.ByteString
pdfSignature = "%PDF-1.4\n%\xBF\xF7\xA2\xFE\n"

refOf :: PdfId -> B.ByteString
refOf i = buildToStrict $ intDec i <> " 0 R"

arrayOf :: Builder -> Builder
arrayOf a = tp "[ " <> a <> tp " ]"

arrayOfB :: B.ByteString -> B.ByteString
arrayOfB a = "[ " <> a <> " ]"

dicObj :: [(B.ByteString, B.ByteString)] -> PdfId -> PdfObject
dicObj annots pid = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = annots
  , _pdfStream   = mempty
  }

--------------------------------------------------
----            PDF object helper
--------------------------------------------------
outlinesObject :: Foldable f => f PdfCommand -> PdfId -> PdfObject
outlinesObject outlines = dicObj
  [ ("Type", "/Outlines")
  , ("Count", buildToStrict . intDec $ glength outlines)
  ]

pagesObject :: Foldable f => f PdfId -> PdfId -> PdfObject
pagesObject pages = dicObj
  [ ("Type", "/Pages")
  , ("Kids", buildToStrict . arrayOf $ foldMap (toPdf . refOf) pages)
  , ("Count", buildToStrict . intDec $ glength pages)
  ]


catalogObject :: PdfId -> PdfId -> PdfId -> PdfObject
catalogObject pagesId outlineId = dicObj
  [ ("Type", "/Catalog")
  , ("Outlines", refOf outlineId)
  , ("Pages", refOf pagesId)
  ]

pageObject :: Int -> Int -> PdfId -> PdfId -> PdfId -> PdfId -> PdfObject
pageObject width height parentId contentId resourceId = dicObj
  [ ("Type", "/Page")
  , ("Parent", refOf parentId)
  , ("MediaBox", buildToStrict box)
  , ("Contents", refOf contentId)
  , ("Resources", refOf resourceId)
  ]
  where
    box = tp "[0 0 " <> intDec width <> tp " " <> intDec height <> tp "]"

gradientPatternObject :: PdfId -> PdfId -> PdfObject
gradientPatternObject gradientId = dicObj
  [ ("Type", "/Pattern")
  , ("PatternType", "2")
  , ("Matrix", "[ 1 0 0 1 0 0 ]")
  , ("Shading", refOf gradientId)
  ]

linearGradientObject :: Line -> PdfId -> PdfId -> PdfObject
linearGradientObject (Line p1 p2) funId = dicObj
  [ ("ShadingType", "2")
  , ("ColorSpace", "/DeviceRGB")
  , ("Coords", buildToStrict coords)
  , ("Function", refOf funId)
  , ("Extend", "[ true true ]")
  ]
  where
    coords = arrayOf $ toPdf p1 <> tp " " <> toPdf p2

radialGradientObject :: Point -> Point -> Float -> PdfId -> PdfId -> PdfObject
radialGradientObject center focus radius funId = dicObj
  [ ("ShadingType", "3")
  , ("ColorSpace", "/DeviceRGB")
  , ("Coords", buildToStrict coords)
  , ("Function", refOf funId)
  , ("Extend", "[ true true ]")
  ]
  where
    coords = arrayOf $ toPdf center <> tp " " <> floatDec radius
                    <> " " <> toPdf focus <> tp " 0"

contentObject :: B.ByteString -> PdfId -> PdfObject
contentObject content pid = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = []
  , _pdfStream   = content
  }

vswap :: Int -> Point -> Point
vswap height (V2 x y) = V2 x (fromIntegral height - y)

pathToPdf :: Int -> [Primitive] -> Builder
pathToPdf height ps = case ps of
    [] -> mempty
    [_] -> mempty
    p:_ ->
      toPdf (vswap height $ firstPointOf p) <> tp " m\n" <> foldMap (toPdf . verticalSwap) ps <> "\n"
  where
    verticalSwap = transform (vswap height)

colorToPdf :: PixelRGBA8 -> PdfCommand
colorToPdf (PixelRGBA8 r g b _a) =
    toNum r <> " " <> toNum g <> " " <> toNum b
  where
    toNum c = 
      B.pack . printf "%g" $ (fromIntegral c / 255 :: Float)

colorInterpolationFunction :: PixelRGBA8 -> PixelRGBA8 -> PdfId -> PdfObject
colorInterpolationFunction c0 c1 = dicObj
  [ ("FunctionType", "2")
  , ("Domain", "[ 0 1 ]")
  , ("C0", arrayOfB $ colorToPdf c0)
  , ("C1", arrayOfB $ colorToPdf c1)
  , ("N", "1")
  ]

resourceObject :: [(B.ByteString, B.ByteString)] -> PdfId -> PdfObject
resourceObject patterns = dicObj
  [ ("Pattern", buildToStrict $ toPdf patterns)
  , ("ProcSet", buildToStrict . arrayOf $ tp "/PDF /Text")
  ]

stitchingFunction :: [PdfId] -> [(Float, Float)] -> PdfId -> PdfObject
stitchingFunction interpolations bounds = dicObj
  [ ("FunctionType", "3")
  , ("Domain", "[ 0 1 ]")
  , ("Functions", buildToStrict interpIds)
  , ("Bounds", buildToStrict boundsId)
  , ("Encode", buildToStrict . arrayOf . F.fold $ map (const $ tp "0 1 ") interpolations)
  ]
  where
    interpIds =
       tp "[ " <> foldMap (\i -> toPdf (refOf i) <> tp " ") interpolations <> tp "]"
    boundEncode (_, b2) = floatDec b2
    boundsId = tp "[ " <> foldMap boundEncode (init bounds) <> tp "]"

{-
repeatingFunction :: PdfId -> PdfObject
repeatingFunction begin end = dicObj
  [ ("FunctionType", "3")
  , ("Domain", buildToStrict . arrayOf $ floatDec begin <> tp " " <> floatDec end)
  , ("Functions", buildToStrict interpIds)
  , ("Bounds", buildToStrict boundsId)
  , ("Encode", buildToStrict . arrayOf . F.fold $ map (const $ tp "0 1 ") interpolations)
  ]
-}

gradientToPdf :: Gradient PixelRGBA8 -> PdfEnv PdfId
gradientToPdf [] = return 0
gradientToPdf [(_, a), (_, b)] = generateObject (colorInterpolationFunction a b)
gradientToPdf lst@(_:rest) = do
  interpolations <-
     mapM generateObject [colorInterpolationFunction a b
                            | ((_, a), (_, b)) <- zip lst rest]
  let bounds = zip (map fst lst) (map fst rest)
  generateObject (stitchingFunction interpolations bounds)

gradientObjectGenerator :: PlaneBound -> SamplerRepeat -> Gradient PixelRGBA8
                        -> (PdfId -> PdfId -> PdfObject)
                        -> PdfEnv (Either String PdfCommand)
gradientObjectGenerator _pBounds _sampler grad gen = do
  shaderId <- gradientToPdf grad
  gradId <- generateObject (gen shaderId)
  patternId <- generateObject (gradientPatternObject gradId)
  (patternName, _patAssoc) <- namePatternObject patternId
  return . Right . buildToStrict $
      tp "/Pattern cs\n" <> patternName <> tp " scn\n"

textureToPdf :: PlaneBound -> Int -> Texture PixelRGBA8
             -> PdfEnv (Either String PdfCommand)
textureToPdf pBounds height = go SamplerPad where
  go sampler tex = case tex of
    RawTexture img -> go sampler (SampledTexture img)
    WithSampler newSampler tx -> go newSampler tx
    SolidTexture px -> pure . pure $ colorToPdf px <> " rg\n"
    LinearGradientTexture grad line -> do
      let shapedLine = transform (vswap height) line
      gradientObjectGenerator pBounds sampler grad (linearGradientObject shapedLine)
    RadialGradientTexture grad center radius ->
       go sampler $ RadialGradientWithFocusTexture grad center radius center
    RadialGradientWithFocusTexture grad center rad focus -> do
      let cs = vswap height center
          fs = vswap height focus
      gradientObjectGenerator pBounds sampler grad (radialGradientObject cs fs rad)
    WithTextureTransform _trans tx -> go sampler tx

    SampledTexture _img -> return $ Left "Unsupported raw image in PDF output."
    ShaderTexture  _f -> return $ Left "Unsupported shader function in PDF output."
    ModulateTexture _tx _modulation -> return $ Left "Unsupported modulation in PDF output."

resplit :: [Primitive] -> [[Primitive]]
resplit = uncurry (:) . go where
  go [] = ([], [])
  go (x:xs@(y:_)) | lastPointOf x `isDistingableFrom` firstPointOf y =
      ([x], after:rest) where (after, rest) = go xs
  go (x:xs) = (x:curr, rest) where (curr, rest) = go xs

fillCommandOfOrder :: DrawOrder a -> Builder
fillCommandOfOrder = go . _orderFillMethod where
  go FillWinding = tp "f\n"
  go FillEvenOdd = tp "f*\n"

orderToPdf :: Int -> DrawOrder PixelRGBA8 -> PdfEnv Builder
orderToPdf height order = do
  let orderBounds = foldMap (foldMap planeBounds) $ _orderPrimitives order
  etx <- textureToPdf orderBounds height $ _orderTexture order
  let processPath =
        foldMap (pathToPdf height) . resplit -- . removeDegeneratePrimitive
      geometryCode =
        foldMap processPath $ _orderPrimitives order
  case etx of
    Left _ -> pure mempty
    Right tx -> pure $ toPdf tx <> geometryCode <> fillCommandOfOrder order

buildXRefTable :: [Int] -> Builder
buildXRefTable lst = tp "xref\n0 " <> intDec (glength lst) <> tp "\n"
                   <> foldMap build lst where
  build 0 = "0000000000 65535 f \n"
  build ix = toPdf . B.pack $ printf "%010d 00000 n \n" ix

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
  (catalogId : outlineId : pagesId : pageId : contentId : endObjId : firstFreeId :  _) = [1..]
  startContext = emptyContext firstFreeId
  (content, endContext) =
      runState (F.fold <$> mapM (orderToPdf height) orders) startContext

  objects =
    [ catalogObject  pagesId outlineId catalogId 
    , outlinesObject [] outlineId
    , pagesObject    [pageId] pagesId
    , pageObject     width height pagesId contentId endObjId pageId
    , contentObject  (buildToStrict content) contentId
    , resourceObject (_patternAssoc endContext) endObjId
    ]
    <> reverse (_generatedPdfObjects endContext)

  (indexes, objs) = unzip $ prepareObjects objects
  lastIndex = last indexes
  xrefIndex = lastIndex + B.length (last objs)

  xrefPosition = "startxref\n" <> intDec xrefIndex <> tp "\n"

  xref = buildXRefTable indexes


prepareObjects :: [PdfObject] -> [(Int, B.ByteString)]
prepareObjects = scanl go (0, pdfSignature) where
  go (ix, prev) obj = (ix + B.length prev, buildToStrict $ toPdf obj)

