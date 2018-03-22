{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Rasterific.MicroPdf( renderDrawingToPdf
                                   , renderOrdersToPdf
                                   ) where

import Control.Monad.Free( liftF, Free( .. ) )
import Control.Monad.Free.Church( fromF )
import Control.Monad.State( StateT, get, put, runStateT, modify, execState )
import Control.Monad.Reader( Reader, local, asks, runReader )

import Numeric( showFFloat )
import Data.Monoid( (<>) )
import qualified Data.Foldable as F
import Data.Word( Word32 )
import Data.ByteString.Builder( byteString
                              , intDec
                              , toLazyByteString
                              , word32BE
                              , word8
                              , Builder )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Codec.Picture( PixelRGBA8( PixelRGBA8 )
                    , Pixel8
                    , Pixel
                    , PixelBaseComponent
                    , pixelOpacity
                    , mixWithAlpha
                    )

import Graphics.Rasterific.MiniLens( Lens', use, (.^), (.=), (+=), (%=) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Command
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.PlaneBoundable
import Graphics.Rasterific.Line
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PathWalker
import Graphics.Rasterific.ComplexPrimitive
import Graphics.Rasterific.Patch
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.MeshPatch
import Graphics.Text.TrueType( Dpi )
import Text.Printf
{-import Debug.Trace-}

glength :: Foldable f => f a -> Int
glength = F.length

type PdfCommand = B.ByteString
type PdfId = Int

data PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: !Resources
  , _pdfStream   :: !B.ByteString
  }

instance Eq PdfObject where
  obj1 == obj2 =
    (_pdfAnnot obj1, _pdfStream obj1) == (_pdfAnnot obj2, _pdfStream obj2)

instance Ord PdfObject where
  compare obj1 obj2 =
    compare (_pdfAnnot obj1, _pdfStream obj1) (_pdfAnnot obj2, _pdfStream obj2)

type InnerRenderer =
    forall px . PdfColorable px => Drawing px () -> [DrawOrder px]

data PdfConfiguration = PdfConfiguration
  { _pdfConfDpi     :: !Dpi
  , _pdfWidth       :: !Int
  , _pdfHeight      :: !Int
  , _pdfConfToOrder :: InnerRenderer
  }

domainOfCircle :: Point -> Float -> (Point, Point) -> Domain
domainOfCircle center radius (mini, maxi) = (0, max d1 d2 / radius)
  where
   d1 = distance maxi center
   d2 = distance mini center

domainOfLinearGradient :: Line -> (Point, Point) -> (Float, Float)
domainOfLinearGradient (Line p1 p2) (mini, maxi) =
    (t0 + xxAdd + yxAdd, t0 + xyAdd + yyAdd)
  where
    {-
     * Linear gradients are othrogonal to the line passing through
     * their extremes. Because of convexity, the parameter range can
     * be computed as the convex hull (one the real line) of the
     * parameter values of the 4 corners of the box.
     *
     * The parameter value t for a point (x,y) can be computed as:
     *
     *   t = (p2 - p1) . (x,y) / |p2 - p1|^2
     *
     * t0  is the t value for the top left corner
     * tdx is the difference between left and right corners
     * tdy is the difference between top and bottom corners
     -}
    delta = p2 ^-^ p1
    invSquareNorm = 1 / quadrance delta

    normDelta = delta ^* invSquareNorm

    t0 = (mini ^-^ p1) `dot` normDelta
    V2 tdx tdy = (maxi ^-^ mini) * normDelta

    (xxAdd, xyAdd) | tdx < 0 = (tdx, 0)
                   | otherwise = (0, tdx)
    (yxAdd, yyAdd) | tdy < 0 = (tdy, 0)
                   | otherwise = (0, tdy)

--------------------------------------------------
----       Monadic generation types
--------------------------------------------------
type PdfEnv = StateT PdfContext (Reader PdfConfiguration)

runPdfEnv :: PdfConfiguration -> PdfId -> PdfEnv a -> (a, PdfContext)
runPdfEnv conf firstFreeId producer =
  runReader (runStateT producer $ emptyContext firstFreeId) conf 

type Resources = [(B.ByteString, B.ByteString)]

data PdfResourceAssoc = PdfResourceAssoc
  { _resFreeIndex :: !Int
  , _resAssoc     :: !Resources
  }

resFreeIndex :: Lens' PdfResourceAssoc Int
resFreeIndex f v = setter <$> f (_resFreeIndex v) where
  setter new = v { _resFreeIndex = new }

resAssoc :: Lens' PdfResourceAssoc Resources
resAssoc f v = setter <$> f (_resAssoc v) where
  setter new = v { _resAssoc = new }

data PdfContext = PdfContext
  { _pdfFreeIndex        :: !Int
  , _generatedPdfObjects :: ![PdfObject]
  , _pdfPatterns         :: !PdfResourceAssoc
  , _pdfShadings         :: !PdfResourceAssoc
  , _pdfGraphicStates    :: !PdfResourceAssoc
  , _pdfXObjects         :: !PdfResourceAssoc
  }

pdfXObjects :: Lens' PdfContext PdfResourceAssoc
pdfXObjects f v = setter <$> f (_pdfXObjects v) where
  setter new = v { _pdfXObjects = new }

pdfPatterns :: Lens' PdfContext PdfResourceAssoc
pdfPatterns f v = setter <$> f (_pdfPatterns v) where
  setter new = v { _pdfPatterns = new }

pdfShadings :: Lens' PdfContext PdfResourceAssoc
pdfShadings f v = setter <$> f (_pdfShadings v) where
  setter new = v { _pdfShadings = new }

pdfGraphicStates :: Lens' PdfContext PdfResourceAssoc
pdfGraphicStates f v = setter <$> f (_pdfGraphicStates v) where
  setter new = v { _pdfGraphicStates = new }

isPixelTransparent :: (Modulable (PixelBaseComponent px), Pixel px) => px -> Bool
isPixelTransparent p = pixelOpacity p < fullValue

isGradientTransparent :: (Modulable (PixelBaseComponent px), Pixel px) => Gradient px -> Bool
isGradientTransparent = F.any (isPixelTransparent . snd)

toAlphaGradient :: Pixel px => Gradient px -> Gradient (PixelBaseComponent px)
toAlphaGradient = fmap extractOpacity where
  extractOpacity (o, p) = (o, pixelOpacity p)
 
toOpaqueGradient :: RenderablePixel px => Gradient px -> Gradient px
toOpaqueGradient = fmap (\(o, p) -> (o, mixWithAlpha pxId pxOpaq p p)) where
  pxId _ _ v = v
  pxOpaq _ _ = fullValue

withLocalSubcontext :: PdfEnv a -> PdfEnv (a, PdfId)
withLocalSubcontext sub = do
  oldShadings <- reset (pdfShadings.resAssoc) []
  oldPatterns <- reset (pdfPatterns.resAssoc) []
  oldStates <- reset (pdfGraphicStates.resAssoc) []
  oldXObjects <- reset (pdfXObjects.resAssoc) []

  result <- sub

  newShadings <- reset (pdfShadings.resAssoc) oldShadings
  newStates <- reset (pdfGraphicStates.resAssoc) oldStates
  newPatterns <- reset (pdfPatterns.resAssoc) oldPatterns
  newXObjects <- reset (pdfXObjects.resAssoc) oldXObjects 

  (result,) <$> generateObject (resourceObject newShadings newStates newPatterns newXObjects)
  where
    reset :: Lens' PdfContext a -> a -> PdfEnv a
    reset l old = do
      v <- use l
      l .= old
      return v

nameObject :: B.ByteString -> Lens' PdfContext PdfResourceAssoc -> B.ByteString -> PdfEnv Builder
nameObject prefix lens info = do
  idx <- use (lens.resFreeIndex)
  lens.resFreeIndex += 1
  let key = buildToStrict $ tp prefix <> intDec idx
  lens.resAssoc %= ((key, info) :)
  return . tp $ "/" <> key

nameStateObject :: PdfId -> PdfEnv Builder
nameStateObject = nameObject "gs" pdfGraphicStates . refOf

nameOpacityObject :: Float -> PdfEnv Builder
nameOpacityObject opa = nameObject "gs" pdfGraphicStates opac where
  opb = toPdf opa
  opac = buildToStrict $ "<< /ca " <> opb <> " /CA " <> opb <> ">> "

nameXObject :: PdfId -> PdfEnv Builder
nameXObject = nameObject "x" pdfXObjects . refOf

{-nameShadingObject :: PdfId -> PdfEnv Builder-}
{-nameShadingObject = nameObject "Sh" pdfShadings . refOf-}

namePatternObject :: B.ByteString -> PdfEnv Builder
namePatternObject = nameObject "P" pdfPatterns

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
  , _generatedPdfObjects = mempty
  , _pdfPatterns = emptyAssoc
  , _pdfShadings = emptyAssoc
  , _pdfGraphicStates = emptyAssoc
  , _pdfXObjects = emptyAssoc
  }
  where
    emptyAssoc = PdfResourceAssoc
        { _resFreeIndex = 1
        , _resAssoc     = mempty
        }



--------------------------------------------------
----            ToPdf class & instances
--------------------------------------------------
class ToPdf a where
  toPdf :: a -> Builder

instance ToPdf Float where
  toPdf v = toPdf . B.pack $ showFFloat (Just 4) v ""

instance ToPdf B.ByteString where
  toPdf = byteString

newtype Matrix = Matrix Transformation

instance ToPdf Transformation where
  toPdf (Transformation a c e b d f) =
     foldMap t [a, b, c, d, e, f] <> tp " cm\n"
    where
      t v = toPdf v <> tp " "

instance ToPdf Matrix where
  toPdf (Matrix (Transformation a c e b d f)) =
     arrayOf $ foldMap t [a, b, c, d, e, f]
    where
      t v = toPdf v <> tp " "

instance ToPdf Resources where
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
  toPdf (V2 x y) = toPdf x <> tp " " <> toPdf y

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

instance PdfColorable px => ToPdf (V2 Double, V2 Float, V2 Float, TensorPatch (ParametricValues px)) where
  toPdf (V2 sx sy, V2 dx dy, V2 _tx ty, patch) = word8 0 <> coords <> foldMap colorToBinaryPdf [c00, c03, c33, c30] where
    fx x = floor . max 0 . min maxi $ realToFrac (x + dx) * sx
    fy y = floor . max 0 . min maxi $ realToFrac (ty - (y + dy)) * sy

    maxi = fromIntegral (maxBound :: Word32)

    coords = foldMap word32BE
       [ fx x00, fy y00, fx x01, fy y01, fx x02, fy y02, fx x03, fy y03
       , fx x13, fy y13, fx x23, fy y23, fx x33, fy y33, fx x32, fy y32
       , fx x31, fy y31, fx x30, fy y30, fx x20, fy y20, fx x10, fy y10
       , fx x11, fy y11, fx x12, fy y12, fx x22, fy y22, fx x21, fy y21 ]

    CubicBezier (V2 x00 y00) (V2 x10 y10) (V2 x20 y20) (V2 x30 y30) = _curve0 patch
    CubicBezier (V2 x01 y01) (V2 x11 y11) (V2 x21 y21) (V2 x31 y31) = _curve1 patch
    CubicBezier (V2 x02 y02) (V2 x12 y12) (V2 x22 y22) (V2 x32 y32) = _curve2 patch
    CubicBezier (V2 x03 y03) (V2 x13 y13) (V2 x23 y23) (V2 x33 y33) = _curve3 patch
    param = _tensorValues patch

    c00 = _northValue param
    c30 = _eastValue param
    c33 = _southValue param
    c03 = _westValue param

--------------------------------------------------
----            Helper functions
--------------------------------------------------
buildToStrict :: Builder -> B.ByteString
buildToStrict = LB.toStrict . toLazyByteString

tp :: B.ByteString -> Builder
tp = toPdf

pdfSignature :: B.ByteString
pdfSignature = "%PDF-1.4\n%\xBF\xF7\xA2\xFE\n"

refOf :: PdfId -> B.ByteString
refOf i = buildToStrict $ intDec i <> " 0 R"

arrayOf :: Builder -> Builder
arrayOf a = tp "[ " <> a <> tp " ]"

localGraphicState :: Builder -> Builder
localGraphicState sub = tp "q\n" <> sub <> tp "Q\n"

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

pageObject :: PdfColorable px
           => Proxy px -> Int -> Int -> PdfId -> PdfId -> PdfId -> PdfId -> PdfObject
pageObject px width height parentId contentId resourceId = dicObj
  [ ("Type", "/Page")
  , ("Parent", refOf parentId)
  , ("MediaBox", buildToStrict box)
  , ("Contents", refOf contentId)
  , ("Resources", refOf resourceId)
  , ("Group", buildToStrict . toPdf $ groupDic px)
  ]
  where
    box = tp "[0 0 " <> intDec width <> tp " " <> intDec height <> tp "]"

gradientPatternObject :: Transformation -> PdfId -> PdfId -> PdfObject
gradientPatternObject trans gradientId = dicObj
  [ ("Type", "/Pattern")
  , ("PatternType", "2")
  , ("Matrix", it)
  , ("Shading", refOf gradientId)
  ]
  where
    it = buildToStrict . toPdf $ Matrix trans

linearGradientObject :: Line -> Domain -> B.ByteString -> PdfId -> PdfId -> PdfObject
linearGradientObject (Line p1 p2) (beg, end) colorSpace funId = dicObj
  [ ("ShadingType", "2")
  , ("ColorSpace", colorSpace)
  , ("Coords", buildToStrict coords)
  , ("Function", refOf funId)
  , ("Domain", buildToStrict . arrayOf $ toPdf beg <> tp " " <> toPdf end)
  , ("Extend", "[true true]")
  ]
  where
    coords = arrayOf $ toPdf p1 <> tp " " <> toPdf p2

radialGradientObject :: Domain -> Point -> Point -> Float -> B.ByteString -> PdfId
                     -> PdfId -> PdfObject
radialGradientObject (beg, end) center focus radius colorSpace funId = dicObj
  [ ("ShadingType", "3")
  , ("ColorSpace", colorSpace)
  , ("Coords", buildToStrict coords)
  , ("Function", refOf funId)
  , ("Domain", buildToStrict . arrayOf $ toPdf beg <> tp " " <> toPdf end)
  , ("Extend", "[true true]")
  ]
  where
    coords = arrayOf $ toPdf center <> tp " " <> toPdf radius
                    <> " " <> toPdf focus <> tp " 0"

meshGradientObject :: PdfColorable px => MeshPatch px -> Int -> PdfId -> PdfObject
meshGradientObject mesh height pid = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    =
      [ ("ShadingType", "7")
      , ("ColorSpace", "/DeviceRGB")
      , ("BitsPerComponent", "8")
      , ("BitsPerCoordinate", "32")
      , ("BitsPerFlag", "8")
      , ("Decode", B.pack $ printf "[%g %g %g %g 0 1 0 1 0 1]" 
                                     x0 x1 (fromIntegral height - y1)
                                     (fromIntegral height - y0))
      ]
  , _pdfStream = buildToStrict
               . foldMap (\patch -> toPdf (scal, transl, fullSize, patch))
               $ tensorPatchesOf mesh
  }
  where
    maxi = fromIntegral (maxBound :: Word32)
    scaleOf :: Float -> Float -> Double
    scaleOf a b | nearZero $ a - b = 0
                | otherwise = maxi / (realToFrac b - realToFrac a)

    fullSize = V2 (x1 - x0) (y1 - y0)
    transl = V2 (-x0) (-y0)
    scal = V2 (scaleOf x0 x1) (scaleOf y0 y1)
    PlaneBound (V2 x0 y0) (V2 x1 y1) =
      foldMeshPoints (\v -> mappend v . planeBounds) mempty mesh

createMeshGradient :: forall px. PdfBaseColorable px
                   => Builder -> MeshPatch px -> PdfEnv (Either String Builder)
createMeshGradient inner mesh = do
  height <- asks _pdfHeight      
  meshId <- generateObject $ meshGradientObject mesh height 
  patId <- generateObject (gradientPatternObject mempty meshId)
  pat <- namePatternObject $ refOf patId
  pure . pure $
    "/Pattern cs\n" <> pat <> " scn\n" <>
    "/Pattern CS\n" <> pat <> " SCN\n" <> inner


contentObject :: B.ByteString -> PdfId -> PdfObject
contentObject content pid = PdfObject
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfAnnot    = []
  , _pdfStream   = content
  }

pathToPdf :: [Primitive] -> Builder
pathToPdf ps = case ps of
    [] -> mempty
    p:_ ->
      toPdf (firstPointOf p) <> tp " m\n" <> foldMap toPdf ps <> "\n"

class RenderablePixel px => PdfColorable px where
  pdfColorSpace :: Proxy px -> B.ByteString
  colorToPdf :: px -> Builder
  colorToBinaryPdf :: px -> Builder

instance PdfColorable Pixel8 where
  pdfColorSpace _ = "/DeviceGray"
  colorToPdf c = toPdf (fromIntegral c / 255 :: Float)
  colorToBinaryPdf = word8

instance PdfColorable PixelRGBA8 where
  pdfColorSpace _ = "/DeviceRGB"
  colorToPdf (PixelRGBA8 r g b _a) = 
     colorToPdf r <> tp " " <> colorToPdf g <> tp " " <> colorToPdf b
  colorToBinaryPdf (PixelRGBA8 r g b _a) = 
     colorToBinaryPdf r <> colorToBinaryPdf g <> colorToBinaryPdf b


maskObject :: PdfId -> PdfId -> PdfObject
maskObject maskId = dicObj
  [ ("Type", "/Mask")
  , ("S", "/Luminosity")
  , ("G", refOf maskId)
  ]

alphaMaskObject :: PdfId -> PdfId -> PdfObject
alphaMaskObject maskId = dicObj
  [ ("Type", "/Mask")
  , ("S", "/Alpha")
  , ("G", refOf maskId)
  ]


opaState :: Float -> PdfId -> PdfObject
opaState opa = dicObj
  [ ("Type", "/ExtGState")
  , ("ca", v)
  , ("CA", v)
  ]
  where v = buildToStrict $ toPdf opa

maskState :: PdfId -> PdfId -> PdfObject
maskState maskObj = dicObj
  [ ("Type", "/ExtGState")
  , ("SMask", refOf maskObj)
  , ("ca", "1")
  , ("CA", "1")
  , ("AIS", "false")
  ]

colorInterpolationFunction :: PdfColorable px => px -> px -> PdfId -> PdfObject
colorInterpolationFunction c0 c1 = dicObj
  [ ("FunctionType", "2")
  , ("Domain", "[ 0 1 ]")
  , ("C0", buildToStrict . arrayOf $ colorToPdf c0)
  , ("C1", buildToStrict . arrayOf $ colorToPdf c1)
  , ("N", "1")
  ]

resourceObject :: Resources -> Resources -> Resources -> Resources
               -> PdfId -> PdfObject
resourceObject shadings extStates patterns xobjects= dicObj $
  ("ProcSet", buildToStrict . arrayOf $ tp "/PDF /Text") :
       genExt "ExtGState" (("ao", "<< /ca 1 /CA 1 >>") : extStates)
    <> genExt "Pattern" patterns
    <> genExt "Shading" shadings
    <> genExt "XObject" xobjects
  where
  genExt _ [] = []
  genExt k lst = [(k, buildToStrict $ toPdf lst)]

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
       arrayOf $ foldMap (\i -> toPdf (refOf i) <> tp " ") interpolations
    boundsId = arrayOf . foldMap ((<> " ") . toPdf . snd) $ init bounds

repeatingFunction :: Bool -> Float -> Float -> PdfId -> PdfId -> PdfObject
repeatingFunction reflect begin end fun = dicObj
  [ ("FunctionType", "3")
  , ("Domain", buildToStrict . arrayOf $ intDec ibegin <> tp " " <> intDec iend)
  , ("Functions", buildToStrict interpIds)
  , ("Bounds", buildToStrict $ arrayOf boundsIds)
  , ("Encode", buildToStrict . arrayOf $ foldMap encoding [ibegin .. iend - 1])
  ]
  where
    ibegin = floor begin
    iend = ceiling end
    interpIds =
       arrayOf $ foldMap (\_ -> toPdf (refOf fun) <> tp " ") [ibegin .. iend - 1]
    boundsIds =
       foldMap ((<> tp " ") . intDec) [ibegin + 1 .. iend - 1]
    encoding i | i `mod` 2 /= 0 && reflect = tp "1 0 "
               | otherwise = tp "0 1 "

tillingPattern :: Transformation -> Int -> Int -> Builder -> PdfId -> PdfId -> PdfObject
tillingPattern trans w h content res pid = PdfObject 
  { _pdfId       = pid
  , _pdfRevision = 0
  , _pdfStream   = buildToStrict content
  , _pdfAnnot    =
      [ ("Type", "/Pattern")
      , ("PatternType", "1")
      , ("PaintType", "1")
      , ("TilingType", "1")
      , ("BBox", buildToStrict $ "[0 0 " <> intDec w <> tp " " <> intDec h <> "]")
      , ("XStep", buildToStrict $ intDec w)
      , ("YStep", buildToStrict $ intDec h)
      , ("Resources", refOf res)
      , ("Matrix", buildToStrict . toPdf $ Matrix trans)
      ]
  }

groupDic :: PdfColorable px => Proxy px -> [(B.ByteString, B.ByteString)]
groupDic px =
  [ ("Type", "/Group")
  , ("S", "/Transparency")
  , ("I", "true")
  , ("CS", pdfColorSpace px)
  ]


formObject :: PdfColorable px
           => Resources -> Proxy px -> B.ByteString -> PdfId
           -> PdfEnv (PdfId -> PdfObject)
formObject aditionalAttributes px content res = do
  width <- intDec <$> asks _pdfWidth
  height <- intDec <$> asks _pdfHeight
  pure $ \pid -> PdfObject
    { _pdfId       = pid
    , _pdfRevision = 0
    , _pdfStream   = content
    , _pdfAnnot    =
        [ ("Type", "/XObject")
        , ("Subtype", "/Form")
        , ("BBox", buildToStrict $ "[0 0 " <> width <> tp " " <> height <> "]")
        , ("XStep", buildToStrict width)
        , ("YStep", buildToStrict height)
        , ("Resources", refOf res)
        , ("Group", buildToStrict . toPdf $ groupDic px)
        ] <> aditionalAttributes
    }

gradientToPdf :: PdfColorable px => Gradient px -> PdfEnv PdfId
gradientToPdf [] = return 0
gradientToPdf [(_, a), (_, b)] = generateObject (colorInterpolationFunction a b)
gradientToPdf lst@(_:rest) = do
  interpolations <-
     mapM generateObject [colorInterpolationFunction a b
                            | ((_, a), (_, b)) <- zip lst rest]
  let bounds = zip (map fst lst) (map fst rest)
  generateObject (stitchingFunction interpolations bounds)

repeatFunction :: SamplerRepeat -> Float -> Float -> PdfId -> PdfEnv PdfId
repeatFunction sampler beg end fun = case sampler of
  SamplerPad -> pure fun
  _ | abs (ceiling end - floor beg) <= (1 :: Int) -> pure fun
  SamplerRepeat -> generateObject $ repeatingFunction False beg end fun
  SamplerReflect -> generateObject $ repeatingFunction True beg end fun

type Domain = (Float, Float)

createGradientFunction :: PdfColorable px
                       => Transformation -> Domain -> SamplerRepeat -> Gradient px
                       -> (PdfId -> PdfId -> PdfObject)
                       -> PdfEnv PdfId
createGradientFunction trans (beg, end) sampler grad generator = do
  shaderId <- gradientToPdf grad
  stitched <- repeatFunction sampler beg end shaderId
  gradId <- generateObject (generator stitched)
  generateObject (gradientPatternObject trans gradId)

type PdfBaseColorable px =
  ( PdfColorable px
  , PdfColorable (PixelBaseComponent px)
  , Integral (PixelBaseComponent px)
  , PixelBaseComponent (PixelBaseComponent px) ~ (PixelBaseComponent px))

fullPageFill :: PdfEnv Builder
fullPageFill = do
  w <- asks _pdfWidth
  h <- asks _pdfHeight
  pure $ "0 0 " <> intDec w <> " " <> intDec h <> " re f\n"

{-  
+------------+
| Color   {c}|<---------\
| interp n   |          |
+------------+          |
                        |
   * * *                |
                        |
+------------+        +-+---------+    +------------+    +------------+     /-------------\
| Color   {c}|<-------+ Stitching |<---+ Repeat  {c}|<---+ Gradient   |<----+ Page     {r}|
| interp n   |        | fun    {c}|    | function   |    |         {c}|     | resources   |
+------------+        +-----------+    +------------+    +------------+     \-----+-------/
                                                                                  |
                                                                                  v
           Gradient with alpha PDF generation                               +-------------+
           (yes this is quite complex)                                      | ExtGState   |
                                                                            | SMask    {a}|
                                                                            +-----+-------+
                                                                                  |
                                                                                  v
                                                                            +-------------+
                                                                            | Mask        |
                                                                            |          {a}|
                                                                            +-----+-------+
                                                                                  |
                                                                                  v
+------------+        +-----------+    +------------+    +------------+     +--------------+
| Color   {a}|<-------+ Stitching |<---+ Repeat  {a}|<---+ Gradient   |<----+ Form with    |
| interp 0   |        | fun    {a}|    | function   |    |         {a}|     | transparency |
+------------+        +-+---------+    +------------+    +------------+     | group     {a}|
                        |                                                   +--------------+
   * * *                |
                        |
+------------+          |
| Color   {a}|<---------/
| interp n   |
+------------+

::: .a { fill: white; }
::: .r { fill: rgb(128, 200, 128); }
-}
gradientObjectGenerator :: forall px. PdfBaseColorable px
                        => Builder -> Transformation
                        -> Domain -> SamplerRepeat -> Gradient px
                        -> (B.ByteString -> PdfId -> PdfId -> PdfObject)
                        -> PdfEnv (Either String Builder)
gradientObjectGenerator inner rootTrans dom sampler rootGrad generator
  | isGradientTransparent rootGrad = goAlpha rootGrad
  | otherwise = go rootTrans rootGrad
  where
    alphaPxProxy = Proxy :: Proxy (PixelBaseComponent px)
    alphaColorspace = pdfColorSpace alphaPxProxy
    pxFullProxy = Proxy :: Proxy px
    colorSpace = pdfColorSpace pxFullProxy

    go trans grad = do
      patternId <- createGradientFunction trans dom sampler grad $ generator colorSpace
      pat <- namePatternObject $ refOf patternId
      pure . pure $
        "/Pattern cs\n" <> pat <> " scn\n" <>
        "/Pattern CS\n" <> pat <> " SCN\n" <> inner

    goAlpha grad = do
      let alphaGrad = toAlphaGradient grad
      (colorGradCom, xObjectRes) <-
          withLocalSubcontext . go mempty $ toOpaqueGradient grad
      alphaId <- createGradientFunction mempty dom sampler alphaGrad $ generator alphaColorspace

      (command, resourceId) <- withLocalSubcontext $ do
          alphaShadingName <- namePatternObject $ refOf alphaId
          opaDicId <- generateObject $ opaState 1
          gsName <-  nameStateObject opaDicId
          fullFill <- fullPageFill
          pure . buildToStrict $ gsName <> " gs /Pattern cs " <> alphaShadingName <> " scn\n"
                              <> fullFill
      let subInfo = either (const mempty) buildToStrict colorGradCom
      formId <- generateObject =<< formObject [("FormType", "1")] alphaPxProxy command resourceId
      xObjectGenerator <- formObject [] pxFullProxy subInfo xObjectRes
      xObjName <- nameXObject  =<< generateObject xObjectGenerator
      maskId <- generateObject $ maskObject formId
      maskGraphicStateId <- generateObject $ maskState maskId
      stateName <- nameStateObject maskGraphicStateId
      pure . pure . localGraphicState $ stateName <> " gs\n" <> xObjName <> " Do\n"

alphaLayerGenerator :: forall px. PdfBaseColorable px
                    => Proxy px -> (Builder, PdfId) -> Float -> PdfEnv Builder
alphaLayerGenerator pxFullProxy (inner, innerResource) alpha = go where
  generateFill = withLocalSubcontext $do
    fill <- fullPageFill 
    shade <- nameOpacityObject alpha
    let co = colorToPdf (emptyPx :: px)
    pure . buildToStrict $ co <> " rg\n" <> co <> " RG\n" <> shade <> " gs " <> fill <> " " 

  go = do
    (transpCall, layerRes) <- generateFill
    formId <- generateObject =<< formObject mempty pxFullProxy transpCall layerRes
    maskId <- generateObject $ alphaMaskObject formId
    maskName <- nameStateObject =<< generateObject (maskState maskId)

    xObjId <- generateObject =<< formObject [] pxFullProxy (buildToStrict inner) innerResource
    xObjName <- nameXObject xObjId
    pure . localGraphicState $ maskName <> tp " gs\n" <> xObjName <> tp " Do\n"

sampledDomainOf :: SamplerRepeat -> Domain -> Domain 
sampledDomainOf _ (beg, end) | abs (beg - end) <= 1 = (0, 1)
sampledDomainOf sampler (beg, end) = case sampler of
  SamplerPad -> (0, 1)
  SamplerRepeat -> (beg, end)
  SamplerReflect -> (beg, end)

currentViewBox :: Transformation -> PdfEnv (Point, Point)
currentViewBox trans = do
  width <- asks $ fromIntegral . _pdfWidth
  height <- asks $ fromIntegral . _pdfHeight
  let pMin = V2 0 0
      pMax = V2 width height
      fitBounds t = (applyTransformation t pMin, applyTransformation t pMax)
  pure . maybe (pMin, pMax) fitBounds $ inverseTransformation trans

createLinearGradient :: forall px. PdfBaseColorable px
                     => Builder -> Transformation -> SamplerRepeat -> Gradient px -> Line
                     -> PdfEnv (Either String Builder)
createLinearGradient inner trans sampler grad line = do
  baseDomain <- domainOfLinearGradient line <$> currentViewBox trans
  let dom@(beg, end) = sampledDomainOf sampler baseDomain
      sampledLine = extendLine beg end line
  gradientObjectGenerator inner trans dom sampler grad $
      linearGradientObject sampledLine dom

createRadialGradient :: forall px. PdfBaseColorable px
                     => Builder -> Transformation -> SamplerRepeat -> Gradient px
                     -> Point -> Point -> Float
                     -> PdfEnv (Either String Builder)
createRadialGradient inner trans sampler grad center focus radius = do
    baseDomain <- domainOfCircle center radius <$> currentViewBox trans
    let dom@(beg, end) = sampledDomainOf sampler baseDomain
        radius' = radius * max (abs beg) (abs end)
    gradientObjectGenerator inner trans dom sampler grad $
        radialGradientObject dom center focus radius'

opacityToPdf :: forall n. (Integral n, Modulable n) => n -> Float
opacityToPdf comp = fromIntegral comp / fromIntegral fv where
  fv = fullValue :: n


textureToPdf :: forall px. PdfBaseColorable px
             => Transformation -> Builder -> Texture px
             -> PdfEnv (Either String Builder)
textureToPdf rootTrans inner = go rootTrans SamplerPad where
  go currTrans sampler tex = case tex of
    SampledTexture _img -> return $ Left "Unsupported raw image in PDF output."
    ShaderTexture  _f -> return $ Left "Unsupported shader function in PDF output."
    ModulateTexture _tx _modulation -> return $ Left "Unsupported modulation in PDF output."
    AlphaModulateTexture _tx _modulation -> return $ Left "Unsupported alpha modulation in PDF output."
    RawTexture img -> go currTrans sampler (SampledTexture img)
    WithSampler newSampler tx -> go currTrans newSampler tx
    SolidTexture px | isPixelTransparent px -> do
      localState <- nameOpacityObject . opacityToPdf $ pixelOpacity px
      pure . pure . localGraphicState $
          localState <> " gs\n" <> co <> " rg\n" <> co <> " RG\n" <> inner
        where co = colorToPdf px
    SolidTexture px ->
      pure . pure $ "/ao gs " <> co <> " rg\n" <> co <> " RG\n" <> inner
        where co = colorToPdf px
    MeshPatchTexture _ mesh -> createMeshGradient inner mesh
    LinearGradientTexture grad line -> createLinearGradient inner currTrans sampler grad line
    RadialGradientTexture grad center radius ->
       go currTrans sampler $ RadialGradientWithFocusTexture grad center radius center
    RadialGradientWithFocusTexture grad center rad focus -> do
      let invGrad = reverse [(1 - o, c) | (o, c) <- grad]
      createRadialGradient inner currTrans sampler invGrad center focus rad
    WithTextureTransform trans tx ->
        go tt sampler tx
      where tt = case inverseTransformation trans of
              Nothing -> currTrans
              Just v -> currTrans <> v
    PatternTexture w h px draw _img -> do
      let withPatternSize conf = conf { _pdfWidth = w, _pdfHeight = h }
          baseTexture = SolidTexture px
          backRect = rectangle (V2 0 0) (fromIntegral w) (fromIntegral h)
          backDraw =
            liftF $ SetTexture baseTexture
               (liftF $ Fill FillWinding backRect ()) ()
      (content, resId) <-
          local withPatternSize . withLocalSubcontext $ pdfProducer baseTexture (backDraw >> draw)
      tillingId <- generateObject $ tillingPattern rootTrans w h (content) resId
      pat <- namePatternObject $ refOf tillingId
      return . Right $ "/Pattern cs\n" <> pat <> " scn\n" <> inner

reClose :: [Primitive] -> Builder
reClose [] = mempty
reClose lst@(x:_)
  | lastPointOf (last lst) `isDistingableFrom` firstPointOf x = mempty
  | otherwise = tp " h\n"

fillCommandOf :: FillMethod -> Builder
fillCommandOf m = tp $ case m of
  FillWinding -> "f\n"
  FillEvenOdd -> "f*\n"

clipCommandOf :: FillMethod -> Builder
clipCommandOf m = tp $ case m of
  FillWinding -> "W n\n"
  FillEvenOdd -> "W* n\n"

lineCapOf :: Cap -> Builder
lineCapOf c = tp $ case c of
  CapStraight 0 -> "0 J "
  CapStraight _g -> "2 J "
  CapRound -> "1 J "

lineJoinOf :: Join -> Builder
lineJoinOf j = case j of
  JoinRound -> tp "1 j "
  JoinMiter 0 -> tp "8 M 0 j "
  JoinMiter n -> toPdf n <> tp " M 0 j "

orderToPdf :: PdfBaseColorable px => Transformation -> DrawOrder px
           -> PdfEnv Builder
orderToPdf trans order = do
  let processPath = foldMap pathToPdf . resplit -- . removeDegeneratePrimitive
      geometryCode = foldMap processPath $ _orderPrimitives order
  etx <- textureToPdf trans geometryCode $ _orderTexture order
  case etx of
    Left _ -> pure mempty
    Right tx -> pure $ tx <> geometryCode <> fillCommandOf (_orderFillMethod order)

buildXRefTable :: [Int] -> Builder
buildXRefTable lst = tp "xref\n0 " <> intDec (glength lst) <> tp "\n"
                   <> foldMap build lst where
  build 0 = "0000000000 65535 f \n"
  build ix = toPdf . B.pack $ printf "%010d 00000 n \n" ix

buildTrailer :: Foldable f => f a -> PdfId -> Builder
buildTrailer objs startId = tp "trailer\n" <> toPdf
  [("Size" :: B.ByteString, buildToStrict . intDec $ glength objs + 1)
  ,("Root", refOf startId)
  ]

toPdfSpace :: Float -> Transformation
toPdfSpace h = translate (V2 0 h) <> scale 1 (-1)

pdfFromProducer :: PdfBaseColorable px
                => Proxy px -> PdfConfiguration -> PdfEnv Builder -> LB.ByteString
pdfFromProducer px conf producer = toLazyByteString $
  foldMap byteString objs
    <> xref
    <> buildTrailer objects catalogId
    <> xrefPosition 
    <> tp "%%EOF"
  where
  height = _pdfHeight conf
  (catalogId : outlineId : pagesId : pageId : contentId : endObjId : firstFreeId :  _) = [1..]
  (content, endContext) = runPdfEnv conf firstFreeId producer
  initialTransform = toPdf . toPdfSpace $ fromIntegral height

  objects =
    [ catalogObject  pagesId outlineId catalogId 
    , outlinesObject [] outlineId
    , pagesObject    [pageId] pagesId
    , pageObject     px (_pdfWidth conf) height pagesId contentId endObjId pageId
    , contentObject  (buildToStrict $ initialTransform <> content) contentId
    , resourceObject
        (endContext .^ pdfShadings.resAssoc)
        (endContext .^ pdfGraphicStates.resAssoc)
        (endContext .^ pdfPatterns.resAssoc)
        (endContext .^ pdfXObjects.resAssoc)
        endObjId
    ]
    <> reverse (_generatedPdfObjects endContext)

  (indexes, objs) = unzip $ prepareObjects objects
  lastIndex = last indexes
  xrefIndex = lastIndex + B.length (last objs)

  xrefPosition = "startxref\n" <> intDec xrefIndex <> tp "\n"

  xref = buildXRefTable indexes

renderDrawingToPdf :: (forall px . PdfColorable px => Drawing px () -> [DrawOrder px])
                   -> Int -> Int -> Dpi -> Drawing PixelRGBA8 ()
                   -> LB.ByteString
renderDrawingToPdf toOrders width height dpi =
    pdfFromProducer px conf . pdfProducer baseTexture
  where
    px = Proxy :: Proxy PixelRGBA8
    baseTexture = SolidTexture emptyPx 
    conf = PdfConfiguration
        { _pdfConfDpi     = dpi
        , _pdfWidth       = width
        , _pdfHeight      = height
        , _pdfConfToOrder = toOrders
        }

pdfProducer :: forall pixel . PdfBaseColorable pixel
            => Texture pixel -> Drawing pixel () -> PdfEnv Builder
pdfProducer baseTexture draw = do
  initTrans <- asks (toPdfSpace . fromIntegral . _pdfHeight)
  goNext False initTrans fillCommandOf baseTexture $ fromF draw where

  goNext :: forall px. PdfBaseColorable px
         => Bool -> Transformation -> (FillMethod -> Builder) -> Texture px
         -> Free (DrawCommand px) ()
         -> PdfEnv Builder
  goNext forceInverse activeTrans filler prevTexture f = case f of
    Free c -> go forceInverse activeTrans filler prevTexture c
    Pure () -> pure mempty

  go :: forall px. PdfBaseColorable px
     => Bool -> Transformation -> (FillMethod -> Builder) -> Texture px
     -> DrawCommand px (Free (DrawCommand px) ()) -> PdfEnv Builder
  go forceInverse activeTrans filler prevTexture com = case com of
     CustomRender _mesh next -> recurse next
     MeshPatchRender i m next -> do
       w <- asks $ fromIntegral . _pdfWidth
       h <- asks $ fromIntegral . _pdfHeight
       let rect = rectangle (V2 0 0) w h
       go forceInverse activeTrans filler prevTexture $
         SetTexture (MeshPatchTexture i m) (liftF $ Fill FillWinding rect ()) next
           
     Fill method prims next -> do
       after <- recurse next
       pure $ foldMap pathToPdf (resplit prims)
            <> filler method
            <> after

     Stroke w j (c, _) prims next -> do
       after <- recurse next
       let output p = pathToPdf p <> reClose p
           stroke = case w of
             0 -> mempty
             _ -> toPdf w <> tp " w "
                          <> lineJoinOf j
                          <> lineCapOf  c <> "\n"
                          <> foldMap output (resplit prims)
                          <> tp "S\n"
       pure $ stroke <> after
     
     DashedStroke o pat w j (c, _) prims next -> do
       sub <- go forceInverse activeTrans filler prevTexture $ Stroke w j (c, c) prims (Pure ())
       after <- recurse next
       pure $ arrayOf (foldMap coords pat) 
           <> toPdf o <> tp " d "
           <> sub
           <> "[] 0 d "
           <> after
       where
         coords co = toPdf co <> tp " "
     
     -- Opacity is ignored for now
     WithGlobalOpacity opacity sub next | opacity >= fullValue ->
       (<>) <$> recurse (fromF sub) <*> recurse next
     WithGlobalOpacity opacity sub next -> do
       inner <- withLocalSubcontext . recurse $ fromF sub
       after <- recurse next
       let alpha = opacityToPdf opacity
           proxy = Proxy :: Proxy px
       (<> after) <$> alphaLayerGenerator proxy inner alpha

     WithImageEffect _f sub next ->
       (<>) <$> recurse (fromF sub) <*> recurse next

     WithTransform trans sub next | forceInverse -> do
        after <- recurse next
        let subTrans = (activeTrans <> trans)
        inner <- goNext forceInverse subTrans filler prevTexture $ fromF sub
        let inv = foldMap toPdf $ inverseTransformation trans
        pure $ toPdf trans <> inner <> inv <> after

     WithTransform trans sub next -> do
        after <- recurse next
        let subTrans = activeTrans <> trans
        inner <- goNext forceInverse subTrans filler prevTexture $ fromF sub
        pure $ localGraphicState (toPdf trans <> inner) <> after

     SetTexture tx sub next -> do
        innerCode <- goNext forceInverse activeTrans filler tx $ fromF sub
        after <- recurse next
        tex <- textureToPdf activeTrans innerCode tx
        pure $ case tex of
           Left _ -> innerCode <> after
           Right texCode -> localGraphicState texCode <> after

     WithCliping clipping sub next -> do
        after <- recurse next
        let draw8 = clipping :: Drawing px ()
            localClip | forceInverse = id
                      | otherwise = localGraphicState
        clipPath <- goNext True activeTrans clipCommandOf prevTexture $ fromF draw8
        drawing <- recurse (fromF sub)
        pure $ localClip (clipPath <> tp "\n" <> drawing)
            <> after

     TextFill p ranges next -> do
        dpi <- asks _pdfConfDpi
        after <- recurse next
        let orders = textToDrawOrders dpi prevTexture p ranges
        textPrint <- mapM (orderToPdf activeTrans) orders
        pure $ F.fold textPrint <> after

     WithPathOrientation path base subDrawings next -> do
       toOrders <- asks _pdfConfToOrder
       let orders :: [DrawOrder px]
           orders = toOrders . liftF $ SetTexture prevTexture subDrawings ()

           drawer trans _ order =
             modify (liftF (WithTransform trans (orderToDrawing order) ()) :)

           placedDrawings :: [Drawing px ()]
           placedDrawings =
             reverse $ execState (drawOrdersOnPath drawer 0 base path orders) []
       after <- recurse next
       this <- recurse . fromF $ F.fold placedDrawings
       pure $ this <> after

    where
      recurse = goNext forceInverse activeTrans filler prevTexture

renderOrdersToPdf :: InnerRenderer -> Int -> Int -> Dpi -> [DrawOrder PixelRGBA8]
                  -> LB.ByteString
renderOrdersToPdf toOrders width height dpi orders =
  pdfFromProducer (Proxy :: Proxy PixelRGBA8) conf $
      F.fold <$> mapM (orderToPdf rootTrans) orders
  where
    rootTrans = toPdfSpace $ fromIntegral height
    conf = PdfConfiguration
      { _pdfConfDpi     = dpi
      , _pdfWidth       = width
      , _pdfHeight      = height
      , _pdfConfToOrder = toOrders
      }

prepareObjects :: [PdfObject] -> [(Int, B.ByteString)]
prepareObjects = scanl go (0, pdfSignature) where
  go (ix, prev) obj = (ix + B.length prev, buildToStrict $ toPdf obj)

