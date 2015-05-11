{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Rasterific.MicroPdf( renderDrawingToPdf
                                   , renderOrdersToPdf ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable, foldMap, fold )
import Data.Monoid( mempty )
import Control.Applicative( (<$>), pure )
#endif

import Control.Monad.Free( liftF, Free( .. ) )
import Control.Monad.Free.Church( fromF )
import Control.Monad.State( StateT, get, put, runStateT, modify, execState )
import Control.Monad.Reader( Reader, local, asks, runReader )

import Numeric( showFFloat )
import Data.Monoid( (<>) )
import qualified Data.Foldable as F
import Data.ByteString.Builder( byteString
                              , intDec
                              , toLazyByteString
                              , Builder )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Command
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Line
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Operators
import Graphics.Rasterific.PlaneBoundable
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PathWalker
import Graphics.Rasterific.ComplexPrimitive
import Graphics.Text.TrueType( Dpi )
import Text.Printf
{-import Debug.Trace-}


type PdfCommand = B.ByteString
type PdfId = Int

data PdfObject = PdfObject
  { _pdfId       :: !PdfId
  , _pdfRevision :: !PdfId
  , _pdfAnnot    :: ![(B.ByteString, B.ByteString)]
  , _pdfStream   :: !B.ByteString
  }

data PdfConfiguration = PdfConfiguration
  { _pdfConfDpi     :: !Dpi
  , _pdfWidth       :: !Int
  , _pdfHeight      :: !Int
  , _pdfConfToOrder :: Drawing PixelRGBA8 () -> [DrawOrder PixelRGBA8]
  }

domainOfLinearGradient :: Point -> Point -> Line -> (Float, Float)
domainOfLinearGradient mini maxi (Line p1 p2) =
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

data PdfContext = PdfContext
  { _pdfFreeIndex        :: !Int
  , _pdfPatternIndex     :: !Int
  , _generatedPdfObjects :: ![PdfObject]
  , _patternAssoc        :: ![(B.ByteString, B.ByteString)]
  }

withLocalSubcontext :: PdfEnv a -> PdfEnv (a, PdfId)
withLocalSubcontext sub = do
  ctxt <- get
  let oldAssocs = _patternAssoc ctxt
  put ctxt { _patternAssoc = [] }
  result <- sub
  ctxt' <- get
  put $ ctxt { _patternAssoc = oldAssocs }
  resource <- generateObject (resourceObject (_patternAssoc ctxt'))
  return (result, resource)

shadingName :: PdfEnv Builder
shadingName = do
  ctxt <- get
  let idx = _pdfPatternIndex ctxt
  put $ ctxt { _pdfPatternIndex = idx + 1 }
  return $ tp "Sh" <> intDec idx

patternName :: PdfEnv Builder
patternName = do
  ctxt <- get
  let idx = _pdfPatternIndex ctxt
  put $ ctxt { _pdfPatternIndex = idx + 1 }
  return $ tp "P" <> intDec idx

namePatternObject :: PdfEnv Builder -> PdfId -> PdfEnv (Builder, (B.ByteString, B.ByteString))
namePatternObject namer pid = do
  patId <- namer
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

instance ToPdf Float where
  toPdf v = toPdf . B.pack $ showFFloat (Just 4) v ""

instance ToPdf B.ByteString where
  toPdf = byteString

instance ToPdf Transformation where
  toPdf (Transformation a c e b d f) =
     foldMap t [a, b, c, d, e, f] <> tp " cm\n"
    where
      t v = toPdf v <> tp " "

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

arrayOfB :: B.ByteString -> B.ByteString
arrayOfB a = "[ " <> a <> " ]"

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
  , ("Count", buildToStrict . intDec $ F.length outlines)
  ]

pagesObject :: Foldable f => f PdfId -> PdfId -> PdfObject
pagesObject pages = dicObj
  [ ("Type", "/Pages")
  , ("Kids", buildToStrict . arrayOf $ foldMap (toPdf . refOf) pages)
  , ("Count", buildToStrict . intDec $ F.length pages)
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

linearGradientObject :: Line -> Float -> Float -> PdfId -> PdfId -> PdfObject
linearGradientObject (Line p1 p2) beg end funId = dicObj
  [ ("ShadingType", "2")
  , ("ColorSpace", "/DeviceRGB")
  , ("Coords", buildToStrict coords)
  , ("Function", refOf funId)
  , ("Domain", buildToStrict . arrayOf $ toPdf beg <> tp " " <> toPdf end)
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
    coords = arrayOf $ toPdf center <> tp " " <> toPdf radius
                    <> " " <> toPdf focus <> tp " 0"

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

colorToPdf :: PixelRGBA8 -> PdfCommand
colorToPdf (PixelRGBA8 r g b _a) = buildToStrict $
    toNum r <> tp " " <> toNum g <> tp " " <> toNum b
  where
    toNum c = toPdf (fromIntegral c / 255 :: Float)

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
       arrayOf $ foldMap (\i -> toPdf (refOf i) <> tp " ") interpolations
    boundsId = arrayOf . foldMap (toPdf . snd) $ init bounds

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

tillingPattern :: Int -> Int -> Builder -> PdfId -> PdfId -> PdfObject
tillingPattern w h content res pid = PdfObject 
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
      ]
  }

gradientToPdf :: Gradient PixelRGBA8 -> PdfEnv PdfId
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
  SamplerRepeat -> generateObject $ repeatingFunction False beg end fun
  SamplerReflect -> generateObject $ repeatingFunction True beg end fun

gradientObjectGenerator :: Float -> Float -> SamplerRepeat -> Gradient PixelRGBA8
                        -> (PdfId -> PdfId -> PdfObject)
                        -> PdfEnv (Either String PdfCommand)
gradientObjectGenerator beg end sampler grad generator = do
  shaderId <- gradientToPdf grad
  stitched <- repeatFunction sampler beg end shaderId
  gradId <- generateObject (generator stitched)
  patternId <- generateObject (gradientPatternObject gradId)
  (pat, _patAssoc) <- namePatternObject shadingName patternId
  return . Right . buildToStrict $
      tp "/Pattern cs\n" <> pat <> tp " scn\n"


createLinearGradient :: SamplerRepeat -> Gradient PixelRGBA8 -> Line
                     -> PdfEnv (Either String PdfCommand)
createLinearGradient sampler grad line = do
    width <- asks _pdfWidth
    height <- asks _pdfHeight
    let (beg, end) = sampledDomainOf $ domainOfLinearGradient (V2 0 0)
                          (V2 (fromIntegral width) (fromIntegral height))
                          line
        sampledLine = extendLine beg end $ transform (vs height) line
 
    gradientObjectGenerator beg end sampler grad $ linearGradientObject sampledLine beg end
  where
    vs h (V2 x y) = V2 x (fromIntegral h - y)
    sampledDomainOf (beg, end) = case sampler of
      SamplerPad -> (0, 1)
      SamplerRepeat -> (beg, end)
      SamplerReflect -> (beg, end)

textureToPdf :: PlaneBound -> Texture PixelRGBA8 -> PdfEnv (Either String PdfCommand)
textureToPdf _pBounds = go SamplerPad where
  vs h (V2 x y) = V2 x (fromIntegral h - y)

  go sampler tex = case tex of
    RawTexture img -> go sampler (SampledTexture img)
    WithSampler newSampler tx -> go newSampler tx
    SolidTexture px -> pure . pure $ colorToPdf px <> " rg\n"
                                  <> colorToPdf px <> " RG\n"
    LinearGradientTexture grad line -> createLinearGradient sampler grad line
    RadialGradientTexture grad center radius ->
       go sampler $ RadialGradientWithFocusTexture grad center radius center
    RadialGradientWithFocusTexture grad center rad focus -> do
      height <- asks _pdfHeight
      let cs = vs height center
          fs = vs height focus
          invGrad = reverse [(1 - o, c) | (o, c) <- grad]
      gradientObjectGenerator 0 0 sampler invGrad (radialGradientObject cs fs rad)
    WithTextureTransform _trans tx -> go sampler tx

    SampledTexture _img -> return $ Left "Unsupported raw image in PDF output."
    ShaderTexture  _f -> return $ Left "Unsupported shader function in PDF output."
    ModulateTexture _tx _modulation -> return $ Left "Unsupported modulation in PDF output."
    PatternTexture w h px draw _img -> do
      let withPatternSize conf = conf { _pdfWidth = w, _pdfHeight = h }
          baseTexture = SolidTexture px
          backRect = rectangle (V2 0 0) (fromIntegral w) (fromIntegral h)
          backDraw =
            liftF $ SetTexture baseTexture
               (liftF $ Fill FillWinding backRect ()) ()
      (content, resId) <-
          local withPatternSize . withLocalSubcontext $ pdfProducer baseTexture (backDraw >> draw)
      tillingId <- generateObject $ tillingPattern w h content resId
      (pat, _patAssoc) <- namePatternObject patternName tillingId
      return . Right . buildToStrict $
            tp "/Pattern cs\n" <> pat <> tp " scn\n"

resplit :: [Primitive] -> [[Primitive]]
resplit = uncurry (:) . go where
  go [] = ([], [])
  go (x:xs@(y:_)) | lastPointOf x `isDistingableFrom` firstPointOf y =
      ([x], after:rest) where (after, rest) = go xs
  go (x:xs) = (x:curr, rest) where (curr, rest) = go xs

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
  JoinMiter 0 -> tp "2 j "
  JoinMiter n -> toPdf n <> tp " M 0 j "

orderToPdf :: DrawOrder PixelRGBA8 -> PdfEnv Builder
orderToPdf order = do
  let orderBounds = foldMap (foldMap planeBounds) $ _orderPrimitives order
  etx <- textureToPdf orderBounds $ _orderTexture order
  let processPath = foldMap pathToPdf . resplit -- . removeDegeneratePrimitive
      geometryCode =
        foldMap processPath $ _orderPrimitives order
  case etx of
    Left _ -> pure mempty
    Right tx -> pure $ toPdf tx <> geometryCode <> fillCommandOf (_orderFillMethod order)

buildXRefTable :: [Int] -> Builder
buildXRefTable lst = tp "xref\n0 " <> intDec (F.length lst) <> tp "\n"
                   <> foldMap build lst where
  build 0 = "0000000000 65535 f \n"
  build ix = toPdf . B.pack $ printf "%010d 00000 n \n" ix

buildTrailer :: Foldable f => f a -> PdfId -> Builder
buildTrailer objs startId = tp "trailer\n" <> toPdf
  [("Size" :: B.ByteString, buildToStrict . intDec $ F.length objs + 1)
  ,("Root", refOf startId)
  ]

toPdfSpace :: Float -> Transformation
toPdfSpace h = translate (V2 0 h) <> scale 1 (-1)

pdfFromProducer :: PdfConfiguration -> PdfEnv Builder -> LB.ByteString
pdfFromProducer conf producer = toLazyByteString $
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
    , pageObject     (_pdfWidth conf) height pagesId contentId endObjId pageId
    , contentObject  (buildToStrict $ initialTransform <> content) contentId
    , resourceObject (_patternAssoc endContext) endObjId
    ]
    <> reverse (_generatedPdfObjects endContext)

  (indexes, objs) = unzip $ prepareObjects objects
  lastIndex = last indexes
  xrefIndex = lastIndex + B.length (last objs)

  xrefPosition = "startxref\n" <> intDec xrefIndex <> tp "\n"

  xref = buildXRefTable indexes

renderDrawingToPdf :: (Drawing PixelRGBA8 () -> [DrawOrder PixelRGBA8])
                   -> Int -> Int -> Dpi -> Drawing PixelRGBA8 ()
                   -> LB.ByteString
renderDrawingToPdf toOrders width height dpi =
    pdfFromProducer conf . pdfProducer baseTexture
  where
    baseTexture = SolidTexture $ PixelRGBA8 0 0 0 0
    conf = PdfConfiguration
        { _pdfConfDpi     = dpi
        , _pdfWidth       = width
        , _pdfHeight      = height
        , _pdfConfToOrder = toOrders
        }

pdfProducer :: Texture PixelRGBA8 -> Drawing PixelRGBA8 () -> PdfEnv Builder
pdfProducer baseTexture =
    goNext False fillCommandOf baseTexture . fromF where

  goNext forceInverse filler prevTexture f = case f of
    Free c -> go forceInverse filler prevTexture c
    Pure () -> pure mempty

  go forceInverse filler prevTexture com = case com of
     Fill method prims next -> do
       after <- recurse next
       pure $ foldMap pathToPdf (resplit prims)
            <> filler method
            <> after
     Stroke w j (c, _) prims next -> do
       after <- recurse next
       pure $ toPdf w <> tp " w "
            <> lineJoinOf j
            <> lineCapOf  c <> "\n"
            <> foldMap pathToPdf (resplit prims)
            <> tp "S\n"
            <> after
     
     DashedStroke o pat w j (c, _) prims next -> do
       sub <- go forceInverse filler prevTexture (Stroke w j (c, c) prims next)
       pure $ arrayOf (foldMap coords pat) 
           <> toPdf o <> tp " d "
           <> sub
           <> "[] 0 d "
       where
         coords co = toPdf co <> tp " "
     
     -- Opacity is ignored for now
     WithGlobalOpacity _opacity sub next ->
       (<>) <$> recurse (fromF sub) <*> recurse next
     WithImageEffect _f sub next ->
       (<>) <$> recurse (fromF sub) <*> recurse next

     WithTransform trans sub next | forceInverse -> do
        after <- recurse next
        inner <- recurse $ fromF sub
        let inv = foldMap toPdf $ inverseTransformation trans
        pure $ toPdf trans <> inner <> inv <> after

     WithTransform trans sub next -> do
        after <- recurse next
        inner <- recurse $ fromF sub
        pure $ localGraphicState (toPdf trans <> inner) <> after

     SetTexture tx sub next -> do
        tex <- textureToPdf mempty tx
        inner <- goNext forceInverse filler tx $ fromF sub
        after <- recurse next
        pure $ case tex of
           Left _ -> inner <> after
           Right texCode ->
             localGraphicState (tp texCode <> inner) <> after

     WithCliping clipping sub next -> do
        after <- recurse next
        let draw8 = clipping :: Drawing PixelRGBA8 ()
        clipPath <- goNext True clipCommandOf prevTexture (fromF draw8)
        drawing <- recurse (fromF sub)
        pure $ localGraphicState (clipPath <> tp "\n" <> drawing)
            <> after

     TextFill p ranges next -> do
        dpi <- asks _pdfConfDpi
        after <- recurse next
        let orders = textToDrawOrders dpi prevTexture p ranges
        textPrint <- mapM orderToPdf orders
        pure $ F.fold textPrint <> after

     WithPathOrientation path base subDrawings next -> do
       toOrders <- asks _pdfConfToOrder
       let orders :: [DrawOrder PixelRGBA8]
           orders = toOrders . liftF $ SetTexture prevTexture subDrawings ()

           drawer trans _ order =
             modify (liftF (WithTransform trans (orderToDrawing order) ()) :)

           placedDrawings :: [Drawing PixelRGBA8 ()]
           placedDrawings =
             reverse $ execState (drawOrdersOnPath drawer 0 base path orders) []
       after <- recurse next
       this <- recurse . fromF $ F.fold placedDrawings
       pure $ this <> after

    where
      recurse = goNext forceInverse filler prevTexture

renderOrdersToPdf :: (Drawing PixelRGBA8 () -> [DrawOrder PixelRGBA8])
                  -> Int -> Int -> Dpi -> [DrawOrder PixelRGBA8] -> LB.ByteString
renderOrdersToPdf toOrders width height dpi orders =
  pdfFromProducer conf $ F.fold <$> mapM orderToPdf orders
  where
    conf = PdfConfiguration
      { _pdfConfDpi     = dpi
      , _pdfWidth       = width
      , _pdfHeight      = height
      , _pdfConfToOrder = toOrders
      }

prepareObjects :: [PdfObject] -> [(Int, B.ByteString)]
prepareObjects = scanl go (0, pdfSignature) where
  go (ix, prev) obj = (ix + B.length prev, buildToStrict $ toPdf obj)

