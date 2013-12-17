{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Rasterific
    ( Bezier( .. )
    , Texture
    , Compositor
    , DrawContext
    , Modulable
    , uniformTexture
    , renderContext
    , fillBezierShape
    , compositionDestination
    , compositionAlpha
    ) where

import Control.Applicative( Applicative, liftA2, liftA3, (<$>), (<*>) )
import Control.Monad.ST( ST, runST )
import Control.Monad.State( StateT, execStateT, get, lift )
import Codec.Picture.Types( Image( .. )
                          , Pixel( .. )
                          , MutableImage( .. )
                          , createMutableImage
                          , unsafeFreezeImage )
import Data.Bits( unsafeShiftR )
import Data.List( mapAccumL, sortBy )
import Data.Monoid( mappend )
import Data.Word( Word8, Word32 )

import Linear( V2( .. )
             , V1( .. )
             , Additive( .. )
             , (^+^)
             , (^-^)
             , (^/)
             )

type Point = V2 Float

type Texture px = Int -> Int -> px
type Compositor px =
    (PixelBaseComponent px) ->
        (PixelBaseComponent px) -> px -> px -> px

data Bezier = Bezier !Point !Point !Point
  deriving Show

infix  4 ^<, ^<=^, ^<^, ^==^
infixr 3 ^&&^
infixr 2 ^||^

class Ord a => Modulable a where
  clampCoverage :: Float -> (a, a)
  modulate :: a -> a -> a
  alphaOver :: a -> a -> a -> a -> a

instance Modulable Word8 where
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

uniformTexture :: (Pixel px) => px -> Texture px
uniformTexture px _ _ = px

compositionDestination :: (Pixel px, Modulable (PixelBaseComponent px)) => Compositor px
compositionDestination c _ _ a = colorMap (modulate c) $ a

compositionAlpha :: ( Pixel px, Modulable (PixelBaseComponent px)) => Compositor px
compositionAlpha c ic = mixWith (\_ -> alphaOver c ic)

type DrawContext s a px = StateT (MutableImage s px) (ST s) a

renderContext :: (Pixel px)
              => Int -> Int -> px -> (forall s. DrawContext s a px) -> Image px
renderContext width height background drawing = runST $
  createMutableImage width height background
        >>= execStateT drawing
        >>= unsafeFreezeImage

fillBezierShape :: (Pixel px, Modulable (PixelBaseComponent px))
                => Texture px -> [Bezier] -> DrawContext s () px
fillBezierShape texture beziers = do
    img@(MutableImage width height _) <- get
    let mini = V2 0 0
        maxi = V2 (fromIntegral width) (fromIntegral height)
        spans =
            rasterizeBezier $ beziers >>= clipBezier mini maxi

    lift $ mapM_ (composeCoverageSpan texture compositionAlpha img) spans
    {-lift $ mapM_ (composeCoverageSpan texture compositionDestination img) spans-}

(^&&^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^&&^) = liftA2 (&&)

(^||^) :: (Applicative a) => a Bool -> a Bool -> a Bool
(^||^) = liftA2 (||)

(^==^) :: (Eq v, Applicative a) => a v -> a v -> a Bool
(^==^) = liftA2 (==)

(^<=^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<=^) = liftA2 (<=)

(^<^) :: (Ord v, Applicative a) => a v -> a v -> a Bool
(^<^) = liftA2 (<)

(^<) :: (Applicative a, Ord v) => a v -> v -> a Bool
(^<) vec v = (< v) <$> vec

vmin :: (Ord n, Applicative a) => a n -> a n -> a n
vmin = liftA2 min

vmax :: (Ord n, Applicative a) => a n -> a n -> a n
vmax = liftA2 max

vabs :: (Num n, Functor a) => a n -> a n
vabs = fmap abs

vfloor :: (Additive a) => a Float -> a Int
vfloor = fmap floor

vceil :: (Additive a) => a Float -> a Int
vceil = fmap ceiling

clampPoint :: Point -> Point -> Point -> Point
clampPoint mini maxi v = vmin maxi $ vmax mini v

midPoint :: (Additive a) => a Float -> a Float -> a Float
midPoint a b = (a ^+^ b) ^/ 2.0

vpartition :: (Applicative a) => a Bool -> a v -> a v -> a v
vpartition = liftA3 choose
  where choose True a _ = a
        choose False _ b = b

clipBezier :: Point -> Point -> Bezier -> [Bezier]
clipBezier mini maxi bezier@(Bezier a b c)
    | insideX && insideY = [bezier]
    | outsideX || outsideY =
        [Bezier clampedA (clampedA `midPoint` clampedC) clampedC]
    | otherwise =
        recurse (Bezier m (b `midPoint`c) c) `mappend`
            recurse (Bezier a (a `midPoint` b) m)
  where bmin = vmin a $ vmin b c
        bmax = vmax a $ vmax b c

        recurse = clipBezier mini maxi

        clamper = clampPoint mini maxi
        clampedA = clamper a
        clampedC = clamper c

        V2 insideX insideY = mini ^<=^ bmin ^&&^ bmax ^<=^ maxi
        V2 outsideX outsideY = bmax ^<=^ mini ^||^ maxi ^<=^ bmin

        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)
        edgeSeparator = vabs (abbc ^-^ mini) ^<^ vabs (abbc ^-^ maxi)
        edge = vpartition edgeSeparator mini maxi
        m = vpartition (vabs (abbc ^-^ edge) ^< 0.1) edge abbc

data EdgeSample = EdgeSample
  { _sampleX     :: !Float
  , _sampleY     :: !Float
  , _sampleAlpha :: !Float
  , _sampleH     :: !Float
  }
  deriving Show

decomposeBeziers :: Bezier -> [EdgeSample]
decomposeBeziers (Bezier a@(V2 ax ay) b c@(V2 cx cy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise =
        decomposeBeziers (Bezier m (b `midPoint` c) c) ++
            decomposeBeziers (Bezier a (a `midPoint` b) m)
  where floorA = vfloor a
        floorC = vfloor c
        V2 px py  = fromIntegral <$> vmin floorA floorC
        V1 w = (px + 1 -) <$>  (V1 cx `midPoint` V1 ax)
        h = cy - ay

        V2 insideX insideY =
            floorA ^==^ floorC ^||^ vceil a ^==^ vceil c

        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)
        mini = fromIntegral <$> vfloor abbc
        maxi = fromIntegral <$> vceil abbc
        nearmin = vabs (abbc ^-^ mini) ^< 0.1
        nearmax = vabs (abbc ^-^ maxi) ^< 0.1

        minMaxing mi nearmi ma nearma p
          | nearmi = mi
          | nearma = ma
          | otherwise = p

        m = minMaxing <$> mini <*> nearmin <*> maxi <*> nearmax <*> abbc

data CoverageSpan = CoverageSpan
    { _coverageX      :: !Float
    , _coverageY      :: !Float
    , _coverageVal    :: !Float
    , _coverageLength :: !Float
    }
    deriving Show

combineEdgeSamples :: [EdgeSample] -> [CoverageSpan]
combineEdgeSamples = append . mapAccumL go (0, 0, 0, 0)
  where append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan x y (min 1 $ abs a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x', y', a + a', h + h'), [])
          | y == y' = ((x', y', h + a', h + h'), [p1, p2])
          | otherwise =
             ((x', y', a', h'), [CoverageSpan x y (min 1 $ abs a) 1])
               where p1 = CoverageSpan x y (min 1 $ abs a) 1
                     p2 = CoverageSpan (x + 1) y (min 1 $ abs h) (x' - x - 1)

{-vecOfRgba8 :: PixelRGBA8 -> V4 Word8-}
{-vecOfRgba8 (PixelRGBA8 r g b a) = V4 r g b a-}

{-rgba8OfVec :: V4 Word8 -> PixelRGBA8-}
{-rgba8OfVec (V4 r g b a) = PixelRGBA8 r g b a-}

rasterizeBezier :: [Bezier]-> [CoverageSpan]
rasterizeBezier = combineEdgeSamples . sortBy xy . concatMap decomposeBeziers
  where xy a b = compare (_sampleY a, _sampleX a) (_sampleY b, _sampleX b)

toWord8 :: Float -> Int
toWord8 r = floor $ r * 255 + 0.5

-- let's use inference to debug =)
composeCoverageSpan :: forall s px .
                      ( Pixel px, Modulable (PixelBaseComponent px) )
                    => Texture px
                    -> Compositor px
                    -> MutableImage s px
                    -> CoverageSpan
                    -> ST s ()
{-# INLINE composeCoverageSpan #-}
composeCoverageSpan texture compositor img coverage 
  | cov == 0 || initialX < 0 || y < 0 || imgWidth < initialX || imgHeight < y = return ()
  | otherwise = go 0 initialX initIndex
  where compCount = componentCount (undefined :: px)
        maxi = _coverageLength coverage
        imgData = mutableImageData img
        y = floor $ _coverageY coverage
        initialX = floor $ _coverageX coverage
        imgWidth = mutableImageWidth img
        imgHeight = mutableImageHeight img
        initIndex = (initialX + y * imgWidth) * compCount
        (cov, icov) = clampCoverage $ _coverageVal coverage

        go count _   _ | count >= maxi = return ()
        go count x idx = do
          oldPixel <- unsafeReadPixel imgData idx
          unsafeWritePixel imgData idx
            . compositor cov icov oldPixel
            $ texture x y
          go (count + 1) (x + 1) $ idx + compCount

