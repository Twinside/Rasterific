{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Rasterific where

import Control.Applicative( Applicative, liftA3, (<$>) )
import Control.Monad.ST( ST )
import Codec.Picture( PixelRGBA8( .. ), Pixel( .. ) )
import Codec.Picture.Types( MutableImage( .. ) )
import Data.List( mapAccumL )
import Data.Monoid( mappend )
import Data.Word( Word8 )

import Linear( V2( .. )
             , V1( .. )
             , V4( .. )
             , Additive( .. )
             , (^+^)
             , (^-^)
             , (^/)
             , liftI2
             )

type Point = V2 Float

type Texture px = Int -> Int -> px
type Compositor px = px -> px -> px

data Bezier = Bezier !Point !Point !Point

infix 4  ^<, ^<=^, ^<^, ^==^
infixr 3 ^&&^
infixr 2 ^||^

(^&&^) :: (Additive a) => a Bool -> a Bool -> a Bool
(^&&^) = liftI2 (&&)

(^||^) :: (Additive a) => a Bool -> a Bool -> a Bool
(^||^) = liftI2 (||)

(^==^) :: (Eq v, Additive a) => a v -> a v -> a Bool
(^==^) = liftI2 (==)

(^<=^) :: (Ord v, Additive a) => a v -> a v -> a Bool
(^<=^) = liftI2 (<=)

(^<^) :: (Ord v, Additive a) => a v -> a v -> a Bool
(^<^) = liftI2 (<)

(^<) :: (Additive a, Ord v) => a v -> v -> a Bool
(^<) vec v = (< v) <$> vec

vmin :: (Ord n, Additive a) => a n -> a n -> a n
vmin = liftI2 min

vmax :: (Ord n, Additive a) => a n -> a n -> a n
vmax = liftI2 max

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

decomposeBeziers :: Bezier -> [EdgeSample]
decomposeBeziers (Bezier a@(V2 _ ay) b c@(V2 cx cy))
    | insideX && insideY = [EdgeSample (px + 0.5) (py + 0.5) (w * h) h]
    | otherwise =
        decomposeBeziers (Bezier m (b `midPoint` c) c) ++
            decomposeBeziers (Bezier a (a `midPoint` b) m)
  where floorA = vfloor a
        floorC = vfloor c
        V2 px py  = fromIntegral <$> vmin floorA floorC
        V1 w = (px + 1 -) <$>  (V1 cx `midPoint` V1 cy)
        h = cy - ay

        V2 insideX insideY =
            floorA ^==^ floorC ^||^ vceil a ^==^ vceil c

        abbc = (a `midPoint` b) `midPoint` (b `midPoint` c)
        mini = fromIntegral <$> vfloor abbc
        maxi = fromIntegral <$> vceil abbc
        nearmin = vabs (abbc ^-^ mini) ^< 0.1
        nearmax = vabs (abbc ^-^ maxi) ^< 0.1
        m = vpartition nearmin mini $ vpartition nearmax maxi abbc


data CoverageSpan = CoverageSpan
    { _coverageX      :: !Int
    , _coverageY      :: !Int
    , _coverageVal    :: !Float
    , _coverageLength :: !Float
    }

combineEdgeSamples :: [EdgeSample] -> [CoverageSpan]
combineEdgeSamples = append . mapAccumL go (0, 0, 0, 0)
  where append ((x, y, a, _), lst) =
            concat lst ++ [CoverageSpan (floor x) (floor y) (min 1 $ abs a) 1]

        go (x, y, a, h) (EdgeSample x' y' a' h')
          | y == y' && x == x' = ((x, y, a + a', h + h'), [])
          | y == y' = ((x, y, a + a', h + h'), [p1, p2])
          | otherwise =
             ((x, y, a', h'), [CoverageSpan ix iy (min 1 $ abs a) 1])
               where p1 = CoverageSpan ix iy (min 1 $ abs a) 1
                     p2 = CoverageSpan (ix + 1) iy (min 1 $ abs h) (x' - x - 1)
                     ix = floor x
                     iy = floor y

vecOfRgba8 :: PixelRGBA8 -> V4 Word8
vecOfRgba8 (PixelRGBA8 r g b a) = V4 r g b a

rgba8OfVec :: V4 Word8 -> PixelRGBA8
rgba8OfVec (V4 r g b a) = PixelRGBA8 r g b a

composeCoverageSpan :: forall s px . (Pixel px)
                    => Texture px
                    -> Compositor px
                    -> MutableImage s px
                    -> CoverageSpan
                    -> ST s ()
{-# INLINE composeCoverageSpan #-}
composeCoverageSpan texture img coverage = go 0 initialX initIndex
  where compCount = componentCount (undefined :: px)
        maxi = _coverageLength coverage
        imgData = mutableImageData img
        y = _coverageY coverage
        initialX = _coverageX coverage
        imgWidth = mutableImageWidth img
        initIndex = (initialX + y * imgWidth) * compCount

        go count _   _ | count >= maxi = return ()
        go count x idx = do
          oldPixel <- unsafeReadPixel imgData idx
          unsafeWritePixel imgData idx $ texture x y
          go (count + 1) (x + 1) $ idx + compCount

