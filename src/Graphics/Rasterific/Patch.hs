{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation using
-- "An efficient algorithm for subdivising linear Coons surfaces"
-- C.Yao and J.Rokne
-- Computer aided design 8 (1991) 291-303
module Graphics.Rasterific.Patch
    ( CoonPatch( .. )
    , TensorPatch( .. )
    , ParametricValues( .. )
    , CoonColorWeight
    , Subdivided( .. )
    , subdividePatch
    , subdivideTensorPatch
    , drawPatchOutline
    , renderCoonPatch
    , renderTensorPatch
    , debugDrawCoonPatch
    , parametricBase
    )  where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( Applicative( pure, (<*>) ), (<$>) )
import Data.Foldable( Foldable( foldMap ) )
#endif

import Control.Monad( forM_ )
import Control.Monad.Primitive( PrimMonad )
import Data.Monoid( (<>), Sum( .. ) )
import Data.Word( Word8 )
import Graphics.Rasterific.Types
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Texture

import Graphics.Rasterific

import Codec.Picture.Types
    ( PixelRGB8( .. )
    , PixelRGBA8( .. )
    )

-- | Meh
class InterpolablePixel a where
  maxDistance :: a -> a -> CoonColorWeight
  maxRepresentable :: Proxy a -> CoonColorWeight
  lerpValue :: CoonColorWeight -> a -> a -> a

instance InterpolablePixel Float where
  maxDistance a b = abs (a - b)
  maxRepresentable Proxy = 255
  lerpValue zeroToOne a b = (1 - zeroToOne) * a + zeroToOne * b

instance InterpolablePixel Word8 where
  maxDistance a b = abs (fromIntegral b - fromIntegral a) / 255.0
  maxRepresentable Proxy = 255
  lerpValue zeroToOne a b = 
    floor $ (1 - zeroToOne) * fromIntegral a + fromIntegral b * zeroToOne

instance InterpolablePixel PixelRGB8 where
  maxDistance (PixelRGB8 r g b) (PixelRGB8 r' g' b') =
    max (maxDistance b b') . max (maxDistance g g') $ maxDistance r r'
  maxRepresentable Proxy = 255
  lerpValue zeroToOne (PixelRGB8 r g b) (PixelRGB8 r' g' b') =
      PixelRGB8 (l r r') (l g g') (l b b')
     where l = lerpValue zeroToOne

instance InterpolablePixel PixelRGBA8 where
  maxRepresentable Proxy = 255
  maxDistance (PixelRGBA8 r g b a) (PixelRGBA8 r' g' b' a') =
    max (maxDistance a a') 
        . max (maxDistance b b')
        . max (maxDistance g g')
        $ maxDistance r r'

  lerpValue zeroToOne (PixelRGBA8 r g b a) (PixelRGBA8 r' g' b' a') =
      PixelRGBA8 (l r r') (l g g') (l b b') (l a a')
     where l = lerpValue zeroToOne

type CoonColorWeight = Float

-- | Values associated to the corner of a patch
-- @
--  North               East
--      +--------------+
--      |0            1|
--      |              |
--      |              |
--      |              |
--      |3            2|
--      +--------------+
--  West                South
-- @
data ParametricValues a = ParametricValues
    { _northValue :: !a
    , _eastValue  :: !a
    , _southValue :: !a
    , _westValue  :: !a
    }
    deriving (Functor, Show)

instance Applicative ParametricValues where
    pure a = ParametricValues a a a a
    ParametricValues n e s w <*> ParametricValues n' e' s' w' =
        ParametricValues (n n') (e e') (s s') (w w')

instance Foldable ParametricValues where
  foldMap f (ParametricValues n e s w) = f n <> f e <> f s <> f w

transposeParametricValues :: ParametricValues a -> ParametricValues a
transposeParametricValues (ParametricValues n e s w) = ParametricValues n w s e

-- @
--  North    ----->     East
--      +--------------+
--      |      0       |
--    ^ |              | |
--    | |3            1| |
--    | |              | v
--      |      2       |
--      +--------------+
--  West    <-----      South
-- @
maxColorDeepness :: forall px. InterpolablePixel px => ParametricValues px -> Int
maxColorDeepness values = ceiling $ log (maxDelta * range) / log 2 where
  range = maxRepresentable (Proxy :: Proxy px)
  maxDelta = 
    maximum [ maxDistance north east
            , maxDistance east south
            , maxDistance south west
            , maxDistance west north]
  ParametricValues { _westValue = west, _northValue = north
                   , _southValue = south, _eastValue = east } = values

meanValue :: ParametricValues (V2 CoonColorWeight) -> V2 CoonColorWeight
meanValue = (^* 0.25) . getSum . foldMap Sum

  --  N    midNorthEast   E
  --      +-------+------+
  --      |0      :     1|
  --      |       :      |
  --      | Left  :Right |
  --      |       :      |
  --      |3      :     2|
  --      +-------+------+
  --  W    midSouthWest   S
subdivideHorizontal :: ParametricValues (V2 CoonColorWeight)
                    -> (ParametricValues (V2 CoonColorWeight), ParametricValues (V2 CoonColorWeight))
subdivideHorizontal ParametricValues { .. } = (l, r) where
  midNorthEast = _northValue `midPoint` _eastValue
  midSouthWest = _westValue `midPoint` _southValue

  l = ParametricValues
    { _northValue = _northValue
    , _eastValue = midNorthEast
    , _southValue = midSouthWest
    , _westValue = _westValue
    }

  r = ParametricValues
    { _northValue = midNorthEast
    , _eastValue = _eastValue
    , _southValue = _southValue
    , _westValue = midSouthWest
    }

subdivideWeights :: ParametricValues (V2 CoonColorWeight)
                 -> Subdivided (ParametricValues (V2 CoonColorWeight))
subdivideWeights values = Subdivided { .. } where
  ParametricValues
    { _westValue = west
    , _northValue = north
    , _southValue = south
    , _eastValue = east
    } = values

  --  N       midNorth    E
  --      +-------+------+
  --      |0      :     1|
  --   mid|   grid:Mid   |
  --  West+=======:======+ midEast
  --      |       :      |
  --      |3      :     2|
  --      +-------+------+
  --  W       midSouth    S
  midNorthValue = north `midPoint` east
  midWestValue = north `midPoint` west
  midSoutValue = west `midPoint` south
  midEastValue = east `midPoint` south

  gridMidValue = midSoutValue `midPoint` midNorthValue

  _northWest = ParametricValues
    { _northValue = north
    , _eastValue = midNorthValue
    , _southValue = gridMidValue
    , _westValue = midWestValue
    }

  _northEast = ParametricValues
    { _northValue = midNorthValue
    , _eastValue = east
    , _southValue = midEastValue
    , _westValue = gridMidValue
    }

  _southWest = ParametricValues
    { _northValue = midWestValue
    , _eastValue  = gridMidValue
    , _southValue = midSoutValue
    , _westValue = west
    }
  
  _southEast = ParametricValues
    { _northValue = gridMidValue
    , _eastValue = midEastValue
    , _southValue = south
    , _westValue = midSoutValue
    }

data TensorPatch px = TensorPatch
  { _curve0 :: !CubicBezier
  , _curve1 :: !CubicBezier
  , _curve2 :: !CubicBezier
  , _curve3 :: !CubicBezier
  , _tensorValues :: !(ParametricValues px)
  }

instance Transformable (TensorPatch px) where
  transform f (TensorPatch c0 c1 c2 c3 v) =
    TensorPatch
        (transform f c0)
        (transform f c1)
        (transform f c2)
        (transform f c3)
        v

instance {-# OVERLAPPING #-} PointFoldable (TensorPatch px) where
  foldPoints f acc (TensorPatch c0 c1 c2 c3 _) = g c3 . g c2 . g c1 $ g c0 acc
    where g v a = foldPoints f a v

westCurveOfPatch :: TensorPatch px -> CubicBezier
westCurveOfPatch TensorPatch
  { _curve0 = CubicBezier c0 _ _ _
  , _curve1 = CubicBezier c1 _ _ _
  , _curve2 = CubicBezier c2 _ _ _
  , _curve3 = CubicBezier c3 _ _ _
  } = CubicBezier c0 c1 c2 c3

eastCurveOfPatch :: TensorPatch px -> CubicBezier
eastCurveOfPatch TensorPatch
  { _curve0 = CubicBezier _ _ _ c0
  , _curve1 = CubicBezier _ _ _ c1
  , _curve2 = CubicBezier _ _ _ c2
  , _curve3 = CubicBezier _ _ _ c3
  } = CubicBezier c0 c1 c2 c3

transposePatch :: TensorPatch px -> TensorPatch px
transposePatch TensorPatch
  { _curve0 = CubicBezier c00 c01 c02 c03
  , _curve1 = CubicBezier c10 c11 c12 c13
  , _curve2 = CubicBezier c20 c21 c22 c23
  , _curve3 = CubicBezier c30 c31 c32 c33
  , _tensorValues = values
  } = TensorPatch
    { _curve0 = CubicBezier c00 c10 c20 c30
    , _curve1 = CubicBezier c01 c11 c21 c31
    , _curve2 = CubicBezier c02 c12 c22 c32
    , _curve3 = CubicBezier c03 c13 c23 c33
    , _tensorValues = transposeParametricValues values
    }


-- | Perform an operation like:
--
-- @
--    o--------o--------o--------o
--    |        |        |        |
--    |        |        |        |
--    |        |        |        |
--    o--------o--------o--------o
--    |        |        |        |
--    |        |        |        |
--    |        |        |        |
--    o--------o--------o--------o
--    |        |        |        |
--    |        |        |        |
--    |        |        |        |
--    o--------o--------o--------o
--    |        |        |        |
--    |        |        |        |
--    |        |        |        |
--    o--------o--------o--------o
--
--       to (more or less)
--
--    o----*---o----*----o----*---o
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    o----*---o----*----o----*---o
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    o----*---o----*----o----*---o
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    o----*---o----*----o----*---o
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    |    |   |    |    |    |   |
--    o----*---o----*----o----*---o
--    <------------><------------->
--       Left            Right
-- @
--
horizontalTensorSubdivide :: TensorPatch (V2 CoonColorWeight)
                          -> (TensorPatch (V2 CoonColorWeight), TensorPatch (V2 CoonColorWeight))
horizontalTensorSubdivide p = (TensorPatch l0 l1 l2 l3 vl, TensorPatch r0 r1 r2 r3 vr) where
  (l0, r0) = divideCubicBezier $ _curve0 p
  (l1, r1) = divideCubicBezier $ _curve1 p
  (l2, r2) = divideCubicBezier $ _curve2 p
  (l3, r3) = divideCubicBezier $ _curve3 p
  (vl, vr) = subdivideHorizontal $ _tensorValues p

subdivideTensorPatch :: TensorPatch (V2 CoonColorWeight)
                     -> Subdivided (TensorPatch (V2 CoonColorWeight))
subdivideTensorPatch p = subdivided where
  (west, east) = horizontalTensorSubdivide p
  (northWest, southWest) = horizontalTensorSubdivide $ transposePatch west
  (northEast, southEast) = horizontalTensorSubdivide $ transposePatch east
  subdivided = Subdivided
    { _northWest = northWest
    , _northEast = northEast
    , _southWest = southWest
    , _southEast = southEast
    }

--
-- @
--                        ----->
--                  North     _____----------------+
--   ^          +------------/                     /
--   |         /                                  /       |
--   |        /                                  /        |
--   |       /                                  /  east   |
--   | west |                                  /          |
--          |                                 |           v
--           \                                 \   .
--            \                  __-------------+
--             +----------------/
--                    South
--                       <-----
-- @
--
data CoonPatch px = CoonPatch
    { _north :: !CubicBezier
    , _east :: !CubicBezier
    , _south :: !CubicBezier
    , _west :: !CubicBezier
    , _coonValues :: {-# UNPACK #-} !(ParametricValues px)
    }

instance Transformable (CoonPatch px) where
  transform f (CoonPatch n e s w v) =
    CoonPatch
        (transform f n)
        (transform f e)
        (transform f s)
        (transform f w)
        v

instance {-# OVERLAPPING #-} PointFoldable (CoonPatch px) where
  foldPoints f acc (CoonPatch n e s w _) = g n . g e . g s $ g w acc
    where g v a = foldPoints f a v

data Subdivided a = Subdivided
    { _northWest :: !a
    , _northEast :: !a
    , _southWest :: !a
    , _southEast :: !a
    }

-- | Split a coon patch in two vertically
--
-- @
--                        --------->
--                  North     +____----------------+
--   ^          +------------/:                    /
--   |         /              :                   /       |
--   |        /               :                  /        |
--   |       /               :                  /  east   |
--   | west |               :                  /          |
--          |               :                 |           v
--           \               :                 \    .   
--            \               :  __-------------+
--             +--------------+-/
--                    South
--                       <---------
-- @
--
subdividePatch :: CoonPatch (V2 CoonColorWeight)
               -> Subdivided (CoonPatch (V2 CoonColorWeight))
subdividePatch patch = Subdivided
    { _northWest = northWest
    , _northEast = northEast
    , _southWest = southWest
    , _southEast = southEast
    } where
  north@(CubicBezier nw _ _ ne) = _north patch
  south@(CubicBezier se _ _ sw) = _south patch

  midNorthLinear = nw `midPoint` ne
  midSouthLinear = sw `midPoint` se
  midWestLinear = nw `midPoint` sw
  midEastLinear = ne `midPoint` se

  (northLeft@(CubicBezier _ _ _ midNorth), northRight) = divideCubicBezier north
  (southRight, southLeft@(CubicBezier midSouth _ _ _ )) = divideCubicBezier south
  (westBottom, westTop@(CubicBezier midWest _ _ _)) = divideCubicBezier $ _west patch
  (eastTop@(CubicBezier _ _ _ midEast), eastBottom) = divideCubicBezier $ _east patch

  midNorthSouth = north `midCurve` south
  midEastWest = _east patch `midCurve` _west patch 

  (splitNorthSouthTop, splitNorthSouthBottom) =
      divideCubicBezier $ combine
        midEastWest
        (midNorth `straightLine` midSouth)
        (midNorthLinear `straightLine` midSouthLinear)

  (splitWestEastLeft, splitWestEastRight) =
      divideCubicBezier $ combine
        midNorthSouth
        (midWest `straightLine` midEast)
        (midWestLinear `straightLine` midEastLinear)

  weights = subdivideWeights $ _coonValues patch

  northWest = CoonPatch
    { _west = westTop
    , _north = northLeft
    , _east = splitNorthSouthTop
    , _south = inverseBezier splitWestEastLeft
    , _coonValues = _northWest weights
    }

  northEast = CoonPatch
    { _west = inverseBezier splitNorthSouthTop
    , _north = northRight
    , _east = eastTop
    , _south = inverseBezier splitWestEastRight
    , _coonValues = _northEast weights
    }

  southWest = CoonPatch
    { _west = westBottom
    , _north = splitWestEastLeft
    , _east = splitNorthSouthBottom
    , _south = southLeft
    , _coonValues = _southWest weights
    }

  southEast = CoonPatch
    { _west = inverseBezier splitNorthSouthBottom
    , _north = splitWestEastRight
    , _east = eastBottom
    , _south = southRight
    , _coonValues = _southEast weights
    }


-- | We must reinverse some bezier curve to match the global
-- direction
inverseBezier :: CubicBezier -> CubicBezier
inverseBezier (CubicBezier a b c d) = CubicBezier d c b a

combine :: CubicBezier -> CubicBezier -> CubicBezier -> CubicBezier
combine (CubicBezier a1 b1 c1 d1)
        (CubicBezier a2 b2 c2 d2)
        (CubicBezier a3 b3 c3 d3) =
  CubicBezier (a1 ^+^ a2 ^-^ a3)
              (b1 ^+^ b2 ^-^ b3)
              (c1 ^+^ c2 ^-^ c3)
              (d1 ^+^ d2 ^-^ d3)


straightLine :: Point -> Point -> CubicBezier
straightLine a b = CubicBezier a p1 p2 b where
  p1 = lerp (1/3) a b
  p2 = lerp (2/3) a b


-- | The curves in the coon patch are inversed!
midCurve :: CubicBezier -> CubicBezier -> CubicBezier
midCurve (CubicBezier a b c d) (CubicBezier d' c' b' a') =
  CubicBezier
    (a `midPoint` a')
    (b `midPoint` b')
    (c `midPoint` c')
    (d `midPoint` d')

weightToColor :: InterpolablePixel px
              => ParametricValues px -> V2 CoonColorWeight -> px
weightToColor ParametricValues { .. } (V2 u v) = lerpValue v uTop uBottom where
  uTop = lerpValue u _northValue _eastValue
  uBottom = lerpValue u _westValue _southValue

drawPatchOutline :: CoonPatch px -> Drawing pxb ()
drawPatchOutline CoonPatch { .. } =
  stroke 2 JoinRound (CapRound, CapRound) [_north, _east, _south, _west]

pointsOf :: PointFoldable v => v -> [Point]
pointsOf = foldPoints (flip (:)) []

debugDrawCoonPatch :: CoonPatch px -> Drawing PixelRGBA8 ()
debugDrawCoonPatch patch@(CoonPatch { .. }) = do
  let stroker = stroke 2 JoinRound (CapRound, CapRound)
  withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) $
    drawPatchOutline patch
  withTexture (uniformTexture (PixelRGBA8 20 20 40 255)) $
    forM_ (pointsOf patch) $ \p -> stroker $ circle p 4
  let controlDraw = stroker . toPrimitives . lineFromPath . pointsOf
  withTexture (uniformTexture (PixelRGBA8 50 50 128 255)) $ do
    mapM_ controlDraw [_north, _east, _west, _south]

parametricBase :: ParametricValues (V2 CoonColorWeight)
parametricBase = ParametricValues
  { _northValue = V2 0 0
  , _eastValue  = V2 1 0
  , _southValue = V2 1 1
  , _westValue  = V2 0 1
  }

renderCoonPatch :: forall m px.
                   (PrimMonad m, RenderablePixel px, InterpolablePixel px)
                => CoonPatch px -> DrawContext m px ()
renderCoonPatch originalPatch = go maxDeepness basePatch where
  maxDeepness = maxColorDeepness baseColors
  baseColors = _coonValues originalPatch

  basePatch = originalPatch { _coonValues = parametricBase }

  drawPatchUniform CoonPatch { .. } = fillWithTextureNoAA FillWinding texture geometry where
    geometry = toPrim <$> [_north, _east, _south, _west]
    texture = uniformTexture . weightToColor baseColors $ meanValue _coonValues

  go 0 patch = drawPatchUniform patch
  go depth (subdividePatch -> Subdivided { .. }) =
    let d = depth - (1 :: Int) in
    go d _northWest >> go d _northEast >> go d _southWest >> go d _southEast

renderTensorPatch :: forall m px.
                   (PrimMonad m, RenderablePixel px, InterpolablePixel px)
                  => TensorPatch px -> DrawContext m px ()
renderTensorPatch originalPatch = go maxDeepness basePatch where
  maxDeepness = maxColorDeepness baseColors
  baseColors = _tensorValues originalPatch

  basePatch = originalPatch { _tensorValues = parametricBase }

  drawPatchUniform p = fillWithTextureNoAA FillWinding texture geometry where
    geometry = toPrim <$> [_curve0 p, westCurveOfPatch p, _curve3 p, eastCurveOfPatch p]
    texture = uniformTexture . weightToColor baseColors . meanValue $ _tensorValues p

  go 0 patch = drawPatchUniform patch
  go depth (subdivideTensorPatch -> Subdivided { .. }) =
    let d = depth - (1 :: Int) in
    go d _northWest >> go d _northEast >> go d _southWest >> go d _southEast

