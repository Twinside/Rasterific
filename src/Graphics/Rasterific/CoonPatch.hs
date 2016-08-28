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
module Graphics.Rasterific.CoonPatch
    ( CoonPatch( .. )
    , CoonValues( .. )
    , CoonColorWeight
    , Subdivided( .. )
    , subdividePatch
    , drawPatchOutline
    , renderCoonPatch 
    , parametricBase
    )  where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( Applicative( pure, (<*>) ), (<$>) )
import Data.Foldable( Foldable( foldMap ) )
#endif

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

import Debug.Trace
import Text.Printf

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
data CoonValues a = CoonValues
    { _northValue :: !a
    , _eastValue  :: !a
    , _southValue :: !a
    , _westValue  :: !a
    }
    deriving (Functor, Show)

instance Applicative CoonValues where
    pure a = CoonValues a a a a
    CoonValues n e s w <*> CoonValues n' e' s' w' =
        CoonValues (n n') (e e') (s s') (w w')

instance Foldable CoonValues where
  foldMap f (CoonValues n e s w) = f n <> f e <> f s <> f w

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
    , _coonValues :: {-# UNPACK #-} !(CoonValues px)
    }

data Subdivided a = Subdivided
    { _northWest :: !a
    , _northEast :: !a
    , _southWest :: !a
    , _southEast :: !a
    }

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
maxColorDeepness :: forall px. InterpolablePixel px => CoonValues px -> Int
maxColorDeepness values = ceiling $ log (maxDelta * range) / log 2 where
  range = maxRepresentable (Proxy :: Proxy px)
  maxDelta = 
    maximum [ maxDistance north east
            , maxDistance east south
            , maxDistance south west
            , maxDistance west north]
  CoonValues { _westValue = west, _northValue = north
             , _southValue = south, _eastValue = east } = values

toTolerance :: CoonValues (V2 CoonColorWeight) -> CoonValues CoonColorWeight
-- Order of substraction is important in order to avoid a call to abs
toTolerance values = CoonValues (ex - nx) (sy - ey) (sx - wx) (wy - ny) where
  CoonValues
    { _northValue = V2 nx ny
    , _eastValue  = V2 ex ey
    , _southValue = V2 sx sy
    , _westValue  = V2 wx wy
    } = values

meanValue :: CoonValues (V2 CoonColorWeight) -> V2 CoonColorWeight
meanValue = (^* 0.25) . getSum . foldMap Sum

isBelowWeightBounds :: CoonValues CoonColorWeight -> CoonValues CoonColorWeight -> Bool
isBelowWeightBounds bounds values =
    {-(\a -> trace (printf "%s %s %s" (show a) (show values) (show bounds)) a) $ -}
  and $ (<=) <$> values <*> bounds

subdivideWeights :: CoonValues (V2 CoonColorWeight)
                 -> Subdivided (CoonValues (V2 CoonColorWeight))
subdivideWeights values = Subdivided { .. } where
  CoonValues
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

  _northWest = CoonValues
    { _northValue = north
    , _eastValue = midNorthValue
    , _southValue = gridMidValue
    , _westValue = midWestValue
    }

  _northEast = CoonValues
    { _northValue = midNorthValue
    , _eastValue = east
    , _southValue = midEastValue
    , _westValue = gridMidValue
    }

  _southWest = CoonValues
    { _northValue = midWestValue
    , _eastValue  = gridMidValue
    , _southValue = midSoutValue
    , _westValue = west
    }
  
  _southEast = CoonValues
    { _northValue = gridMidValue
    , _eastValue = midEastValue
    , _southValue = south
    , _westValue = midSoutValue
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
              => CoonValues px -> V2 CoonColorWeight -> px
weightToColor CoonValues { .. } (V2 u v) = lerpValue v uTop uBottom where
  uTop = lerpValue u _northValue _eastValue
  uBottom = lerpValue u _westValue _southValue

drawPatchOutline :: CoonPatch px -> Drawing pxb ()
drawPatchOutline CoonPatch { .. } =
  stroke 2 JoinRound (CapRound, CapRound) [_north, _east, _south, _west]


parametricBase :: CoonValues (V2 CoonColorWeight)
parametricBase = CoonValues
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

  toUniformOrder CoonPatch { .. } = DrawOrder
    { _orderPrimitives = [toPrim <$> [_north, _east, _south, _west]]
    , _orderTexture =
        uniformTexture . weightToColor baseColors $ meanValue _coonValues
    , _orderFillMethod = FillWinding
    , _orderMask = Nothing
    }

  go 0 patch = fillOrder $ toUniformOrder patch
  go depth (subdividePatch -> Subdivided { .. }) =
    let d = depth - (1 :: Int) in
    go d _northWest >> go d _northEast >> go d _southWest >> go d _southEast

