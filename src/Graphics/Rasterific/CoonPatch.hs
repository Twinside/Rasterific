-- | Implementation using
-- "An efficient algorithm for subdivising linear Coons surfaces"
-- C.Yao and J.Rokne
-- Computer aided design 8 (1991) 291-303
module Graphics.Rasterific.CoonPatch where

import Graphics.Rasterific.Types
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Linear

--
-- @
--                North     _____----------------+
--            +------------/                     /
--           /                                  /
--          /                                  / 
--         /                                  /  east
--   west |                                  / 
--        |                                 |   
--         \                                 \   
--          \                  __-------------+
--           +----------------/
--                  South
-- @
--
data CoonPatch px = CoonPatch
    { _west :: !CubicBezier
    , _north :: !CubicBezier
    , _east :: !CubicBezier
    , _south :: !CubicBezier

    , _westValue :: !px
    , _northValue :: !px
    , _eastValue :: !px
    , _southValue :: !px
    }

data SubdividedCoonPatch px = SubdividedCoonPatch 
    { _northWest :: !(CoonPatch px)
    , _northEast :: !(CoonPatch px)
    , _southWest :: !(CoonPatch px)
    , _southEast :: !(CoonPatch px)
    }

class Halveable a where
   midValue :: a -> a -> a

-- | Split a coon patch in two vertically
--
-- @
--                North     +____----------------+
--            +------------/:                    /
--           /              :                   /
--          /               :                  / 
--         /               :                  /  east
--   west |               :                  / 
--        |               :                 |   
--         \               :                 \   
--          \               :  __-------------+
--           +--------------+-/
--                  South
-- @
--
subdividePatch :: Halveable px => CoonPatch px -> SubdividedCoonPatch px
subdividePatch patch = SubdividedCoonPatch 
    { _northWest = northWest
    , _northEast = northEast
    , _southWest = southWest
    , _southEast = southEast
    } where
  north@(CubicBezier nw _ _ ne) = _north patch
  south@(CubicBezier sw _ _ se) = _south patch

  midNorthLinear = nw `midPoint` ne
  midSouthLinear = sw `midPoint` se
  midWestLinear = nw `midPoint` sw
  midEastLinear = ne `midPoint` se

  (northLeft@(CubicBezier _ _ _ midNorth), northRight) = divideCubicBezier north
  (southLeft@(CubicBezier _ _ _ midSouth), southRight) = divideCubicBezier south
  (westTop@(CubicBezier _ _ _ midWest), westBottom) = divideCubicBezier $ _west patch
  (eastTop@(CubicBezier _ _ _ midEast), eastBottom) = divideCubicBezier $ _east patch

  midNorthSouth = north `midCurve` south
  midWestEast = _west patch `midCurve` _east patch

  (splitNorthSouthTop, splitNorthSouthBottom) =
      divideCubicBezier $ combine
        midWestEast
        (midNorth `straightLine` midSouth)
        (midNorthLinear `straightLine` midSouthLinear)

  (splitWestEastLeft, splitWestEastRight) =
      divideCubicBezier $ combine
        midNorthSouth
        (midWest `straightLine` midEast)
        (midWestLinear `straightLine` midEastLinear)

  midNorthValue = _northValue patch `midValue` _eastValue patch
  midWestValue = _northValue patch `midValue` _westValue patch
  midSoutValue = _westValue patch `midValue` _southValue patch
  midEastValue = _eastValue patch `midValue` _southValue patch
  
  gridMidValue = midSoutValue  `midValue` midNorthValue

  northWest = CoonPatch
    { _west = westTop
    , _north = northLeft
    , _east = splitNorthSouthTop
    , _south = splitWestEastLeft

    , _westValue = _westValue patch
    , _northValue = _northValue patch
    , _eastValue = midEastValue
    , _southValue = gridMidValue
    }

  northEast = CoonPatch
    { _west = splitNorthSouthTop
    , _north = northRight
    , _east = eastTop
    , _south = splitWestEastRight

    , _westValue = gridMidValue
    , _northValue = midNorthValue
    , _eastValue = _eastValue patch
    , _southValue = midEastValue
    }

  southWest = CoonPatch
    { _west = westBottom
    , _north = splitWestEastLeft
    , _east = splitNorthSouthBottom
    , _south = southLeft

    , _westValue = _westValue patch
    , _northValue = midWestValue
    , _eastValue  = gridMidValue
    , _southValue = midSoutValue
    }

  southEast = CoonPatch
    { _west = splitNorthSouthBottom
    , _north = splitWestEastRight
    , _east = eastBottom
    , _south = southRight

    , _westValue = midSoutValue
    , _northValue = gridMidValue
    , _eastValue = midEastValue
    , _southValue = _southValue patch
    }



combine :: CubicBezier -> CubicBezier -> CubicBezier -> CubicBezier
combine (CubicBezier a1 b1 c1 d1)
        (CubicBezier a2 b2 c2 d2)
        (CubicBezier a3 b3 c3 d3) =
  CubicBezier (a1 ^+^ a2 ^-^ a3)
              (b1 ^+^ b2 ^-^ b3)
              (c1 ^+^ c2 ^-^ c3)
              (d1 ^+^ d2 ^-^ d3)


straightLine :: Point -> Point -> CubicBezier
straightLine a b = CubicBezier a p p b
  where p = a `midPoint` b

midCurve :: CubicBezier -> CubicBezier -> CubicBezier
midCurve (CubicBezier a b c d) (CubicBezier a' b' c' d') =
  CubicBezier
    (a `midPoint` a')
    (b `midPoint` b')
    (c `midPoint` c')
    (d `midPoint` d')

