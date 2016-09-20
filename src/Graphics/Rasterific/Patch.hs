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
    , DebugOption( .. )
    , InterpolablePixel
    , subdividePatch
    , subdivideTensorPatch
    , horizontalTensorSubdivide
    , transposePatch
    , drawCoonPatchOutline
    , renderCoonPatch
    , renderTensorPatch
    , debugDrawCoonPatch
    , debugDrawTensorPatch
    , parametricBase
    , defaultDebug
    , transformCoon
    , renderCoonMesh
    )  where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( Applicative( pure, (<*>) ), (<$>) )
import Data.Foldable( Foldable( foldMap ) )
#endif

import Control.Monad.Free( liftF )
import Control.Monad( when, forM_ )
import Control.Monad.Primitive( PrimMonad )
import Data.Monoid( Sum( .. ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Operators
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Compositor
import Graphics.Rasterific.ComplexPrimitive
import Graphics.Rasterific.Line( lineFromPath )
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.PatchTypes
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific.Command

import Codec.Picture.Types( PixelRGBA8( .. ) )

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

basePointOfCoonPatch :: CoonPatch px -> [(Point, px)]
basePointOfCoonPatch CoonPatch
    { _north = CubicBezier a _ _ b
    , _south = CubicBezier c _ _ d
    , _coonValues = ParametricValues { .. }
    } = [(a, _northValue), (b, _eastValue), (c, _southValue), (d, _westValue)]

controlPointOfCoonPatch :: CoonPatch px -> [Point]
controlPointOfCoonPatch CoonPatch
    { _north = CubicBezier _ a b _
    , _east  = CubicBezier _ c d _
    , _south = CubicBezier _ e f _
    , _west  = CubicBezier _ g h _
    } = [a, b, c, d, e, f, g, h]

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

  -- These points are to calculate S_C and S_D
  (northLeft@(CubicBezier _ _ _ midNorth), northRight) = divideCubicBezier north
  (southRight, southLeft@(CubicBezier midSouth _ _ _ )) = divideCubicBezier south
  (westBottom, westTop@(CubicBezier midWest _ _ _)) = divideCubicBezier $ _west patch
  (eastTop@(CubicBezier _ _ _ midEast), eastBottom) = divideCubicBezier $ _east patch

  -- This points are to calculate S_B
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

-- | Calculate the new cubic bezier using S
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
weightToColor ParametricValues { .. } (V2 u v) = fromFloatPixel $ lerp v uTop uBottom where
  uTop = lerp u (toFloatPixel _northValue) (toFloatPixel _eastValue)
  uBottom = lerp u (toFloatPixel _westValue) (toFloatPixel _southValue)

drawCoonPatchOutline :: CoonPatch px -> Drawing pxb ()
drawCoonPatchOutline CoonPatch { .. } =
  liftF $ Stroke 2 JoinRound (CapRound, CapRound) prims ()
  where
    prims = toPrimitives [_north, _east, _south, _west]

pointsOf :: PointFoldable v => v -> [Point]
pointsOf = foldPoints (flip (:)) []

data DebugOption = DebugOption
  { _drawControlMesh    :: !Bool
  , _drawBaseVertices   :: !Bool
  , _drawControVertices :: !Bool
  , _colorVertices      :: !Bool
  , _drawOutline        :: !Bool
  , _outlineColor       :: !PixelRGBA8
  , _controlMeshColor   :: !PixelRGBA8
  , _vertexColor        :: !PixelRGBA8
  , _controlColor       :: !PixelRGBA8
  }

defaultDebug :: DebugOption
defaultDebug = DebugOption
  { _drawControlMesh    = True
  , _drawBaseVertices   = True
  , _drawControVertices = True
  , _drawOutline        = True
  , _colorVertices      = False
  , _outlineColor       = PixelRGBA8 0 0 0 255
  , _controlMeshColor   = PixelRGBA8 50 50 128 255
  , _vertexColor        = PixelRGBA8 20 20 40 255
  , _controlColor       = PixelRGBA8 20 20 40 255
  }

debugDrawCoonPatch :: DebugOption -> CoonPatch PixelRGBA8 -> Drawing PixelRGBA8 ()
debugDrawCoonPatch DebugOption { .. } patch@(CoonPatch { .. }) = do
  let stroker v = liftF $ Stroke 2 JoinRound (CapRound, CapRound) v ()
      fill sub = liftF $ Fill FillWinding sub ()
      setColor c inner = liftF $ SetTexture (SolidTexture c) inner ()
  when _drawOutline $
    setColor _outlineColor (drawCoonPatchOutline patch)

  when _drawBaseVertices $
    forM_ (basePointOfCoonPatch patch) $ \(p, c) ->
       if not _colorVertices then
         setColor _vertexColor (stroker $ circle p 4)
       else do
         setColor c . fill $ circle p 4
         setColor _vertexColor . stroker $ circle p 5

  when _drawControVertices $
    forM_ (controlPointOfCoonPatch patch) $ \p ->
       setColor _controlColor . stroker $ circle p 4

  let controlDraw = stroker . toPrimitives . lineFromPath . pointsOf
  when _drawControlMesh $
    setColor _controlMeshColor $ do
        mapM_ controlDraw [_north, _east, _west, _south]

debugDrawTensorPatch :: DebugOption -> TensorPatch px -> Drawing PixelRGBA8 ()
debugDrawTensorPatch DebugOption { .. } p = do
  let stroker v = liftF $ Stroke 2 JoinRound (CapRound, CapRound) v ()
      setColor c inner =
          liftF $ SetTexture (SolidTexture c) inner ()
      p' = transposePatch p

  when _drawOutline $
    setColor _outlineColor $
        mapM_ (stroker . toPrimitives)
            [ _curve0 p, _curve1 p, _curve2 p, _curve3 p
            , _curve0 p', _curve1 p', _curve2 p', _curve3 p']

  when _drawBaseVertices   $
    setColor _vertexColor $
        forM_ (pointsOf p) $ \pp -> stroker $ circle pp 4

  let controlDraw = stroker . toPrimitives . lineFromPath . pointsOf
  when _drawControlMesh $
    setColor _controlMeshColor $ do
        mapM_ controlDraw
            [ _curve0 p, _curve1 p, _curve2 p, _curve3 p
            , _curve0 p', _curve1 p', _curve2 p', _curve3 p']

parametricBase :: ParametricValues (V2 CoonColorWeight)
parametricBase = ParametricValues
  { _northValue = V2 0 0
  , _eastValue  = V2 1 0
  , _southValue = V2 1 1
  , _westValue  = V2 0 1
  }

renderCoonMesh :: forall m px.  (PrimMonad m, RenderablePixel px)
               => MeshPatch px -> DrawContext m px ()
renderCoonMesh = mapM_ renderCoonPatch . coonPatchesOf

renderCoonPatch :: forall m px.  (PrimMonad m, RenderablePixel px)
                => CoonPatch px -> DrawContext m px ()
renderCoonPatch originalPatch = go maxDeepness basePatch where
  maxDeepness = maxColorDeepness baseColors
  baseColors = _coonValues originalPatch

  basePatch = originalPatch { _coonValues = parametricBase }

  drawPatchUniform CoonPatch { .. } = fillWithTextureNoAA FillWinding texture geometry where
    geometry = toPrim <$> [_north, _east, _south, _west]
    texture = SolidTexture . weightToColor baseColors $ meanValue _coonValues

  go 0 patch = drawPatchUniform patch
  go depth (subdividePatch -> Subdivided { .. }) =
    let d = depth - (1 :: Int) in
    go d _northWest >> go d _northEast >> go d _southWest >> go d _southEast

renderTensorPatch :: forall m px.  (PrimMonad m, RenderablePixel px)
                  => TensorPatch px -> DrawContext m px ()
renderTensorPatch originalPatch = go maxDeepness basePatch where
  maxDeepness = maxColorDeepness baseColors
  baseColors = _tensorValues originalPatch

  basePatch = originalPatch { _tensorValues = parametricBase }

  drawPatchUniform p = fillWithTextureNoAA FillWinding texture geometry where
    geometry = toPrim <$> [_curve0 p, westCurveOfPatch p, _curve3 p, eastCurveOfPatch p]
    texture = SolidTexture . weightToColor baseColors . meanValue $ _tensorValues p

  go 0 patch = drawPatchUniform patch
  go depth (subdivideTensorPatch -> Subdivided { .. }) =
    let d = depth - (1 :: Int) in
    go d _northWest >> go d _northEast >> go d _southWest >> go d _southEast

