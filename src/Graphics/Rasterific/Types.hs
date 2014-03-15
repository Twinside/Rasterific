{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Gather all the types used in the rasterization engine.
module Graphics.Rasterific.Types
    ( -- * Geometry description
      Vector
    , Point
    , Line( .. )
    , Bezier( .. )
    , CubicBezier( .. )
    , Primitive( .. )
    , Container
    , PathCommand( .. )
    , Path( .. )
    , Transformable( .. )

      -- * Rasterization control types
    , Cap( .. )
    , Join( .. )
    , SamplerRepeat( .. )
    , DashPattern
    , StrokeWidth

      -- * Internal type
    , EdgeSample( .. )
    , pathToPrimitives
    ) where

import Data.List( foldl' )
import Linear( V2( .. ) )

-- | Represent a vector
type Vector = V2 Float

-- | Represent a point
type Point = V2 Float

-- | Type alias just to get more meaningful
-- type signatures
type StrokeWidth = Float

-- | Dash pattern to use
type DashPattern = [Float]

-- | Describe how we will "finish" the stroking
-- that don't loop.
data Cap
    -- | Create a straight caping on the stroke.
    -- Cap value should be positive and represent
    -- the distance from the end of curve to the actual cap
    --
    --  * cap straight with param 0 : <<docimages/cap_straight.png>>
    --
    --  * cap straight with param 1 : <<docimages/cap_straight_1.png>>
    --
  = CapStraight Float 

    -- | Create a rounded caping on the stroke.
    -- <<docimages/cap_round.png>>
  | CapRound          
  deriving (Eq, Show)

-- | Describe how to display the join of broken lines
-- while stroking.
data Join
    -- | Make a curved join.
    -- <<docimages/join_round.png>>
  = JoinRound       
    -- | Make a mitter join. Value must be positive or null.
    -- Seems to make sense in [0;1] only
    --
    --  * Miter join with 0 : <<docimages/join_miter.png>>
    --
    --  * Miter join with 5 : <<docimages/join_miter_5.png>>
    --
  | JoinMiter Float 
  deriving (Eq, Show)

-- | Describe the behaviour of samplers and texturers
-- when they are out of the bounds of image and/or gradient.
data SamplerRepeat
    -- | Will clamp (ie. repeat the last pixel) when
    -- out of bound
    -- <<docimages/sampler_pad.png>>
  = SamplerPad
    -- | Will loop on it's definition domain
    -- <<docimages/sampler_repeat.png>>
  | SamplerRepeat
    -- | Will loop inverting axises
    -- <<docimages/sampler_reflect.png>>
  | SamplerReflect
  deriving (Eq, Show)

-- | Represent a raster line
data EdgeSample = EdgeSample
  { _sampleX     :: {-# UNPACK #-} !Float -- ^ Horizontal position
  , _sampleY     :: {-# UNPACK #-} !Float -- ^ Vertical position
  , _sampleAlpha :: {-# UNPACK #-} !Float -- ^ Alpha
  , _sampleH     :: {-# UNPACK #-} !Float -- ^ Height
  }
  deriving Show

-- | This typeclass is there to help transform the geometry,
-- by applying a transformation on every point of a geometric
-- element.
class Transformable a where
    -- | Apply a transformation function for every
    --  point in the element.
    transform :: (Point -> Point) -> a -> a

    -- | Fold an accumulator on all the points of
    -- the primitive.
    foldPoints :: (b -> Point -> b) -> b -> a -> b

-- | Describe a simple 2D line between two points.
--
-- > fill $ LinePrim <$> [ Line (V2 10 10) (V2 190 10)
-- >                     , Line (V2 190 10) (V2 95 170)
-- >                     , Line (V2 95 170) (V2 10 10)]
--
-- <<docimages/simple_line.png>>
--
data Line = Line
  { _lineX0 :: {-# UNPACK #-} !Point -- ^ Origin point
  , _lineX1 :: {-# UNPACK #-} !Point -- ^ End point
  }
  deriving (Eq, Show)

instance Transformable Line where
    {-# INLINE transform #-}
    transform f (Line a b) = Line (f a) $ f b

    {-# INLINE foldPoints #-}
    foldPoints f acc (Line a b) = f (f acc b) a

-- | Describe a quadratic bezier spline, described
-- using 3 points.
--
-- > fill $ BezierPrim <$> [Bezier (V2 10 10) (V2 200 50) (V2 200 100)
-- >                       ,Bezier (V2 200 100) (V2 150 200) (V2 120 175)
-- >                       ,Bezier (V2 120 175) (V2 30 100) (V2 10 10)]
--
-- <<docimages/quadratic_bezier.png>>
--
data Bezier = Bezier
  { -- | Origin points, the spline will pass through it.
    _bezierX0 :: {-# UNPACK #-} !Point 
    -- | Control point, the spline won't pass on it.
  , _bezierX1 :: {-# UNPACK #-} !Point 
    -- | End point, the spline will pass through it.
  , _bezierX2 :: {-# UNPACK #-} !Point 
  }
  deriving (Eq, Show)

instance Transformable Bezier where
    {-# INLINE transform #-}
    transform f (Bezier a b c) = Bezier (f a) (f b) $ f c

    {-# INLINE foldPoints #-}
    foldPoints f acc (Bezier a b c) = 
        foldl' f acc [a, b, c]

-- | Describe a cubic bezier spline, described
-- using 4 points.
--
-- > stroke 4 JoinRound (CapRound, CapRound) $
-- >    [CubicBezierPrim $ CubicBezier (V2 0 10) (V2 205 250)
-- >                                   (V2 (-10) 250) (V2 160 35)]
--
-- <<docimages/cubic_bezier.png>>
--
data CubicBezier = CubicBezier 
  { -- | Origin point, the spline will pass through it.
    _cBezierX0 :: {-# UNPACK #-} !Point 
    -- | First control point of the cubic bezier curve.
  , _cBezierX1 :: {-# UNPACK #-} !Point 
    -- | Second control point of the cubic bezier curve.
  , _cBezierX2 :: {-# UNPACK #-} !Point
    -- | End point of the cubic bezier curve
  , _cBezierX3 :: {-# UNPACK #-} !Point
  }
  deriving (Eq, Show)

instance Transformable CubicBezier where
    {-# INLINE transform #-}
    transform f (CubicBezier a b c d) =
        CubicBezier (f a) (f b) (f c) $ f d

    {-# INLINE foldPoints #-}
    foldPoints f acc (CubicBezier a b c d) = 
        foldl' f acc [a, b, c, d]

-- | This datatype gather all the renderable primitives,
-- they are kept separated otherwise to allow specialization
-- on some specific algorithms. You can mix the different
-- primitives in a single call :
-- 
-- > fill
-- >    [ CubicBezierPrim $ CubicBezier (V2 50 20) (V2 90 60)
-- >                                    (V2  5 100) (V2 50 140)
-- >    , LinePrim $ Line (V2 50 140) (V2 120 80)
-- >    , LinePrim $ Line (V2 120 80) (V2 50 20) ]
--
-- <<docimages/primitive_mixed.png>>
--
data Primitive
  = LinePrim !Line      -- ^ Primitive used for lines
  | BezierPrim !Bezier  -- ^ Primitive used for quadratic beziers curves
  | CubicBezierPrim !CubicBezier -- ^ Primitive used for cubic bezier curve
  deriving (Eq, Show)

instance Transformable Primitive where
    {-# INLINE transform #-}
    transform f (LinePrim l) = LinePrim $ transform f l
    transform f (BezierPrim b) = BezierPrim $ transform f b
    transform f (CubicBezierPrim c) = CubicBezierPrim $ transform f c

    {-# INLINE foldPoints #-}
    foldPoints f acc = go
      where go (LinePrim l) = foldPoints f acc l
            go (BezierPrim b) = foldPoints f acc b
            go (CubicBezierPrim c) = foldPoints f acc c

type Container a = [a]

-- | Describe a path in a way similar to many graphical
-- packages, using a "pen" position in memory and reusing
-- it for the next "move"
-- For example the example from Primitive could be rewritten:
--
-- > fill . pathToPrimitives $ Path (V2 50 20) True
-- >    [ PathCubicBezierCurveTo (V2 90 60) (V2  5 100) (V2 50 140)
-- >    , PathLineTo (V2 120 80) ]
--
-- <<docimages/path_example.png>>
--
data Path = Path 
    { -- | Origin of the point, equivalent to the
      -- first "move" command.
      _pathOriginPoint :: Point
      -- | Tell if we must close the path.
    , _pathClose       :: Bool
      -- | List of commands in the path
    , _pathCommand     :: [PathCommand]
    }
    deriving (Eq, Show)

-- | Actions to create a path
data PathCommand
    = -- | Draw a line from the current point to another point
      PathLineTo Point
      -- | Draw a quadratic bezier curve from the current point
      -- through the control point to the end point.
    | PathQuadraticBezierCurveTo Point Point

      -- | Draw a cubic bezier curve using 2 control points.
    | PathCubicBezierCurveTo Point Point Point
    deriving (Eq, Show)

-- | Transform a path description into a list of renderable
-- primitives.
pathToPrimitives :: Path -> [Primitive]
pathToPrimitives (Path origin needClosing commands) = go origin commands
  where
    go prev [] | prev /= origin && needClosing = [LinePrim $ Line prev origin]
    go _ [] = []
    go prev (PathLineTo to : xs) =
        LinePrim (Line prev to) : go to xs
    go prev (PathQuadraticBezierCurveTo c1 to : xs) =
        BezierPrim (Bezier prev c1 to) : go to xs
    go prev (PathCubicBezierCurveTo c1 c2 to : xs) =
        CubicBezierPrim (CubicBezier prev c1 c2 to) : go to xs

