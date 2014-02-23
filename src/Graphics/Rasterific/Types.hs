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

      -- * Rasterization control types
    , Cap( .. )
    , Join( .. )
    , SamplerRepeat( .. )
    , DashPattern
    , StrokeWidth

      -- * Internal type
    , EdgeSample( .. )
    ) where

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
-- when they are "out of bound
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

-- | Describe a simple 2D line between two points.
data Line = Line
  { _lineX0 :: {-# UNPACK #-} !Point -- ^ Origin point
  , _lineX1 :: {-# UNPACK #-} !Point -- ^ End point
  }
  deriving (Eq, Show)

-- | Describe a quadratic bezier spline, described
-- using 3 points.
data Bezier = Bezier
  { -- | Origin points, the spline will pass through it.
    _bezierX0 :: {-# UNPACK #-} !Point 
    -- | Control point, the spline won't pass on it.
  , _bezierX1 :: {-# UNPACK #-} !Point 
    -- | End point, the spline will pass through it.
  , _bezierX2 :: {-# UNPACK #-} !Point 
  }
  deriving (Eq, Show)

-- | Describe a cubic bezier spline, described
-- using 4 points.
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

data Primitive
  = LinePrim !Line
  | BezierPrim !Bezier
  | CubicBezierPrim !CubicBezier
  deriving (Eq, Show)

type Container a = [a]

