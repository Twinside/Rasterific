{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Gather all the types used in the rasterization engine.
module Graphics.Rasterific.Types
    ( Vector
    , Point
    , Cap( .. )
    , Join( .. )
    , EdgeSample( .. )
    , Primitive( .. )
    , Line( .. )
    , Bezier( .. )
    , CubicBezier( .. )
    , StrokeWidth
    ) where

import Linear( V2( .. ) )

-- | Represent a vector
type Vector = V2 Float

-- | Represent a point
type Point = V2 Float

-- | Type alias just to get more meaningful
-- type signatures
type StrokeWidth = Float

-- | Describe how we will "finish" the stroking
-- that don't loop.
data Cap =
    -- | Create a straight caping on the stroke.
    -- Cap value should be positive and represent
    -- the distance from the end of curve to the actual cap
    CapStraight Float 
  | CapRound          -- ^ Create a rounded caping on the stroke.
  deriving (Eq, Show)

-- | Describe how to display the join of broken lines
-- while stroking.
data Join =
    -- | Make a curved join.
    JoinRound       
    -- | Make a mitter join. Value must be positive or null.
    -- Seems to make sense in [0;1] only
  | JoinMiter Float 
  deriving (Eq, Show)

-- | Represent a raster line
data EdgeSample = EdgeSample
  { _sampleX     :: {-# UNPACK #-} !Float -- ^ Horizontal position
  , _sampleY     :: {-# UNPACK #-} !Float -- ^ Vertical position
  , _sampleAlpha :: {-# UNPACK #-} !Float -- ^ Alpha
  , _sampleH     :: {-# UNPACK #-} !Float -- ^ Height
  }
  deriving Show

data Line = Line
  { _lineX0 :: !Point
  , _lineX1 :: !Point
  }
  deriving (Eq, Show)

data Bezier = Bezier
  { _bezierX0 :: !Point
  , _bezierX1 :: !Point
  , _bezierX2 :: !Point
  }
  deriving (Eq, Show)

data CubicBezier = CubicBezier 
  { _cBezierX0 :: !Point
  , _cBezierX1 :: !Point
  , _cBezierX2 :: !Point
  , _cBezierX3 :: !Point
  }
  deriving (Eq, Show)

data Primitive
  = LinePrim !Line
  | BezierPrim !Bezier
  | CubicBezierPrim !CubicBezier
  deriving (Eq, Show)


