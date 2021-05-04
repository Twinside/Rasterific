{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Module implementing types used for geometry
-- bound calculations.
module Graphics.Rasterific.PlaneBoundable ( PlaneBound( .. )
                                          , PlaneBoundable( .. )
                                          , boundWidth
                                          , boundHeight
                                          , boundLowerLeftCorner
                                          ) where

import Graphics.Rasterific.Linear( V2( .. ) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.CubicBezier

-- | Represent the minimal axis aligned rectangle
-- in which some primitives can be drawn. Should
-- fit to bezier curve and not use directly their
-- control points.
data PlaneBound = PlaneBound
    { -- | Corner upper left of the bounding box of
      -- the considered primitives.
      _planeMinBound :: !Point
      -- | Corner lower right of the bounding box of
      -- the considered primitives.
    , _planeMaxBound :: !Point
    }
    deriving (Eq, Show)

-- | Extract the width of the bounds
boundWidth :: PlaneBound -> Float
boundWidth (PlaneBound (V2 x0 _) (V2 x1 _)) = x1 - x0

-- | Extract the height of the bound
boundHeight :: PlaneBound -> Float
boundHeight (PlaneBound (V2 _ y0) (V2 _ y1)) = y1 - y0

-- | Extract the position of the lower left corner of the
-- bounds.
boundLowerLeftCorner :: PlaneBound -> Point
boundLowerLeftCorner (PlaneBound (V2 x _) (V2 _ y)) = V2 x y

instance Semigroup PlaneBound where
  (<>) (PlaneBound mini1 maxi1) (PlaneBound mini2 maxi2) =
    PlaneBound (min <$> mini1 <*> mini2)
               (max <$> maxi1 <*> maxi2)

instance Monoid PlaneBound where
  mappend = (<>)
  mempty = PlaneBound infPoint negInfPoint
    where
      infPoint = V2 (1 / 0) (1 / 0)
      negInfPoint = V2 (negate 1 / 0) (negate 1 / 0)

-- | Class used to calculate bounds of various geometrical
-- primitives. The calculated is precise, the bounding should
-- be minimal with respect with drawn curve.
class PlaneBoundable a where
    -- | Given a graphical elements, calculate it's bounds.
    planeBounds :: a -> PlaneBound

instance PlaneBoundable Point where
    planeBounds a = PlaneBound a a

instance PlaneBoundable Line where
    planeBounds (Line p1 p2) = planeBounds p1 <> planeBounds p2

instance PlaneBoundable Bezier where
    planeBounds (Bezier p0 p1 p2) =
        planeBounds (CubicBezier p0 p1 p1 p2)

instance PlaneBoundable CubicBezier where
    planeBounds = foldMap planeBounds . cubicBezierBounds

instance PlaneBoundable Primitive where
    planeBounds (LinePrim l) = planeBounds l
    planeBounds (BezierPrim b) = planeBounds b
    planeBounds (CubicBezierPrim c) = planeBounds c

