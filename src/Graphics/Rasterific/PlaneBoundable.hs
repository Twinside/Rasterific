{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Module implementing types used for geometry
-- bound calculations.
module Graphics.Rasterific.PlaneBoundable ( PlaneBound( .. )
                                          , PlaneBoundable( .. ) ) where

import Control.Applicative( (<$>), (<*>) )
import Data.Monoid( Monoid( .. ), (<>) )
import Data.Foldable( foldMap )

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

instance Monoid PlaneBound where
  mempty = PlaneBound infPoint negInfPoint
    where
      infPoint = V2 (1 / 0) (1 / 0)
      negInfPoint = V2 (negate 1 / 0) (negate 1 / 0)

  mappend (PlaneBound mini1 maxi1) (PlaneBound mini2 maxi2) =
    PlaneBound (min <$> mini1 <*> mini2)
               (max <$> maxi1 <*> maxi2)

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

