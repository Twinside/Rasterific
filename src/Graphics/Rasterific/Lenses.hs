{-# LANGUAGE RankNTypes #-}
-- | This module provides lenses compatible with the `lens`
-- module, without depending on it.
module Graphics.Rasterific.Lenses
    ( -- * Line lenses
      lineX0
    , lineX1
    , linePoints

      -- * Quadratic bezier curve
    , bezX0
    , bezX1
    , bezX2
    , bezierPoints

      -- * Cubic bezier lenses
    , cbezX0
    , cbezX1
    , cbezX2
    , cbezX3
    , cubicBezierPoints

      -- * Primitive lenses
    , primitivePoints

      -- * Path oriented lenses
    , pathCommandPoints
    , pathPoints

      -- * Type definition to match Lens
    , Lens
    , Lens'
    , Traversal
    , Traversal'
    ) where

import Graphics.Rasterific.Types

-- | Does it look familiar? Yes, it's the official
-- Lens type.
type Lens s t a b =
    forall f. Functor f => (a -> f b) -> s -> f t

-- | Try to match the Lens' type alias.
type Lens' s a = Lens s s a a

-- | Traversal type, matched to the one of the lens
-- package.
type Traversal s t a b =
    forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

-- | Create a full lens out of setter and getter
lens :: (s -> a)
     -> (s -> b -> t)
     -> Lens s t a b
{-# INLINE lens #-}
lens accessor setter = \f src ->
  fmap (setter src) $ f (accessor src)

-- | Traverse all the points of a line.
linePoints :: Traversal' Line Point
linePoints f (Line p0 p1) = Line <$> f p0 <*> f p1

-- | Line origin point.
lineX0 :: Lens' Line Point
lineX0 = lens _lineX0 setter where
  setter a b = a { _lineX0 = b }

-- | Line end point.
lineX1 :: Lens' Line Point
lineX1 = lens _lineX1 setter where
  setter a b = a { _lineX1 = b }

-- | Quadratic bezier starting point.
bezX0 :: Lens' Bezier Point
bezX0 = lens _bezierX0 setter where
  setter a b = a { _bezierX0 = b }

-- | bezier control point.
bezX1 :: Lens' Bezier Point
bezX1 = lens _bezierX1 setter where
  setter a b = a { _bezierX1 = b }

-- | bezier end point.
bezX2 :: Lens' Bezier Point
bezX2 = lens _bezierX2 setter where
  setter a b = a { _bezierX2 = b }

-- | Traversal of all the bezier's points.
bezierPoints :: Traversal' Bezier Point
bezierPoints f (Bezier p0 p1 p2) =
  Bezier <$> f p0 <*> f p1 <*> f p2

-- | Cubic bezier first point
cbezX0 :: Lens' CubicBezier Point
cbezX0 = lens _cBezierX0 setter where
  setter a b = a { _cBezierX0 = b }

-- | Cubic bezier first control point.
cbezX1 :: Lens' CubicBezier Point
cbezX1 = lens _cBezierX1 setter where
  setter a b = a { _cBezierX1 = b }

-- | Cubic bezier second control point.
cbezX2 :: Lens' CubicBezier Point
cbezX2 = lens _cBezierX2 setter where
  setter a b = a { _cBezierX2 = b }

-- | Cubic bezier last point.
cbezX3 :: Lens' CubicBezier Point
cbezX3 = lens _cBezierX2 setter where
  setter a b = a { _cBezierX3 = b }

-- | Traversal of all the points of the cubic bezier.
cubicBezierPoints :: Traversal' CubicBezier Point
cubicBezierPoints f (CubicBezier p0 p1 p2 p3) =
  CubicBezier <$> f p0 <*> f p1 <*> f p2 <*> f p3

-- | Traverse all the points defined in the primitive.
primitivePoints :: Traversal' Primitive Point
primitivePoints f (LinePrim l) = LinePrim <$> linePoints f l
primitivePoints f (BezierPrim b) = BezierPrim <$> bezierPoints f b
primitivePoints f (CubicBezierPrim c) =
    CubicBezierPrim <$> cubicBezierPoints f c

-- | Traversal of all the points of a path
pathCommandPoints :: Traversal' PathCommand Point
pathCommandPoints f (PathLineTo p) = PathLineTo <$> f p
pathCommandPoints f (PathQuadraticBezierCurveTo p1 p2) =
    PathQuadraticBezierCurveTo <$> f p1 <*> f p2
pathCommandPoints f (PathCubicBezierCurveTo p1 p2 p3) =
    PathCubicBezierCurveTo <$> f p1 <*> f p2 <*> f p3

-- | Traversal of all the points in a path.
pathPoints :: Traversal' Path Point
pathPoints f (Path p0 yn comms) =
  Path <$> f p0 <*> pure yn <*> traverse (pathCommandPoints f) comms

