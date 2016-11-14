-- | Translation of cairo-arc.c
module Graphics.Rasterific.Arc( arcInDirection ) where

import Data.Maybe( fromMaybe )
import Data.Monoid( (<>) )
import qualified Data.Vector.Unboxed as VU

import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Types


errorTable :: VU.Vector (Float, Float)
errorTable = VU.imap calcAngle errors where
  calcAngle i a = (pi / fromIntegral i, a)
  errors = VU.fromListN 10
    [ 0.0185185185185185036127
    , 0.000272567143730179811158
    , 2.38647043651461047433e-05
    , 4.2455377443222443279e-06
    , 1.11281001494389081528e-06
    , 3.72662000942734705475e-07
    , 1.47783685574284411325e-07
    , 6.63240432022601149057e-08
    , 3.2715520137536980553e-08
    , 1.73863223499021216974e-08
    , 9.81410988043554039085e-09 ]

{- Spline deviation from the circle in radius would be given by:

    error = sqrt (x**2 + y**2) - 1

   A simpler error function to work with is:

    e = x**2 + y**2 - 1

   From "Good approximation of circles by curvature-continuous Bezier
   curves", Tor Dokken and Morten Daehlen, Computer Aided Geometric
   Design 8 (1990) 22-41, we learn:

    abs (max(e)) = 4/27 * sin**6(angle/4) / cos**2(angle/4)

   and
    abs (error) =~ 1/2 * e

   Of course, this error value applies only for the particular spline
   approximation that is used in _cairo_gstate_arc_segment.  -}
fixAngleError :: Int -> Float -> Float
fixAngleError i tolerance
    | errorNormalized > tolerance = fixAngleError (i + 1) tolerance
    | otherwise = angle
  where
    angle = pi / fromIntegral i
    errorNormalized = 2.0/27.0 * (sin (angle / 4) ** 6) / (cos (angle / 4) ** 2)

arcMaxAngleForToleranceNormalized :: Float -> Float
arcMaxAngleForToleranceNormalized tolerance = fixAngleError (angleIndex + 1) tolerance
  where
    angleIndex = fromMaybe
      (VU.length errorTable) $
       VU.findIndex ((< tolerance) . snd) errorTable

arcSegmentsNeeded :: Float -> Float -> Transformation -> Float
                  -> Int
arcSegmentsNeeded angle radius trans tolerance = ceiling (angle / maxAngle) where
  -- the error is amplified by at most the length of the
  -- major axis of the circle; see cairo-pen.c for a more detailed analysis
  -- of this.
  majorAxis = matrixTransformedCircleMajorAxis trans radius
  maxAngle = arcMaxAngleForToleranceNormalized (tolerance / majorAxis)

-- determine the length of the major axis of a circle of the given radius
-- after applying the transformation matrix.
matrixTransformedCircleMajorAxis :: Transformation -> Float -> Float
matrixTransformedCircleMajorAxis (Transformation a c _
                                                 b d _) radius =
     radius * sqrt (f + norm v)
  where
    i = a*a + b*b;
    j = c*c + d*d;

    f = 0.5 * (i + j)
    v = V2 (0.5 * (i - j)) (a * c + b * d)
    -- we don't need the minor axis length, which is
    -- double min = radius * sqrt (f - sqrt (g*g+h*h));

data Direction = Forward | Backward

clampAngle :: Float -> Float -> Float
clampAngle angleMin = go where
  go angleMax
    | angleMax - angleMin > 4 * pi = go $ angleMax - 2 * pi
    | otherwise = angleMax

subdivideAngles :: (Monoid m)
                => Direction -> (Float -> Float -> m) -> Float -> Float -> m
subdivideAngles dir f aMin = go aMin . clampAngle aMin where
  go angleMin angleMax | deltaAngle > pi = case dir of
      Forward -> go angleMin angleMid <> go angleMid angleMax
      Backward -> go angleMid angleMax <> go angleMin angleMid
    where
      deltaAngle = angleMax - angleMin
      angleMid = angleMin + deltaAngle / 2
  go angleMin angleMax = f angleMin angleMax


{-  We want to draw a single spline approximating a circular arc radius
   R from angle A to angle B. Since we want a symmetric spline that
   matches the endpoints of the arc in position and slope, we know
   that the spline control points must be:

	(R * cos(A), R * sin(A))
	(R * cos(A) - h * sin(A), R * sin(A) + h * cos (A))
	(R * cos(B) + h * sin(B), R * sin(B) - h * cos (B))
	(R * cos(B), R * sin(B))

   for some value of h.

   "Approximation of circular arcs by cubic poynomials", Michael
   Goldapp, Computer Aided Geometric Design 8 (1991) 227-238, provides
   various values of h along with error analysis for each.

   From that paper, a very practical value of h is:

	h = 4/3 * tan(angle/4)

   This value does not give the spline with minimal error, but it does
   provide a very good approximation, (6th-order convergence), and the
   error expression is quite simple, (see the comment for
   _arc_error_normalized).
-}
arcSegment :: Point -> Float -> Float -> Float -> PathCommand
arcSegment (V2 xc yc) radius angleA angleB = PathCubicBezierCurveTo p1 p2 p3 where
  rSinA = radius * sin angleA
  rCosA = radius * cos angleA
  rSinB = radius * sin angleB
  rCosB = radius * cos angleB

  h = 4.0/3.0 * tan ((angleB - angleA) / 4.0)

  p1 = V2 (xc + rCosA - h * rSinA) (yc + rSinA + h * rCosA)
  p2 = V2 (xc + rCosB + h * rSinB) (yc + rSinB - h * rCosB)
  p3 = V2 (xc + rCosB) (yc + rSinB)

arcInDirection :: Point -- ^ center
               -> Float -- ^ Radius
               -> Direction
               -> Float -- ^ Tolerance
               -> Float -- ^ Angle minimum
               -> Float -- ^ Angle maximum
               -> [PathCommand]
arcInDirection p radius dir tolerance = subdivideAngles dir go where
  go angleMin angleMax = commands where
    deltaAngle = angleMax - angleMin
    segmentCount = arcSegmentsNeeded deltaAngle radius mempty tolerance

    (angle, angleStep) = case dir of
      Forward -> (angleMin, deltaAngle / fromIntegral segmentCount)
      Backward -> (angleMax, - deltaAngle / fromIntegral segmentCount)

    commands =
        [arcSegment p radius a (a + angleStep)
            | i <- [0 .. segmentCount - 1]
            , let a = angle + angleStep * fromIntegral i]

