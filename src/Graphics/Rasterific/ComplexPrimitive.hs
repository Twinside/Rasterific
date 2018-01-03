-- | Provides definitions for some higher level objects (only slightly)
module Graphics.Rasterific.ComplexPrimitive( rectangle
                                           , roundedRectangle
                                           , circle
                                           , ellipse
                                           ) where

import Control.Applicative( empty, (<|>) )
import Control.Exception( throw, ArithException( .. ) )

import Graphics.Rasterific.Linear( V2( .. ), (^+^), (^*) )
import Graphics.Rasterific.Line
import Graphics.Rasterific.CubicBezier
import Graphics.Rasterific.Types

isCoordValid :: RealFloat a => a -> Maybe ArithException
isCoordValid v
  | isInfinite v = pure Overflow
  | isNaN v || isDenormalized v = pure Denormal
  | otherwise = empty

isPointValid :: RealFloat a => V2 a -> Maybe ArithException
isPointValid (V2 x y) = isCoordValid x <|> isCoordValid y

-- | Generate a list of primitives representing a circle.
--
-- > fill $ circle (V2 100 100) 75
--
-- <<docimages/fill_circle.png>>
--
circle :: Point -- ^ Circle center in pixels
       -> Float -- ^ Circle radius in pixels
       -> [Primitive]
circle p r
  | Just ex <- isCoordValid r <|> isPointValid p = throw ex
circle center radius =
    CubicBezierPrim . transform mv <$> cubicBezierCircle
  where
    mv p = (p ^* radius) ^+^ center

-- | Generate a list of primitives representing an ellipse.
--
-- > fill $ ellipse (V2 100 100) 75 30
--
-- <<docimages/fill_ellipse.png>>
--
ellipse :: Point -> Float -> Float -> [Primitive]
ellipse c rx ry
  | Just ex <- isCoordValid rx <|> isCoordValid ry <|> isPointValid c = throw ex
ellipse center rx ry =
    CubicBezierPrim . transform mv <$> cubicBezierCircle
  where
    mv (V2 x y) = V2 (x * rx) (y * ry) ^+^ center

-- | Generate a list of primitives representing a
-- rectangle
--
-- > fill $ rectangle (V2 30 30) 150 100
--
-- <<docimages/fill_rect.png>>
--
rectangle :: Point -- ^ Corner upper left
          -> Float -- ^ Width in pixel
          -> Float -- ^ Height in pixel
          -> [Primitive]
rectangle p w h
  | Just ex <- isCoordValid w <|> isCoordValid h <|> isPointValid p = throw ex
rectangle p@(V2 px py) w h =
  LinePrim <$> lineFromPath
    [ p, V2 (px + w) py, V2 (px + w) (py + h), V2 px (py + h), p ]

-- | Generate a list of primitives representing a rectangle
-- with rounded corners.
--
-- > fill $ roundedRectangle (V2 10 10) 150 150 20 10
--
-- <<docimages/fill_roundedRectangle.png>>
--
roundedRectangle :: Point -- ^ Corner upper left
                 -> Float -- ^ Width in pixel
                 -> Float -- ^ Height in pixel.
                 -> Float -- ^ Radius along the x axis of the rounded corner. In pixel.
                 -> Float -- ^ Radius along the y axis of the rounded corner. In pixel.
                 -> [Primitive]
roundedRectangle p w h rx ry
  | Just ex <- isCoordValid w
        <|> isCoordValid h
        <|> isCoordValid rx
        <|> isCoordValid ry
        <|> isPointValid p = throw ex
roundedRectangle (V2 px py) w h rx ry =
    [ CubicBezierPrim . transform (^+^ V2 xFar yNear) $ cornerTopR
    , LinePrim $ Line (V2 xFar py) (V2 xNear py)
    , CubicBezierPrim . transform (^+^ V2 (px + rx) (py + ry)) $ cornerTopL
    , LinePrim $ Line (V2 px yNear) (V2 px yFar)
    , CubicBezierPrim . transform (^+^ V2 (px + rx) yFar) $ cornerBottomL
    , LinePrim $ Line (V2 xNear (py + h)) (V2 xFar (py + h))
    , CubicBezierPrim . transform (^+^ V2 xFar yFar) $ cornerBottomR
    , LinePrim $ Line (V2 (px + w) yFar) (V2 (px + w) yNear)
    ]
  where
   xNear = px + rx
   xFar = px + w - rx

   yNear = py + ry
   yFar = py + h - ry

   (cornerBottomR :
    cornerTopR     :
    cornerTopL  :
    cornerBottomL:_) = transform (\(V2 x y) -> V2 (x * rx) (y * ry)) <$> cubicBezierCircle

