{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
-- | Gather all the types used in the rasterization engine.
module Graphics.Rasterific.Types
    ( -- * Geometry description
      Vector
    , Point
    , Line( .. )
    , Bezier( .. )
    , CubicBezier( .. )
    , Primitive( .. )

    , Primitivable( .. )
    , Geometry( .. )

    , Producer
    , Container
    , containerOfList
    , listOfContainer
    , containerOfFunction
    , PathCommand( .. )
    , Path( .. )
    , Transformable( .. )
    , PointFoldable( .. )

      -- * Rasterization control types
    , Cap( .. )
    , Join( .. )
    , FillMethod( .. )
    , SamplerRepeat( .. )
    , DashPattern
    , StrokeWidth

      -- * Internal type
    , EdgeSample( .. )
    , pathToPrimitives

      -- * Little geometry helpers
    , firstTangeantOf
    , lastTangeantOf
    , firstPointOf
    , lastPointOf
    , resplit

      -- * RankNType helper
    , Proxy( Proxy )
    ) where

import Data.DList( DList, fromList )

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( Foldable )
import Data.Traversable( Traversable )
import Control.Applicative( (<$>), (<*>), pure )
#endif
import Control.Monad.Identity( runIdentity )
import Data.Foldable( foldl', toList )
import qualified Data.Foldable as F
import Graphics.Rasterific.Linear( V2( .. ), (^-^), nearZero )
import Graphics.Rasterific.Operators
import Foreign.Ptr( castPtr )
import Foreign.Storable( Storable( sizeOf
                       , alignment
                       , peek
                       , poke
                       , peekElemOff
                       , pokeElemOff ) )

-- | Represent a vector
type Vector = V2 Float

-- | Type alias just to get more meaningful
-- type signatures
type StrokeWidth = Float

-- | Dash pattern to use
type DashPattern = [Float]

data Proxy p = Proxy

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

-- | Tell how to fill complex shapes when there is self 
-- intersections. If the filling mode is not specified,
-- then it's the `FillWinding` method which is used.
--
-- The examples used are produced with the following
-- function:
--
--
-- > fillingSample :: FillMethod -> Drawing px ()
-- > fillingSample fillMethod = fillWithMethod fillMethod geometry where
-- >   geometry = transform (applyTransformation $ scale 0.35 0.4
-- >                                            <> translate (V2 (-80) (-180)))
-- >      [ Path (V2 484 499) True
-- >          [ PathCubicBezierCurveTo (V2 681 452) (V2 639 312) (V2 541 314)
-- >          , PathCubicBezierCurveTo (V2 327 337) (V2 224 562) (V2 484 499)
-- >          ]
-- >      , Path (V2 136 377) True
-- >          [ PathCubicBezierCurveTo (V2 244 253) (V2 424 420) (V2 357 489)
-- >          , PathCubicBezierCurveTo (V2 302 582) (V2 47 481) (V2 136 377)
-- >          ]
-- >      , Path (V2 340 265) True
-- >          [ PathCubicBezierCurveTo (V2 64 371) (V2 128 748) (V2 343 536)
-- >          , PathCubicBezierCurveTo (V2 668 216) (V2 17 273) (V2 367 575)
-- >          , PathCubicBezierCurveTo (V2 589 727) (V2 615 159) (V2 340 265)
-- >          ]
-- >      ]
data FillMethod
  -- | Also known as nonzero rule.
  -- To determine if a point falls inside the curve, you draw 
  -- an imaginary line through that point. Next you will count
  -- how many times that line crosses the curve before it reaches
  -- that point. For every clockwise rotation, you subtract 1 and
  -- for every counter-clockwise rotation you add 1.
  --
  -- <<docimages/fill_winding.png>>
  = FillWinding

  -- | This rule determines the insideness of a point on 
  -- the canvas by drawing a ray from that point to infinity
  -- in any direction and counting the number of path segments
  -- from the given shape that the ray crosses. If this number
  -- is odd, the point is inside; if even, the point is outside.
  --
  -- <<docimages/fill_evenodd.png>>
  | FillEvenOdd
  deriving (Eq, Enum, Show)

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
  deriving (Eq, Enum, Show)

-- | Represent a raster line
data EdgeSample = EdgeSample
  { _sampleX     :: {-# UNPACK #-} !Float -- ^ Horizontal position
  , _sampleY     :: {-# UNPACK #-} !Float -- ^ Vertical position
  , _sampleAlpha :: {-# UNPACK #-} !Float -- ^ Alpha
  , _sampleH     :: {-# UNPACK #-} !Float -- ^ Height
  }
  deriving Show

-- | Just to get faster sorting
instance Storable EdgeSample where
   sizeOf _ = 4 * sizeOf (0 :: Float)
   alignment = sizeOf

   {-# INLINE peek #-}
   peek ptr = do
     let q = castPtr ptr
     sx <- peekElemOff q 0
     sy <- peekElemOff q 1
     sa <- peekElemOff q 2
     sh <- peekElemOff q 3
     return $ EdgeSample sx sy sa sh
      
   {-# INLINE poke #-}
   poke ptr (EdgeSample sx sy sa sh) = do
     let q = castPtr ptr
     pokeElemOff q 0 sx
     pokeElemOff q 1 sy
     pokeElemOff q 2 sa
     pokeElemOff q 3 sh

-- | This typeclass is there to help transform the geometry,
-- by applying a transformation on every point of a geometric
-- element.
class Transformable a where
    -- | Apply a transformation function for every
    --  point in the element.
    transform :: (Point -> Point) -> a -> a
    transform f = runIdentity . transformM (return . f)

    -- | Transform but monadic
    transformM :: Monad m => (Point -> m Point) -> a -> m a

-- | Typeclass helper gathering all the points of a given
-- geometry.
class PointFoldable a where
    -- | Fold an accumulator on all the points of
    -- the primitive.
    foldPoints :: (b -> Point -> b) -> b -> a -> b

-- | Just apply the function
instance Transformable Point where
    {-# INLINE transform #-}
    transform f = f
    {-# INLINE transformM #-}
    transformM f = f

-- | Just apply the function
instance PointFoldable Point where
    {-# INLINE foldPoints #-}
    foldPoints f = f

-- | Describe a simple 2D line between two points.
--
-- > fill [ Line (V2 10 10) (V2 190 10)
-- >      , Line (V2 190 10) (V2 95 170)
-- >      , Line (V2 95 170) (V2 10 10)]
--
-- <<docimages/simple_line.png>>
--
data Line = Line
  { _lineX0 :: {-# UNPACK #-} !Point -- ^ Origin point
  , _lineX1 :: {-# UNPACK #-} !Point -- ^ End point
  }
  deriving Eq

instance Show Line where
  show (Line a b) =
      "Line (" ++ show a ++ ") ("
               ++ show b ++ ")"

instance Transformable Line where
    {-# INLINE transformM #-}
    transformM f (Line a b) = Line <$> f a <*> f b

instance PointFoldable Line where
    {-# INLINE foldPoints #-}
    foldPoints f acc (Line a b) = f (f acc b) a

-- | Describe a quadratic bezier spline, described
-- using 3 points.
--
-- > fill [Bezier (V2 10 10) (V2 200 50) (V2 200 100)
-- >      ,Bezier (V2 200 100) (V2 150 200) (V2 120 175)
-- >      ,Bezier (V2 120 175) (V2 30 100) (V2 10 10)]
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
  deriving Eq

instance Show Bezier where
    show (Bezier a b c) =
        "Bezier (" ++ show a ++ ") ("
                   ++ show b ++ ") ("
                   ++ show c ++ ")"

instance Transformable Bezier where
    {-# INLINE transform #-}
    transform f (Bezier a b c) = Bezier (f a) (f b) $ f c
    {-# INLINE transformM #-}
    transformM f (Bezier a b c) = Bezier <$> f a <*> f b <*> f c

instance PointFoldable Bezier where
    {-# INLINE foldPoints #-}
    foldPoints f acc (Bezier a b c) =
        foldl' f acc [a, b, c]

-- | Describe a cubic bezier spline, described
-- using 4 points.
--
-- > stroke 4 JoinRound (CapRound, CapRound) $
-- >    CubicBezier (V2 0 10) (V2 205 250) (V2 (-10) 250) (V2 160 35)
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
  deriving Eq

instance Show CubicBezier where
  show (CubicBezier a b c d) =
     "CubicBezier (" ++ show a ++ ") ("
                ++ show b ++ ") ("
                ++ show c ++ ") ("
                ++ show d ++ ")"

instance Transformable CubicBezier where
    {-# INLINE transform #-}
    transform f (CubicBezier a b c d) =
       CubicBezier (f a) (f b) (f c) $ f d
    transformM f (CubicBezier a b c d) =
       CubicBezier <$> f a <*> f b <*> f c <*> f d

instance PointFoldable CubicBezier where
    {-# INLINE foldPoints #-}
    foldPoints f acc (CubicBezier a b c d) =
        foldl' f acc [a, b, c, d]

-- | This datatype gather all the renderable primitives,
-- they are kept separated otherwise to allow specialization
-- on some specific algorithms. You can mix the different
-- primitives in a single call :
--
-- > fill [ toPrim $ CubicBezier (V2 50 20) (V2 90 60)
-- >                             (V2  5 100) (V2 50 140)
-- >      , toPrim $ Line (V2 50 140) (V2 120 80)
-- >      , toPrim $ Line (V2 120 80) (V2 50 20) ]
--
-- <<docimages/primitive_mixed.png>>
--
data Primitive
  = LinePrim !Line      -- ^ Primitive used for lines
  | BezierPrim !Bezier  -- ^ Primitive used for quadratic beziers curves
  | CubicBezierPrim !CubicBezier -- ^ Primitive used for cubic bezier curve
  deriving (Eq, Show)

-- | Generalizing constructors of the `Primitive` type to work
-- generically.
class Primitivable a where
  toPrim :: a -> Primitive

-- | @toPrim = id@
instance Primitivable Primitive where toPrim = id

-- | @toPrim = LinePrim@
instance Primitivable Line where toPrim = LinePrim

-- | @toPrim = BezierPrim@
instance Primitivable Bezier where toPrim = BezierPrim

-- | @toPrim = CubicBezierPrim@
instance Primitivable CubicBezier where toPrim = CubicBezierPrim

-- | All the rasterization works on lists of primitives,
-- in order to ease the use of the library, the Geometry
-- type class provides conversion facility, which help
-- generalising the geometry definition and avoid applying
-- Primitive constructor.
--
-- Also streamline the Path conversion.
class Geometry a where
  -- | Convert an element to a list of primitives
  -- to be rendered.
  toPrimitives :: a -> [Primitive]

  -- | Helper method to avoid overlaping instances.
  -- You shouldn't use it directly.
  listToPrims :: (Foldable f) => f a -> [Primitive]
  {-# INLINE listToPrims #-}
  listToPrims = F.concatMap toPrimitives . F.toList

instance Geometry Path where
  {-# INLINE toPrimitives #-}
  toPrimitives = pathToPrimitives

instance Geometry Primitive where
  toPrimitives e = [e]
  {-# INLINE listToPrims #-}
  listToPrims = F.toList -- Open question, is it optimised as `id`?

instance Geometry Line where
  {-# INLINE toPrimitives #-}
  toPrimitives e = [toPrim e]

instance Geometry Bezier where
  {-# INLINE toPrimitives #-}
  toPrimitives e = [toPrim e]

instance Geometry CubicBezier where
  {-# INLINE toPrimitives #-}
  toPrimitives e = [toPrim e]

-- | Generalize the geometry to any foldable container,
-- so you can throw any container to the the 'fill' or
-- 'stroke' function.
instance (Foldable f, Geometry a) => Geometry (f a) where
  {-# INLINE toPrimitives #-}
  toPrimitives = listToPrims


instance Transformable Primitive where
    {-# INLINE transform #-}
    transform f (LinePrim l) = LinePrim $ transform f l
    transform f (BezierPrim b) = BezierPrim $ transform f b
    transform f (CubicBezierPrim c) = CubicBezierPrim $ transform f c

    transformM f (LinePrim l) = LinePrim <$> transformM f l
    transformM f (BezierPrim b) = BezierPrim <$> transformM f b
    transformM f (CubicBezierPrim c) = CubicBezierPrim <$> transformM f c

instance PointFoldable Primitive where
    {-# INLINE foldPoints #-}
    foldPoints f acc = go
      where go (LinePrim l) = foldPoints f acc l
            go (BezierPrim b) = foldPoints f acc b
            go (CubicBezierPrim c) = foldPoints f acc c

instance {-# OVERLAPPABLE #-} (Traversable f, Transformable a)
      => Transformable (f a) where
    transform f = fmap (transform f)
    transformM f = mapM (transformM f)

instance (Foldable f, PointFoldable a)
      => PointFoldable (f a) where
    foldPoints f = foldl' (foldPoints f)

type Producer a = [a] -> [a]

type Container a = DList a

containerOfFunction :: ([a] -> [a]) -> Container a
containerOfFunction f = fromList $ f []

containerOfList :: [a] -> Container a
containerOfList = fromList

listOfContainer :: Container a -> [a]
listOfContainer = toList

-- | Describe a path in a way similar to many graphical
-- packages, using a "pen" position in memory and reusing
-- it for the next "move"
-- For example the example from Primitive could be rewritten:
--
-- > fill $ Path (V2 50 20) True
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

instance Transformable Path where
    {-# INLINE transform #-}
    transform f (Path orig close rest) =
        Path (f orig) close (transform f rest)

    transformM f (Path orig close rest) =
        Path <$> f orig <*> pure close <*> transformM f rest


instance PointFoldable Path where
    {-# INLINE foldPoints #-}
    foldPoints f acc (Path o _ rest) =
        foldPoints f (f acc o) rest

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

instance Transformable PathCommand where
    transform f (PathLineTo p) = PathLineTo $ f p
    transform f (PathQuadraticBezierCurveTo p1 p2) =
        PathQuadraticBezierCurveTo (f p1) $ f p2
    transform f (PathCubicBezierCurveTo p1 p2 p3) =
        PathCubicBezierCurveTo (f p1) (f p2) $ f p3

    transformM f (PathLineTo p) = PathLineTo <$> f p
    transformM f (PathQuadraticBezierCurveTo p1 p2) =
        PathQuadraticBezierCurveTo <$> f p1 <*> f p2
    transformM f (PathCubicBezierCurveTo p1 p2 p3) =
        PathCubicBezierCurveTo <$> f p1 <*> f p2 <*> f p3

instance PointFoldable PathCommand where
    foldPoints f acc (PathLineTo p) = f acc p
    foldPoints f acc (PathQuadraticBezierCurveTo p1 p2) =
        f (f acc p1) p2
    foldPoints f acc (PathCubicBezierCurveTo p1 p2 p3) =
        foldl' f acc [p1, p2, p3]

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

-- | Gives the orientation vector for the start of the
-- primitive.
firstTangeantOf :: Primitive -> Vector
firstTangeantOf p = case p of
  LinePrim (Line p0 p1) -> p1 ^-^ p0
  BezierPrim (Bezier p0 p1 p2) ->
      (p1 ^-^ p0) `ifBigEnough` (p2 ^-^ p1)
  CubicBezierPrim (CubicBezier p0 p1 p2 _) -> 
       (p1 ^-^ p0) `ifBigEnough` (p2 ^-^ p1)
 where
   ifBigEnough a b | nearZero a = b
                   | otherwise = a

-- | Gives the orientation vector at the end of the
-- primitive.
lastTangeantOf :: Primitive -> Vector
lastTangeantOf p = case p of
  LinePrim (Line p0 p1) -> p1 ^-^ p0
  BezierPrim (Bezier _ p1 p2) -> p2 ^-^ p1
  CubicBezierPrim (CubicBezier _ _ p2 p3) -> p3 ^-^ p2

-- | Extract the first point of the primitive.
firstPointOf :: Primitive -> Point
firstPointOf p = case p of
  LinePrim (Line p0 _) -> p0
  BezierPrim (Bezier p0 _ _) -> p0
  CubicBezierPrim (CubicBezier p0 _ _ _) -> p0

-- | Return the last point of a given primitive.
lastPointOf :: Primitive -> Point
lastPointOf p = case p of
  LinePrim (Line _ p0) -> p0
  BezierPrim (Bezier _ _ p0) -> p0
  CubicBezierPrim (CubicBezier _ _ _ p0) -> p0

resplit :: [Primitive] -> [[Primitive]]
resplit = uncurry (:) . go where
  go [] = ([], [])
  go (x:xs@(y:_)) | lastPointOf x `isDistingableFrom` firstPointOf y =
      ([x], after:rest) where (after, rest) = go xs
  go (x:xs) = (x:curr, rest) where (curr, rest) = go xs

