{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module help the walking of path of any shape,
-- being able to return the current position and the
-- actual orientation.
module Graphics.Rasterific.PathWalker( PathWalkerT
                                     , PathWalker
                                     , PathDrawer
                                     , runPathWalking
                                     , advanceBy
                                     , currentPosition
                                     , currentTangeant

                                     , drawOrdersOnPath
                                     ) where

import Data.Foldable( foldMap )
import Control.Applicative( Applicative, (<$>), (<*>) )
import Control.Monad.Identity( Identity )
import Control.Monad.State( StateT
                          , MonadTrans
                          , lift
                          , evalStateT
                          , modify
                          , gets )
import Data.Monoid( mempty, (<>) )
import Data.Maybe( fromMaybe )

import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.StrokeInternal
import Graphics.Rasterific.PlaneBoundable
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Command

-- | The walking transformer monad.
newtype PathWalkerT m a = PathWalkerT (StateT WalkerState m a)
    deriving (Monad, Applicative, Functor, MonadTrans)

-- | Simpler alias if monad transformers are not
-- needed.
type PathWalker a = PathWalkerT Identity a

-- | State of the path walker, just a bunch of primitives
-- with continuity guarantee. The continuity is guaranteed
-- by the Path used to derive this primitives.
data WalkerState = WalkerState
    { _walkerPrims :: ![Primitive]
    }

-- | Create a path walker from a given path
runPathWalking :: (Monad m) => Path -> PathWalkerT m a -> m a
runPathWalking path (PathWalkerT walker) = evalStateT walker initialState
  where
    initialState = WalkerState primsOfPath
    primsOfPath = listOfContainer
                . flatten
                . containerOfList
                $ pathToPrimitives path

-- | Advance by the given amount of pixels on the path.
advanceBy :: Monad m => Float -> PathWalkerT m ()
advanceBy by = PathWalkerT . modify $ \s ->
  let (_, leftPrimitives) = splitPrimitiveUntil by $ _walkerPrims s in
  s { _walkerPrims = leftPrimitives }

-- | Extract the first point of the primitive.
firstPointOf :: Primitive -> Point
firstPointOf p = case p of
  LinePrim (Line p0 _) -> p0
  BezierPrim (Bezier p0 _ _) -> p0
  CubicBezierPrim (CubicBezier p0 _ _ _) -> p0

-- | Obtain the current position if we are still on the
-- path, if not, return Nothing.
currentPosition :: (Monad m) => PathWalkerT m (Maybe Point)
currentPosition = PathWalkerT $ gets (currPos . _walkerPrims)
  where
    currPos [] = Nothing
    currPos (prim:_) = Just $ firstPointOf prim

-- | Gives the orientation vector for the current point on
-- the path.
firstTangeantOf :: Primitive -> Vector
firstTangeantOf p = case p of
  LinePrim (Line p0 p1) -> p1 ^-^ p0
  BezierPrim (Bezier p0 p1 _) -> p1 ^-^ p0
  CubicBezierPrim (CubicBezier p0 p1 _ _) -> p1 ^-^ p0

-- | Obtain the current tangeant of the path if we're still
-- on it. Return Nothing otherwise.
currentTangeant :: (Monad m) => PathWalkerT m (Maybe Vector)
currentTangeant = PathWalkerT $ gets (currTangeant . _walkerPrims)
  where
    currTangeant [] = Nothing
    currTangeant (prim:_) = Just . normalize $ firstTangeantOf prim

-- | Callback function in charge to transform the DrawOrder
-- given the transformation to place it on the path.
type PathDrawer m px = Transformation -> DrawOrder px -> m ()

alignFor :: OrderAlignment -> Float -> Point -> Point
alignFor AlignOnLeft _ p = p
alignFor AlignOnMiddle halfWidth p =
    V2 (negate halfWidth) 0 ^+^ p

-- | This function is the workhorse of the placement, it will
-- walk the path and calculate the appropriate transformation
-- for every order.
drawOrdersOnPath :: Monad m
                 => PathDrawer m px  -- ^ Function handling the placement of the order.
                 -> OrderAlignment   -- ^ How to align the order
                 -> Float            -- ^ Baseline vertical position in the orders.
                 -> Path             -- ^ Path on which to place the orders.
                 -> [DrawOrder px]   -- ^ Orders to place on a path.
                 -> m ()
drawOrdersOnPath drawer alignment baseline path =
        runPathWalking path . go Nothing where
  go _ [] = return ()
  go prevX (img : rest) = do
    let bounds =
          foldMap (foldMap planeBounds) $ _orderPrimitives img
        width = boundWidth bounds
        cx = fromMaybe startX prevX
        V2 startX _ = boundLowerLeftCorner bounds
        V2 endX _ = _planeMaxBound bounds
        halfWidth = width / 2
        spaceWidth = abs $ startX - cx
        translation = alignFor alignment halfWidth
                    $ V2 (negate startX) (- baseline)

    if bounds == mempty then go prevX rest
    else do
      advanceBy (halfWidth + spaceWidth)
      mayPos <- currentPosition
      mayDir <- currentTangeant
      case (,) <$> mayPos <*> mayDir of
        Nothing -> return () -- out of path, stop drawing
        Just (pos, dir) -> do
          let imageTransform =
                  translate pos <> toNewXBase dir
                                <> translate translation
          lift $ drawer imageTransform img
          advanceBy halfWidth
          go (Just endX) rest

