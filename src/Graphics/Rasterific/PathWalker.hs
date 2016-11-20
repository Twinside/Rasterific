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

import Data.Monoid( (<>) )

import Control.Monad.Identity( Identity )
import Control.Monad.State( StateT
                          , MonadTrans
                          , lift
                          , evalStateT
                          , modify
                          , gets )
import Data.Maybe( fromMaybe )

import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.StrokeInternal
import Graphics.Rasterific.PlaneBoundable
import Graphics.Rasterific.Immediate

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

-- | Obtain the current position if we are still on the
-- path, if not, return Nothing.
currentPosition :: (Monad m) => PathWalkerT m (Maybe Point)
currentPosition = PathWalkerT $ gets (currPos . _walkerPrims)
  where
    currPos [] = Nothing
    currPos (prim:_) = Just $ firstPointOf prim

-- | Obtain the current tangeant of the path if we're still
-- on it. Return Nothing otherwise.
currentTangeant :: (Monad m) => PathWalkerT m (Maybe Vector)
currentTangeant = PathWalkerT $ gets (currTangeant . _walkerPrims)
  where
    currTangeant [] = Nothing
    currTangeant (prim:_) = Just . normalize $ firstTangeantOf prim

-- | Callback function in charge to transform the DrawOrder
-- given the transformation to place it on the path.
type PathDrawer m px =
    Transformation -> PlaneBound -> DrawOrder px -> m ()

-- | This function is the workhorse of the placement, it will
-- walk the path and calculate the appropriate transformation
-- for every order.
drawOrdersOnPath :: Monad m
                 => PathDrawer m px  -- ^ Function handling the placement of the order.
                 -> Float            -- ^ Starting offset
                 -> Float            -- ^ Baseline vertical position in the orders.
                 -> Path             -- ^ Path on which to place the orders.
                 -> [DrawOrder px]   -- ^ Orders to place on a path.
                 -> m ()
drawOrdersOnPath drawer startOffset baseline path orders =
    runPathWalking path $ advanceBy startOffset >> go Nothing orders where
  go _ [] = return ()
  go prevX (img : rest) = do
    let bounds = planeBounds img
        width = boundWidth bounds
        cx = fromMaybe startX prevX
        V2 startX _ = boundLowerLeftCorner bounds
        V2 endX _ = _planeMaxBound bounds
        halfWidth = width / 2
        spaceWidth = abs $ startX - cx
        translation = V2 (negate startX - halfWidth) (- baseline)

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
          lift $ drawer imageTransform bounds img
          advanceBy halfWidth
          go (Just endX) rest

