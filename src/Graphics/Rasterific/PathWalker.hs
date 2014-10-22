{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module help the walking of path of any shape,
-- being able to return the current position and the
-- actual orientation.
module Graphics.Rasterific.PathWalker( PathWalkerT
                                     , PathWalker
                                     , runPathWalking
                                     , advanceBy
                                     , currentPosition
                                     , currentTangeant

                                     , PathImage( .. )
                                     , drawImageOnPath
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

data WalkerState = WalkerState
    { _walkerPrims :: ![Primitive]
    }
    deriving (Eq, Show)

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

data PathImage px = PathImage
  { _pimgOrder      :: !(DrawOrder px)
  , _pimgDeltaX     :: !Float
  , _pimgDeltaY     :: !Float
  }

type PathDrawer m px = Transformation -> DrawOrder px -> m ()

drawImageOnPath :: Monad m
                => PathDrawer m px -> Float -> Path -> [PathImage px]
                -> m ()
drawImageOnPath drawer baseline path = runPathWalking path . go Nothing where
  go _ [] = return ()
  go prevX (img : rest) = do
    let bounds =
          foldMap (foldMap planeBounds) . _orderPrimitives $ _pimgOrder img
        width = boundWidth bounds
        cx = maybe startX id prevX
        V2 startX _ = boundLowerLeftCorner bounds
        V2 endX _ = _planeMaxBound bounds
        halfWidth = width / 2
        spaceWidth = abs $ startX - cx
        translation = V2 (negate halfWidth - startX) (- baseline)
    if bounds == mempty then go prevX rest
    else do
      advanceBy (halfWidth + spaceWidth)
      mayPos <- currentPosition
      mayDir <- currentTangeant
      case (,) <$> mayPos <*> mayDir of
        Nothing -> return () -- out of path, stop drawing
        Just (pos, dir) -> do
          let imageTransform =
                  translate pos <> toNewXBase dir <> translate translation
          lift $ drawer imageTransform (_pimgOrder img)
          advanceBy halfWidth
          go (Just endX) rest

