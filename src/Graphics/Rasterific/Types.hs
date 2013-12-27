module Graphics.Rasterific.Types
    ( Vector
    , Point
    , Cap( .. )
    , Join( .. )
    ) where

import Linear( V2( .. ) )

type Vector = V2 Float
type Point = V2 Float

-- | Describe how we will "finish" the stroking
-- that don't loop.
data Cap =
    -- | Create a straight caping on the stroke.
    -- Cap value should be positive
    CapStraight Float 
  | CapRound          -- ^ Create a rounded caping on the stroke.
  deriving (Eq, Show)

data Join =
    JoinRound
  | JoinMiter Float
  deriving (Eq, Show)

