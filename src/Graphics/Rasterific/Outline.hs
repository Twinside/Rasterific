-- | This module provides helper functions to create outlines
-- of shapes.
module Graphics.Rasterific.Outline
    ( StrokeWidth
    , strokize
    , S.dashedStrokize 
    , S.approximatePathLength
    ) where

import Graphics.Rasterific.Types
import qualified Graphics.Rasterific.StrokeInternal as S

-- | This function will create the outline of a given geometry
-- given a path. You can then stroke it.
--
-- > stroke 3 (JoinMiter 0) (CapStraight 0, CapStraight 0) $
-- >     strokize 40 JoinRound (CapRound, CapRound) $
-- >         CubicBezier (V2  40 160) (V2 40   40)
-- >                     (V2 160  40) (V2 160 160)
--
-- <<docimages/strokize_path.png>>
--
strokize :: Geometry geom
         => StrokeWidth -- ^ Stroke width
         -> Join        -- ^ Which kind of join will be used
         -> (Cap, Cap)  -- ^ Start and end capping.
         -> geom        -- ^ List of elements to strokize
         -> [Primitive]
strokize w j c = listOfContainer . S.strokize w j c

