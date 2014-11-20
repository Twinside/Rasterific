{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Rasterific.Command ( Drawing
                                   , DrawCommand( .. )
                                   , TextRange( .. )
                                   , OrderAlignment( .. )
                                   , dumpDrawing
                                   ) where

import Control.Monad.Free( Free( .. ), liftF )
import Control.Monad.Free.Church( F, fromF )
import Data.Monoid( Monoid( .. ) )
import Codec.Picture.Types( Pixel( .. ), Pixel8 )

import Graphics.Rasterific.Types
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Shading

import Graphics.Text.TrueType( Font, PointSize )

import Text.Printf
-- | Monad used to record the drawing actions.
type Drawing px = F (DrawCommand px)

-- | Structure defining how to render a text range
data TextRange px = TextRange
    { _textFont    :: Font      -- ^ Font used during the rendering
    , _textSize    :: PointSize -- ^ Size of the text (in pixels)
    , _text        :: String    -- ^ Text to draw
      -- | Texture to use for drawing, if Nothing, the currently
      -- active texture is used.
    , _textTexture :: Maybe (Texture px)
    }

instance Show (TextRange px) where
  show t = printf "TextRange %d \"%s\"" (_textSize t) (_text t)

-- | Help align the order, either on the left of the order,
-- so the left margin will be aligned with the path, or
-- aligned with the middle of the char.
--
-- The middle of the order is determined using the bounds of
-- of geometry.
data OrderAlignment
    = AlignOnLeft       -- ^ Align with the left border
    | AlignOnMiddle     -- ^ Align with the middle (horizontally) of the order.
    deriving (Eq, Show)

data DrawCommand px next
    = Fill FillMethod [Primitive] next
    | Stroke Float Join (Cap, Cap) [Primitive] next
    | DashedStroke Float DashPattern Float Join (Cap, Cap) [Primitive] next
    | TextFill Point [TextRange px] next
    | SetTexture (Texture px)
                 (Drawing px ()) next
    | WithCliping (forall innerPixel. Drawing innerPixel ())
                  (Drawing px ()) next
    | WithTransform Transformation (Drawing px ()) next
    | WithPathOrientation Path OrderAlignment Float (Drawing px ()) next

-- | This function will spit out drawing instructions to
-- help debugging.
--
-- The outputted code looks like Haskell, but there is no
-- guarantee that it is compilable.
dumpDrawing :: ( Show px
               , Show (PixelBaseComponent px)
               , PixelBaseComponent (PixelBaseComponent px)
                    ~ (PixelBaseComponent px)

               ) => Drawing px () -> String
dumpDrawing = go . fromF where
  go ::
        ( Show px
        , Show (PixelBaseComponent px)
        , PixelBaseComponent (PixelBaseComponent px)
                    ~ (PixelBaseComponent px)

        ) => Free (DrawCommand px) () -> String
  go (Pure ()) = "return ()"
  go (Free (WithPathOrientation path align point drawing next)) =
    "withPathOrientation (" ++ show path ++ ") ("
                            ++ show align ++ ") ("
                            ++ show point ++ ") ("
                            ++ go (fromF drawing) ++ ") >>= "
                            ++ go next
  go (Free (Fill _ prims next)) =
    "fill " ++ show prims ++ " >>=\n" ++   go next
  go (Free (TextFill _ texts next)) =
   concat  ["-- Text : " ++ _text t ++ "\n" | t <- texts] ++ go next
  go (Free (SetTexture tx drawing next)) =
    "withTexture (" ++ dumpTexture tx ++ ") (" ++
              go (fromF drawing) ++ ") >>=\n" ++ go next
  go (Free (DashedStroke o pat w j cap prims next)) =
    "dashedStrokeWithOffset "
              ++ show o ++ " "
              ++ show pat ++ " "
              ++ show w ++ " ("
              ++ show j ++ ") "
              ++ show cap ++ " "
              ++ show prims ++ " >>=\n" ++   go next
  go (Free (Stroke w j cap prims next)) =
    "stroke " ++ show w ++ " ("
              ++ show j ++ ") "
              ++ show cap ++ " "
              ++ show prims ++ " >>=\n" ++   go next
  go (Free (WithTransform trans sub next)) =
    "withTransform (" ++ show trans ++ ") ("
                      ++ go (fromF sub) ++ ") >>=\n "
                      ++ go next
  go (Free (WithCliping clipping draw next)) =
    "withClipping (" ++ go (fromF $ withTexture clipTexture clipping)
                     ++ ")\n" ++
        "         (" ++ go (fromF draw) ++ ")\n >>= " ++
              go next
        where clipTexture = uniformTexture (0xFF :: Pixel8)
              withTexture texture subActions =
                 liftF $ SetTexture texture subActions ()


instance Functor (DrawCommand px) where
    fmap f (TextFill pos texts next) =
        TextFill pos texts $ f next
    fmap f (Fill method  prims next) = Fill method prims $ f next
    fmap f (SetTexture t sub next) = SetTexture t sub $ f next
    fmap f (WithCliping sub com next) =
        WithCliping sub com $ f next
    fmap f (Stroke w j caps prims next) =
        Stroke w j caps prims $ f next
    fmap f (DashedStroke st pat w j caps prims next) =
        DashedStroke st pat w j caps prims $ f next
    fmap f (WithTransform trans draw next) =
        WithTransform trans draw $ f next
    fmap f (WithPathOrientation path align point draw next) =
        WithPathOrientation path align point draw $ f next

instance Monoid (Drawing px ()) where
    mempty = return ()
    mappend a b = a >> b

