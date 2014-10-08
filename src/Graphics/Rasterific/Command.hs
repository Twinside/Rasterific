{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Rasterific.Command ( Drawing
                                   , DrawCommand( .. )
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

-- | Monad used to record the drawing actions.
type Drawing px = F (DrawCommand px)

data DrawCommand px next
    = Fill FillMethod [Primitive] next
    | Stroke Float Join (Cap, Cap) [Primitive] next
    | DashedStroke Float DashPattern Float Join (Cap, Cap) [Primitive] next
    | TextFill Font PointSize Point String next
    | SetTexture (Texture px)
                 (Drawing px ()) next
    | WithCliping (forall innerPixel. Drawing innerPixel ())
                  (Drawing px ()) next
    | WithTransform Transformation (Drawing px ()) next
    | WithPathOrientation Path (Drawing px ()) next

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
  go (Free (WithPathOrientation path drawing next)) =
    "withPathOrientation (" ++ show path ++ ") ("
                            ++ go (fromF drawing) ++ ") >>= "
                            ++ go next
  go (Free (Fill _ prims next)) =
    "fill " ++ show prims ++ " >>=\n" ++   go next
  go (Free (TextFill _ _ _ text next)) =
    "-- Text : " ++ text ++ "\n" ++   go next
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
    fmap f (TextFill font size pos str next) =
        TextFill font size pos str $ f next
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
    fmap f (WithPathOrientation path draw next) =
        WithPathOrientation path draw $ f next

instance Monoid (Drawing px ()) where
    mempty = return ()
    mappend a b = a >> b

