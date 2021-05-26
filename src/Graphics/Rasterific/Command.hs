{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Rasterific.Command ( Drawing
                                   , DrawCommand( .. )
                                   , DrawContext
                                   , TextRange( .. )
                                   , dumpDrawing
                                   , Texture( .. )
                                   , Gradient
                                   , ShaderFunction
                                   , ImageTransformer
                                   , dumpTexture
                                   ) where

import Data.Kind ( Type )

import Control.Monad.ST( ST )
import Control.Monad.State( StateT )
import Control.Monad.Primitive( PrimState )
import Control.Monad.Free( Free( .. ), liftF )
import Control.Monad.Free.Church( F, fromF )
import Codec.Picture.Types( Image, Pixel( .. ), Pixel8 )

import Codec.Picture.Types( MutableImage )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PatchTypes

import Graphics.Text.TrueType( Font, PointSize )

-- | Monad used to record the drawing actions.
type Drawing px = F (DrawCommand px)

-- | Monad used to describe the drawing context.
type DrawContext m px =
    StateT (MutableImage (PrimState m) px) m

-- | Structure defining how to render a text range
data TextRange px = TextRange
    { _textFont    :: Font      -- ^ Font used during the rendering
    , _textSize    :: PointSize -- ^ Size of the text (in pixels)
    , _text        :: String    -- ^ Text to draw
      -- | Texture to use for drawing, if Nothing, the currently
      -- active texture is used.
    , _textTexture :: Maybe (Texture px)
    }

type ShaderFunction px = Float -> Float -> px

type ImageTransformer px = Int -> Int -> px -> px

-- | A gradient definition is just a list of stop
-- and pixel values. For instance for a simple gradient
-- of black to white, the finition would be :
--
-- > [(0, PixelRGBA8 0 0 0 255), (1, PixelRGBA8 255 255 255 255)]
-- 
-- the first stop value must be zero and the last, one.
--
type Gradient px = [(Float, px)]

-- | Reification of texture type
data Texture (px :: Type)
  = SolidTexture !px
  | LinearGradientTexture !(Gradient px) !Line 
  | RadialGradientTexture !(Gradient px) !Point !Float
  | RadialGradientWithFocusTexture !(Gradient px) !Point !Float !Point
  | WithSampler    !SamplerRepeat (Texture px)
  | WithTextureTransform !Transformation (Texture px)
  | SampledTexture !(Image px)
  | RawTexture     !(Image px)
  | ShaderTexture  !(ShaderFunction px)
  | ModulateTexture (Texture px) (Texture (PixelBaseComponent px))
  | AlphaModulateTexture (Texture px) (Texture (PixelBaseComponent px))
  | PatternTexture !Int !Int !px (Drawing px ()) (Image px)
  | MeshPatchTexture !PatchInterpolation !(MeshPatch px)


data DrawCommand px next
  = Fill FillMethod [Primitive] next
  | CustomRender (forall s. DrawContext (ST s) px ()) next
  | MeshPatchRender !PatchInterpolation (MeshPatch px) next
  | Stroke Float Join (Cap, Cap) [Primitive] next
  | DashedStroke Float DashPattern Float Join (Cap, Cap) [Primitive] next
  | TextFill Point [TextRange px] next
  | SetTexture (Texture px)
               (Drawing px ()) next
  | WithGlobalOpacity (PixelBaseComponent px) (Drawing px ()) next
  | WithImageEffect (Image px -> ImageTransformer px) (Drawing px ()) next
  | WithCliping (forall innerPixel. Drawing innerPixel ())
                (Drawing px ()) next
  | WithTransform Transformation (Drawing px ()) next
  | WithPathOrientation Path Float (Drawing px ()) next

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
  go (Free (MeshPatchRender i m next)) =
    "renderMeshPatch (" ++ show i ++ ") (" ++ show m ++ ") >>= " ++ go next
  go (Free (CustomRender _r next)) =
    "customRender _ >>= " ++ go next
  go (Free (WithImageEffect _effect sub next)) =
    "withImageEffect ({- fun -}) (" ++ go (fromF sub) ++ ") >>= " ++ go next
  go (Free (WithGlobalOpacity opa sub next)) =
    "withGlobalOpacity " ++ show opa ++ " (" ++ go (fromF sub) ++ ") >>= " ++ go next
  go (Free (WithPathOrientation path point drawing next)) =
    "withPathOrientation (" ++ show path ++ ") ("
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
        where clipTexture = SolidTexture (0xFF :: Pixel8)
              withTexture texture subActions =
                 liftF $ SetTexture texture subActions ()

dumpTexture :: ( Show px
               , Show (PixelBaseComponent px)
               , PixelBaseComponent (PixelBaseComponent px)
                    ~ (PixelBaseComponent px)
               ) => Texture px -> String
dumpTexture (SolidTexture px) = "uniformTexture (" ++ show px ++ ")"
dumpTexture (MeshPatchTexture i mpx) = "meshTexture (" ++ show i ++ ") (" ++ show mpx ++ ")"
dumpTexture (LinearGradientTexture grad (Line a b)) =
    "linearGradientTexture " ++ show grad ++ " (" ++ show a ++ ") (" ++ show b ++ ")"
dumpTexture (RadialGradientTexture grad p rad) =
    "radialGradientTexture " ++ show grad ++ " (" ++ show p ++ ") " ++ show rad
dumpTexture (RadialGradientWithFocusTexture grad center rad focus) =
    "radialGradientWithFocusTexture " ++ show grad ++ " (" ++ show center 
                                      ++ ") " ++ show rad ++ " (" ++ show focus ++ ")"
dumpTexture (WithSampler sampler sub) =
    "withSampler " ++ show sampler ++ " (" ++ dumpTexture sub ++ ")"
dumpTexture (WithTextureTransform trans sub) =
    "transformTexture (" ++ show trans ++ ") (" ++ dumpTexture sub ++ ")"
dumpTexture (SampledTexture _) = "sampledImageTexture <IMG>"
dumpTexture (RawTexture _) = "<RAWTEXTURE>"
dumpTexture (ShaderTexture _) = "shaderFunction <FUNCTION>"
dumpTexture (ModulateTexture sub mask) =
    "modulateTexture (" ++ dumpTexture sub ++ ") ("
                        ++ dumpTexture mask ++ ")"
dumpTexture (AlphaModulateTexture sub mask) =
    "alphaModulate (" ++ dumpTexture sub ++ ") ("
                      ++ dumpTexture mask ++ ")"
dumpTexture (PatternTexture w h px sub _) =
    "patternTexture " ++ show w ++ " " ++ show h ++ " " ++ show px
                      ++ " (" ++ dumpDrawing sub ++ ")"


instance Functor (DrawCommand px) where
    fmap f (WithImageEffect effect sub next) =
        WithImageEffect effect sub $ f next
    fmap f (TextFill pos texts next) =
        TextFill pos texts $ f next
    fmap f (CustomRender m next) =
        CustomRender m $ f next
    fmap f (WithGlobalOpacity opa sub next) =
        WithGlobalOpacity opa sub $ f next
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
    fmap f (WithPathOrientation path point draw next) =
        WithPathOrientation path point draw $ f next
    fmap f (MeshPatchRender i mesh next) =
        MeshPatchRender i mesh $ f next

instance Semigroup (Drawing px ()) where
    (<>) a b = a >> b

instance Monoid (Drawing px ()) where
    mempty = return ()
    mappend = (<>)

