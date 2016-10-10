{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Graphics.Rasterific.Shading
    ( transformTextureToFiller
    , sampledImageShader
    , plotOpaquePixel
    ) where

import Control.Monad.ST( ST )
import Control.Monad.Primitive( PrimState
                              -- one day (GHC >= 7.10 ?)
                              , PrimMonad
                              )
import Data.Fixed( mod' )
import Data.Monoid( (<>) )
import Graphics.Rasterific.Command
import Graphics.Rasterific.Linear
             ( V2( .. )
             , (^-^)
             , (^/)
             , dot
             , norm
             )

import qualified Data.Vector as V

import Codec.Picture.Types( Pixel( .. )
                          , Image( .. )
                          , MutableImage( .. )
                          , Pixel8
                          , PixelRGBA8
                          , unsafeWritePixelBetweenAt
                          , readPackedPixelAt
                          , writePackedPixelAt
                          )

import Graphics.Rasterific.Types( Point
                                , Vector
                                , Line( .. )
                                , SamplerRepeat( .. ) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Rasterize
import Graphics.Rasterific.Compositor( Modulable( .. )
                                     , ModulablePixel
                                     , RenderablePixel
                                     , compositionAlpha )


data TextureSpaceInfo = TextureSpaceInfo
    { _tsStart     :: {-# UNPACK #-} !Point
    , _tsDelta     :: {-# UNPACK #-} !Vector
    , _tsCoverage  :: {-# UNPACK #-} !Float
    , _tsRepeat    :: {-# UNPACK #-} !Int
    , _tsBaseIndex :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Show)

type CoverageFiller m px =
    MutableImage (PrimState m) px -> CoverageSpan -> m ()

type Filler m =
    TextureSpaceInfo -> m ()

-- | Right now, we must stick to ST, due to the fact that
-- we can't specialize with parameterized monad :(
solidColor :: forall s px . (ModulablePixel px)
           => px -> MutableImage s px -> Filler (ST s)
{-# SPECIALIZE solidColor :: PixelRGBA8 -> MutableImage s PixelRGBA8
                          -> TextureSpaceInfo -> ST s () #-}
{-# SPECIALIZE solidColor :: Pixel8 -> MutableImage s Pixel8
                          -> TextureSpaceInfo -> ST s () #-}
solidColor color _ tsInfo
    | pixelOpacity color == emptyValue || _tsCoverage tsInfo <= 0 =
        return ()
solidColor color img tsInfo
    -- We are in the case fully opaque, so we can
    -- just overwrite what was there before
    | pixelOpacity color == fullOpacity && _tsCoverage tsInfo >= 1 =
        unsafeWritePixelBetweenAt img color (_tsBaseIndex tsInfo) maxi
        {-go 0 $ _tsBaseIndex tsInfo-}
  where
    !fullOpacity = fullValue :: PixelBaseComponent px
    !maxi = _tsRepeat tsInfo

-- We can be transparent, so perform alpha blending.
solidColor color img tsInfo = go 0 $ _tsBaseIndex tsInfo
  where
    !opacity = pixelOpacity color
    !(scanCoverage,_) = clampCoverage $_tsCoverage tsInfo
    !(cov, icov) = coverageModulate scanCoverage opacity
    !maxi = _tsRepeat tsInfo
    !compCount = componentCount (undefined :: px)

    go count  _ | count >= maxi = return ()
    go !count !idx = do
      oldPixel <- readPackedPixelAt img idx
      writePackedPixelAt img idx
        $ compositionAlpha cov icov oldPixel color
      go (count + 1) $ idx + compCount


-- | Plot a single pixel on the resulting image.
plotOpaquePixel :: forall m px. (ModulablePixel px, PrimMonad m)
                => MutableImage (PrimState m) px -> px -> Int -> Int
                -> m ()
{-# INLINE plotOpaquePixel #-}
plotOpaquePixel img _color x y
   | x < 0 || y < 0 || 
     x >= mutableImageWidth img || y >= mutableImageHeight img = return ()
plotOpaquePixel img color x y = do
  let !idx = (y * mutableImageWidth img + x) * (componentCount (undefined :: px))
  writePackedPixelAt img idx color

shaderFiller :: forall s px . (ModulablePixel px)
             => ShaderFunction px -> MutableImage s px
             -> Filler (ST s)
{-# SPECIALIZE shaderFiller :: ShaderFunction PixelRGBA8
                            -> MutableImage s PixelRGBA8
                            -> Filler (ST s) #-}
{-# SPECIALIZE shaderFiller :: ShaderFunction Pixel8
                            -> MutableImage s Pixel8
                            -> Filler (ST s) #-}
shaderFiller shader img tsInfo =
    go 0 (_tsBaseIndex tsInfo) xStart yStart
  where
    !(scanCoverage,_) = clampCoverage $_tsCoverage tsInfo
    !maxi = _tsRepeat tsInfo
    !compCount = componentCount (undefined :: px)
    (V2 xStart yStart) = _tsStart tsInfo
    (V2 dx dy) = _tsDelta tsInfo

    go count  _ _ _ | count >= maxi = return ()
    go !count !idx !x !y = do
      let !color = shader x y
          !opacity = pixelOpacity color
          (cov, icov) = coverageModulate scanCoverage opacity
      oldPixel <- readPackedPixelAt img idx
      writePackedPixelAt img idx
        $ compositionAlpha cov icov oldPixel color
      go (count + 1) (idx + compCount) (x + dx) (y + dy)

prepareInfoNoTransform :: (Pixel px)
                       => MutableImage s px -> CoverageSpan
                       -> TextureSpaceInfo
prepareInfoNoTransform img coverage = TextureSpaceInfo
    { _tsStart     = V2 (_coverageX coverage) (_coverageY coverage)
    , _tsDelta     = V2 1 0
    , _tsCoverage  = _coverageVal coverage
    , _tsRepeat    = floor $ _coverageLength coverage
    , _tsBaseIndex =
        mutablePixelBaseIndex img (floor $ _coverageX coverage)
                                  (floor $ _coverageY coverage)
    }

prepareInfo :: (Pixel px)
            => Maybe Transformation -> MutableImage s px -> CoverageSpan
            -> TextureSpaceInfo
prepareInfo Nothing img covSpan = prepareInfoNoTransform img covSpan
prepareInfo (Just t) img covSpan = TextureSpaceInfo
    { _tsStart     = applyTransformation t
                   $ V2 (_coverageX covSpan) (_coverageY covSpan)
    , _tsDelta     = applyVectorTransformation t $ V2 1 0
    , _tsCoverage  = _coverageVal covSpan
    , _tsRepeat    = floor $ _coverageLength covSpan
    , _tsBaseIndex =
        mutablePixelBaseIndex img (floor $ _coverageX covSpan)
                                  (floor $ _coverageY covSpan)
    }

combineTransform :: Maybe Transformation -> Transformation
                 -> Maybe Transformation
combineTransform Nothing a = Just a
combineTransform (Just v) a = Just $ v <> a

withTrans :: Maybe Transformation -> ShaderFunction px
          -> ShaderFunction px
withTrans Nothing shader = shader
withTrans (Just v) shader = \x y ->
    let V2 x' y' = applyTransformation v (V2 x y) in
    shader x' y'

-- | The intent of shader texture is to provide ease of implementation
-- If possible providing a custom filler will be more efficient,
-- like already done for the solid colors.
shaderOfTexture :: forall px . RenderablePixel px
                => Maybe Transformation -> SamplerRepeat -> Texture px
                -> ShaderFunction px
{-# SPECIALIZE
    shaderOfTexture :: Maybe Transformation -> SamplerRepeat -> Texture PixelRGBA8
                    -> ShaderFunction PixelRGBA8 #-}
{-# SPECIALIZE
    shaderOfTexture :: Maybe Transformation -> SamplerRepeat -> Texture Pixel8
                    -> ShaderFunction Pixel8 #-}
shaderOfTexture _ _ (SolidTexture px) = \_ _ -> px
shaderOfTexture _ _ (MeshPatchTexture _ _) = error "MeshPatch should be precomputed"
shaderOfTexture trans sampling (LinearGradientTexture grad (Line a b)) =
  withTrans trans $ linearGradientShader grad a b sampling
shaderOfTexture trans sampling (RadialGradientTexture grad center radius) =
  withTrans trans $ radialGradientShader grad center radius sampling
shaderOfTexture trans sampling (RadialGradientWithFocusTexture grad center 
                                                    radius focus) =
  withTrans trans
             $ radialGradientWithFocusShader grad center radius focus
                                              sampling
shaderOfTexture trans _ (WithSampler sampler sub) =
  shaderOfTexture trans sampler sub
shaderOfTexture trans sampling (WithTextureTransform transform sub) =
  shaderOfTexture (combineTransform trans transform) sampling sub
shaderOfTexture trans sampling (SampledTexture img) =
  withTrans trans $ sampledImageShader img sampling
shaderOfTexture trans _ (ShaderTexture func) =
  withTrans trans func
shaderOfTexture trans _ (RawTexture img) =
  withTrans trans $ imageShader img
shaderOfTexture trans _sampling (PatternTexture _ _ _ _ img) =
  shaderOfTexture trans SamplerRepeat $ SampledTexture img
shaderOfTexture trans sampling (ModulateTexture texture modulation) =
  modulateTexture (shaderOfTexture trans sampling texture)
                  (shaderOfTexture trans sampling modulation)
shaderOfTexture trans sampling (AlphaModulateTexture texture modulation) =
  alphaModulateTexture
    (shaderOfTexture trans sampling texture)
    (shaderOfTexture trans sampling modulation)


-- | This function will interpret the texture description, helping
-- prepare and optimize the real calculation
transformTextureToFiller
    :: (RenderablePixel px)
    => Texture px -> CoverageFiller (ST s) px
transformTextureToFiller = go Nothing SamplerPad
  where
    go _ _ (SolidTexture px) =
        \img -> solidColor px img . prepareInfoNoTransform img
    go trans sampling (WithTextureTransform transform sub) =
        go (combineTransform trans transform) sampling sub
    go trans _ (WithSampler sampler sub) =
        go trans sampler sub
    go trans sampling tex =
        \img -> shaderFiller shader img . prepareInfo trans img
            where shader = shaderOfTexture Nothing sampling tex

type GradientArray px = V.Vector (Float, px)

repeatGradient :: Float -> Float
repeatGradient s = s - fromIntegral (floor s :: Int)

reflectGradient :: Float -> Float
reflectGradient s =
    abs (abs (s - 1) `mod'` 2 - 1)
   
gradientColorAt :: ModulablePixel px
                => GradientArray px -> Float -> px
{-# SPECIALIZE
     gradientColorAt :: GradientArray PixelRGBA8 -> Float -> PixelRGBA8 #-}
{-# SPECIALIZE
     gradientColorAt :: GradientArray Pixel8 -> Float -> Pixel8 #-}
gradientColorAt grad at
    | at <= 0 = snd $ V.head grad
    | at >= 1.0 = snd $ V.last grad
    | otherwise = go (0, snd $ V.head grad) 0
  where
    !maxi = V.length grad
    go (prevCoeff, prevValue) ix
      | ix >= maxi = snd $ V.last grad
      | at < coeff = mixWith (\_ -> alphaOver cov icov) prevValue px
      | otherwise = go value $ ix + 1
      where value@(coeff, px) = grad `V.unsafeIndex` ix
            zeroToOne = (at - prevCoeff) / (coeff - prevCoeff)
            (cov, icov) = clampCoverage zeroToOne

gradientColorAtRepeat :: ModulablePixel px
                      => SamplerRepeat -> GradientArray px -> Float -> px
{-# SPECIALIZE INLINE
    gradientColorAtRepeat ::
        SamplerRepeat -> GradientArray PixelRGBA8 -> Float -> PixelRGBA8 #-}
{-# SPECIALIZE INLINE
    gradientColorAtRepeat ::
        SamplerRepeat -> GradientArray Pixel8 -> Float -> Pixel8 #-}
gradientColorAtRepeat SamplerPad grad = gradientColorAt grad
gradientColorAtRepeat SamplerRepeat grad =
    gradientColorAt grad . repeatGradient
gradientColorAtRepeat SamplerReflect grad =
    gradientColorAt grad . reflectGradient

linearGradientShader :: ModulablePixel px
                     => Gradient px -- ^ Gradient description.
                     -> Point       -- ^ Linear gradient start point.
                     -> Point       -- ^ Linear gradient end point.
                     -> SamplerRepeat
                     -> ShaderFunction px
{-# SPECIALIZE linearGradientShader
                     :: Gradient PixelRGBA8 -> Point -> Point -> SamplerRepeat
                     -> ShaderFunction PixelRGBA8 #-}
{-# SPECIALIZE linearGradientShader
                     :: Gradient Pixel8 -> Point -> Point -> SamplerRepeat
                     -> ShaderFunction Pixel8 #-}
linearGradientShader gradient start end repeating =
    \x y -> colorAt $ (V2 x y `dot` d) - s00
  where
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient
    vector = end ^-^ start
    d = vector ^/ (vector `dot` vector)
    s00 = start `dot` d

-- | Use another image as a texture for the filling.
-- Contrary to `imageTexture`, this function perform a bilinear
-- filtering on the texture.
--
sampledImageShader :: forall px.  ModulablePixel px
                   => Image px -> SamplerRepeat -> ShaderFunction px
{-# SPECIALIZE
     sampledImageShader :: Image Pixel8 -> SamplerRepeat
                        -> ShaderFunction Pixel8 #-}
{-# SPECIALIZE
     sampledImageShader :: Image PixelRGBA8 -> SamplerRepeat
                        -> ShaderFunction PixelRGBA8 #-}
sampledImageShader img sampling x y =
  (at px  py `interpX` at pxn py)
             `interpY`
  (at px pyn `interpX` at pxn pyn)
  where
   coordSampler SamplerPad maxi v =
      min (maxi - 1) . max 0 $ floor v
   coordSampler SamplerReflect maxi v =
      floor $ abs (abs (v - maxif - 1) `mod'` (2 * maxif) - maxif - 1)
        where maxif = fromIntegral maxi
   coordSampler SamplerRepeat maxi v = floor v `mod` maxi

   w = fromIntegral $ imageWidth img
   h = fromIntegral $ imageHeight img

   clampedX = coordSampler sampling w
   clampedY = coordSampler sampling h

   px = clampedX x
   pxn = clampedX $ x + 1
   py = clampedY y
   pyn = clampedY $ y + 1

   dx, dy :: Float
   !dx = x - fromIntegral (floor x :: Int)
   !dy = y - fromIntegral (floor y :: Int)

   at :: Int -> Int -> px
   at !xx !yy =
        unsafePixelAt rawData $ (yy * w + xx) * compCount

   (covX, icovX) = clampCoverage dx
   (covY, icovY) = clampCoverage dy

   interpX = mixWith (const $ alphaOver covX icovX)
   interpY = mixWith (const $ alphaOver covY icovY)

   compCount = componentCount (undefined :: px)
   rawData = imageData img

-- | Use another image as a texture for the filling.
-- This texture use the "nearest" filtering, AKA no
-- filtering at all.
imageShader :: forall px. (Pixel px) => Image px -> ShaderFunction px
{-# SPECIALIZE
    imageShader :: Image PixelRGBA8 -> ShaderFunction PixelRGBA8 #-}
{-# SPECIALIZE
    imageShader :: Image Pixel8 -> ShaderFunction Pixel8 #-}
imageShader img x y =
    unsafePixelAt rawData $ (clampedY * w + clampedX) * compCount
  where
   clampedX = min (w - 1) . max 0 $ floor x
   clampedY = min (h - 1) . max 0 $ floor y
   !compCount = componentCount (undefined :: px)
   !w = imageWidth img
   !h = imageHeight img
   !rawData = imageData img

radialGradientShader :: ModulablePixel px
                     => Gradient px -- ^ Gradient description
                     -> Point       -- ^ Radial gradient center
                     -> Float       -- ^ Radial gradient radius
                     -> SamplerRepeat
                     -> ShaderFunction px
{-# SPECIALIZE 
    radialGradientShader
       :: Gradient PixelRGBA8 -> Point -> Float -> SamplerRepeat
       -> ShaderFunction PixelRGBA8 #-}
{-# SPECIALIZE 
    radialGradientShader
       :: Gradient Pixel8 -> Point -> Float -> SamplerRepeat
       -> ShaderFunction Pixel8 #-}
radialGradientShader gradient center radius repeating =
    \x y -> colorAt $ norm (V2 x y ^-^ center) / radius
  where
    !colorAt = gradientColorAtRepeat repeating gradArray
    !gradArray = V.fromList gradient

radialGradientWithFocusShader
    :: ModulablePixel px
    => Gradient px -- ^ Gradient description
    -> Point      -- ^ Radial gradient center
    -> Float      -- ^ Radial gradient radius
    -> Point      -- ^ Radial gradient focus point
    -> SamplerRepeat
    -> ShaderFunction px
{-# SPECIALIZE
    radialGradientWithFocusShader
        :: Gradient PixelRGBA8 -> Point -> Float -> Point
        -> SamplerRepeat -> ShaderFunction PixelRGBA8 #-}
{-# SPECIALIZE
    radialGradientWithFocusShader
        :: Gradient Pixel8 -> Point -> Float -> Point
        -> SamplerRepeat -> ShaderFunction Pixel8 #-}
radialGradientWithFocusShader gradient center radius focusScreen repeating =
    \x y -> colorAt . go $ V2 x y ^-^ center
  where
    focus@(V2 origFocusX origFocusY) = focusScreen ^-^ center
    colorAt = gradientColorAtRepeat repeating gradArray
    gradArray = V.fromList gradient
    radiusSquared = radius * radius
    dist = sqrt $ focus `dot` focus
    clampedFocus@(V2 focusX focusY)
        | dist <= r = focus
        | otherwise = V2 (r * cos a) (r * sin a)
           where a = atan2 origFocusY origFocusX
                 r = radius * 0.99
    trivial = sqrt $ radiusSquared - origFocusX * origFocusX

    solutionOf (V2 x y) | x == focusX =
        V2 focusX (if y > focusY then trivial else negate trivial)
    solutionOf (V2 x y) = V2 xSolution $ slope * xSolution + yint
      where
        slope = (y - focusY) / (x - focusX)
        yint = y - (slope * x)

        a = slope * slope + 1
        b = 2 * slope * yint
        c = yint * yint - radiusSquared
        det = sqrt $ b * b - 4 * a * c
        xSolution = (-b + (if x < focusX then negate det else det)) / (2 * a)

    go pos = sqrt $ curToFocus / distSquared
      where
        solution = solutionOf pos ^-^ clampedFocus
        toFocus = pos ^-^ clampedFocus
        distSquared = solution `dot` solution
        curToFocus = toFocus `dot` toFocus

-- | Perform a multiplication operation between a full color texture
-- and a greyscale one, used for clip-path implementation.
modulateTexture :: ModulablePixel px
                => ShaderFunction px
                -> ShaderFunction (PixelBaseComponent px)
                -> ShaderFunction px
{-# INLINE modulateTexture #-}
modulateTexture fullTexture modulator x y =
    colorMap (modulate $ modulator x y) $ fullTexture x y

-- | Perform a multiplication operation between a full color texture
-- and a greyscale one, used for clip-path implementation.
alphaModulateTexture :: ModulablePixel px
                => ShaderFunction px
                -> ShaderFunction (PixelBaseComponent px)
                -> ShaderFunction px
{-# INLINE alphaModulateTexture #-}
alphaModulateTexture fullTexture modulator x y =
  let px = fullTexture x y in
  mixWithAlpha (\_ _ a -> a) (\_ _ -> modulator x y) px px

