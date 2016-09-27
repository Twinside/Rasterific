{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
#if !MIN_VERSION_base(4,8,0)
import Data.Foldable( foldMap )
import Control.Applicative( (<$>) )
#endif

import Control.Monad.Trans.State.Strict
import Control.Monad( when, forM_ )
import Control.Monad.ST( ST, runST )
import Data.Monoid( (<>) )
import Graphics.Rasterific hiding ( fill
                                  , dashedStrokeWithOffset
                                  , dashedStroke
                                  , fillWithMethod, stroke)
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear( (^+^), (^-^), (^*) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Patch
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific

import Graphics.Text.TrueType( loadFontFile )
import Codec.Picture

import qualified Data.Vector as V

type Stroker g =
  (Geometry g) =>
      Float -> Join -> (Cap, Cap) -> g -> Drawing PixelRGBA8 ()

sansSerifFont :: FilePath
sansSerifFont = "test_fonts/DejaVuSans.ttf"

monospaceFont :: FilePath
monospaceFont =  "test_fonts/DejaVuSansMono.ttf"

background, blue, black, yellow, red, green, orange, white :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255
red = PixelRGBA8 255 0 0 255
green =  PixelRGBA8 0 255 0 255
black = PixelRGBA8 0 0 0 255
{-grey = PixelRGBA8 128 128 128 255-}
orange = PixelRGBA8 255 0xA5 0 255
yellow = PixelRGBA8 255 255 0 255
{-brightblue = PixelRGBA8 0 255 255 255-}
white = PixelRGBA8 255 255 255 255

biColor, triColor :: Gradient PixelRGBA8
biColor = [ (0.0, black) , (1.0, yellow) ]
triColor = [ (0.0, blue), (0.5, white) , (1.0, red) ]

frontColor, accentColor, accent2Color :: PixelRGBA8
frontColor = PixelRGBA8 0 0x86 0xc1 255
accentColor = PixelRGBA8 0xff 0xf4 0xc1 255
accent2Color = PixelRGBA8 0xFF 0x53 0x73 255


drawImm :: FilePath -> Int -> Int -> (forall s. DrawContext (ST s) PixelRGBA8 ()) -> IO ()
drawImm path w h d = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white d

drawPatchDebug :: FilePath -> Int -> Int -> CoonPatch PixelRGBA8 -> IO ()
drawPatchDebug path w h p = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white $ do
    renderCoonPatch p
    mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ debugDrawCoonPatch defaultDebug p

drawTensorDebug :: DebugOption -> FilePath -> Int -> Int -> TensorPatch PixelRGBA8 -> IO ()
drawTensorDebug opt path w h p = do
  putStrLn $ "Rendering " ++ path
  writePng path $ runST $ runDrawContext w h white $ do
    renderTensorPatch p
    mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ debugDrawTensorPatch opt p

drawPure :: FilePath -> Int -> Int -> Drawing PixelRGBA8 () -> IO ()
drawPure path w h act = do
  putStrLn $ "Rendering " ++ path
  writePng path $ renderDrawing w h white $ 
    withTexture (uniformTexture black) act

coonTest :: IO ()
coonTest = do
  drawing "coon_img/single_patch.png" defaultDebug 400 440 patch [patch]
  drawing "coon_img/single_patch_subdiv.png" defaultDebug 400 410 patch [n, e, w, s]
  where
    draw path p = do
      putStrLn $ "Rendering " ++ path
      writePng path . renderDrawing 800 800 (PixelRGBA8 255 255 255 255) $
          withTexture (uniformTexture (PixelRGBA8 0 0 0 255)) p

    drawing path opt w h rootPatch patches = do
      putStrLn $ "Rendering " ++ path
      writePng path $ runST $ runDrawContext w h white $ do
        renderCoonPatch rootPatch
        forM_ patches $ \p ->
          mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $
            debugDrawCoonPatch defaultDebug p { _coonValues = colors }

    Subdivided n e w s = subdividePatch patch { _coonValues = parametricBase }

    colors = ParametricValues
      { _northValue = frontColor
      , _eastValue = accentColor
      , _southValue = accent2Color
      , _westValue = frontColor
      }

    patch = transformCoon (\p -> p ^* 0.5 ^+^ V2 0 20) CoonPatch
      { _north = CubicBezier (V2 52 67) (V2 198 (-55)) (V2 580 104) (V2 713 113)
      , _east = CubicBezier (V2 713 113) (V2 775 369) (V2 437 392) (V2 670 674)
      , _south = CubicBezier (V2 670 674) (V2 471 690) (V2 294 762) (V2 72 722)
      , _west = CubicBezier (V2 73 722) (V2 9 490) (V2 56 284) (V2 52 67)
      , _coonValues = colors
      }

coonTensorTest :: IO ()
coonTensorTest = do
  drawImm "coon_img/compare_tensor.png" 400 400 $ renderTensorPatch $ tensorPatch
  drawTensorDebug opt "coon_img/compare_tensor_debug.png" 400 400 tensorPatch
  drawImm "coon_img/compare_coon.png" 400 400 $ renderCoonPatch $ coonPatch
  where
    opt = defaultDebug { _drawOutline = False }
    [ c00, c01, c02, c03
      , c10, c11, c12, c13
      , c20, c21, c22, c23
      , c30, c31, c32, c33
      ] = fmap (\p -> (p ^+^ (V2 0 (-852.36))) * 2)
        {-
          [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
          ,(V2 13.98 923.5), (V2 90 950),     (V2 117 950),      (V2 193.9 944.6)
          ,(V2 2.253 974.9), (V2 90 1000),    (V2 117 1000),     (V2 109.2 950.4)
          ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
          ] -- -} 
       -- {- 
          [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
          ,(V2 13.98 923.5), (V2 140 990),     (V2 147 1000),      (V2 193.9 944.6)
          ,(V2 2.253 974.9), (V2 140 1000),    (V2 147 1005),     (V2 109.2 950.4)
          ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
          ] -- -}

    coonPatch = CoonPatch
        { _north = CubicBezier c00 c01 c02 c03
        , _east  = CubicBezier c03 c13 c23 c33
        , _south = CubicBezier c33 c32 c31 c30
        , _west  = CubicBezier c30 c20 c10 c00
        , _coonValues = colors
        }

    tensorPatch = TensorPatch
      { _curve0 = CubicBezier c00 c01 c02 c03
      , _curve1 = CubicBezier c10 c11 c12 c13
      , _curve2 = CubicBezier c20 c21 c22 c23
      , _curve3 = CubicBezier c30 c31 c32 c33
      , _tensorValues = colors
      }

    colors = ParametricValues blue red red red

tensorSplit :: IO ()
tensorSplit = do
  drawImm "coon_img/split_tensor_orig.png" 400 400 $ renderTensorPatch $ tensorPatch
  drawTensorDebug defaultDebug "coon_img/split_tensor_orig_debug.png" 400 400 tensorPatch
  drawTensorSubdivDebug "coon_img/split_tensor_orig_subH.png" 400 400 tensorPatch [patchWest, patchEast]
  drawTensorSubdivDebug "coon_img/split_tensor_orig_subHVR.png" 400 400 tensorPatch [patchWest, patchNorthEast, patchSouthEast]
  where
    drawTensorSubdivDebug path w h p ps = do
        putStrLn $ "Rendering " ++ path
        writePng path $ runST $ runDrawContext w h white $ do
            renderTensorPatch p
            mapM_ fillOrder $ drawOrdersOfDrawing w h 96 white $ mapM_ (debugDrawTensorPatch opt) ps

    opt = defaultDebug
    [ c00, c01, c02, c03
     , c10, c11, c12, c13
     , c20, c21, c22, c23
     , c30, c31, c32, c33
     ] = fmap (\p -> (p ^+^ (V2 30 (-802.36))) ^* 1.5)
        [(V2 13.21 869.2), (V2 49.67 838.5), (V2 145.1 878.4), (V2 178.2 880.7)
        ,(V2 13.98 923.5), (V2 120 950),     (V2 147 950),      (V2 193.9 944.6)
        ,(V2 2.253 974.9), (V2 120 1000),    (V2 147 1000),     (V2 220.2 950.4)
        ,(V2 18.21  1033), (V2 73.48 1043), (V2 117.7 1025),   (V2 167.5 1021)
        ]

    tensorPatch = TensorPatch
      { _curve0 = CubicBezier c00 c01 c02 c03
      , _curve1 = CubicBezier c10 c11 c12 c13
      , _curve2 = CubicBezier c20 c21 c22 c23
      , _curve3 = CubicBezier c30 c31 c32 c33
      , _tensorValues = colors
      }

    (patchWest, patchEast) = horizontalTensorSubdivide tensorPatch { _tensorValues = parametricBase }
    (patchNorthEast, patchSouthEast) = horizontalTensorSubdivide $ transposePatch patchEast
    colors = ParametricValues frontColor accentColor accent2Color frontColor

coonTestColorStop :: IO ()
coonTestColorStop = do
  drawImm "coon_render_color.png" 800 800 $ renderCoonPatch patch
  drawPatchDebug "coon_render_color_debug.png" 800 800 patch
  where
    cc a b c d e f = PathCubicBezierCurveTo (V2 a b) (V2 c d) (V2 e f)
    [CubicBezierPrim c1, CubicBezierPrim c2, CubicBezierPrim c3, CubicBezierPrim c4] =
        toPrimitives . transform (\p -> (p ^+^ (V2 0 (-852.36))) * 4) $ Path (V2 13.21 869.2) False
          [cc 49.67 838.5 145.1 878.4 178.2 (880.7 :: Float)
          ,cc 193.9 944.6 109.2 950.4 167.5 1021
          ,cc 117.7 1025 73.48 1043 18.21 1033
          ,cc 2.253 974.9 13.98 923.5 13.21 869.2
          ]
    patch = CoonPatch c1 c2 c3 c4 
              (ParametricValues (PixelRGBA8 255 20 0 255)
                          red
                          red
                          red)

toCoon :: V2 Float -> ParametricValues px -> [[V2 Float]] -> CoonPatch px
toCoon st values = build . go st where
  build [n, e, s, w] = CoonPatch n e s w values
  build _ = error "toCoon"

  go _ [] = []
  go p [lst] = case toAbsolute p lst of
    [c1, c2] -> [CubicBezier p c1 c2 st]
    _ -> error "Mouh"
  go p (x : xs) = case toAbsolute p x of
    [c1, c2, c3] -> CubicBezier p c1 c2 c3 : go c3 xs
    _ -> error "Mouh"

  toAbsolute p = fmap (p ^+^)

drawVertex :: Point -> Drawing PixelRGBA8 ()
drawVertex p = stroke 2 JoinRound (CapRound, CapRound) $ circle p 4

drawBetweenPoint :: Point -> Point -> Drawing PixelRGBA8 ()
drawBetweenPoint p1 p2 =
  stroke 1.5 JoinRound (CapRound, CapRound) $ line p1 p2

jitPoints :: Transformable a => Float -> a -> a
jitPoints force e = evalState (transformM jit e) jitter where
  jitter = cycle $ (^* force) <$> [ V2 1 0.5, V2 0.5 1 , V2 (-0.5) (-1)
                                  , V2 (-1) (-0.5), V2 2 (-1.5), V2 (-1) 1]
  jit p = do
    v:rest <- get
    put rest
    return $ p ^+^ v

renderCoonMeshBicubic :: MeshPatch PixelRGBA8 -> DrawContext (ST s) PixelRGBA8 ()
renderCoonMeshBicubic =
  mapM_ renderCoonPatch . cubicCoonPatchesOf . calculateMeshColorDerivative

grid :: IO ()
grid = do
    drawPure "coon_img/grid.png" 460 300 $ drawing simpleGrid limesh
    drawPure "coon_img/grid_bez.png" 460 300 $ drawing simpleGrid moved
    drawPure "coon_img/grid_bez_color.png" 460 300 $ drawing simpleColor moved
    drawImm  "coon_img/gradient_mesh_first.png" 460 300 $ renderCoonMesh moved
    drawImm  "coon_img/gradient_mesh_first_bicubic.png" 460 300 $ renderCoonMeshBicubic moved
  where
    height = 3
    width = 4
    colorConstants = [frontColor, accentColor, frontColor, accent2Color, accentColor]
    colors = V.fromListN ((width + 1) * (height + 1))
           $ cycle $ colorConstants ++ tail colorConstants
    limesh = generateLinearGrid width height (V2 30 40) (V2 90 70) colors
    moved = jitPoints 12 limesh

    varPatchIndices = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

    simpleGrid = defaultDebug
        { _drawControlMesh = False
        , _drawBaseVertices = False
        , _drawControVertices = False
        }

    simpleColor = simpleGrid
        { _colorVertices    = True
        , _drawBaseVertices = True
        , _drawControVertices = False
        }

    drawing opt mesh =
      forM_ varPatchIndices $ \(x, y) -> do
          debugDrawCoonPatch opt $ coonPatchAt mesh x y

debugCubic :: IO ()
debugCubic = do
    drawImm "coon_img/gradient_mesh_bilinear_debug.png" 500 500 $ renderCoonMesh mesh
    drawImm "coon_img/gradient_mesh_bicubic_debug.png" 500 500 $ renderCoonMeshBicubic mesh
  where
    px r g b = PixelRGBA8 r g b 255
    mesh =
      generateLinearGrid 4 4 (V2 10 10) (V2 100 100) colors
  
    colors = V.fromListN (5 * 5)
      [ px 205 255 41
      , px 255 196 0
      , px 255 103 0
      , px 255 103 0
      , px 0   176 255
  
      , px 241 7   0
      , px 127 0   0
      , px 0   76  255
      , px 41  255 205
      , px 255 196 0
  
      , px 0   76  255
      , px 255 103 0
      , px 0   76  255
      , px 124 255 121
      , px 255 103 0
  
      , px 0   76  255
      , px 241 7   0
      , px 0   176 255
      , px 255 103 0
      , px 0   76  255
  
      , px 241 7   0
      , px 0   0   241
      , px 241 7   0
      , px 41  255 205
      , px 41  255 205
      ]

coonTestWild :: IO ()
coonTestWild = do
  drawImm "coon_render_wild.png" 800 800 $ renderCoonPatch patch
  drawPatchDebug "coon_render_wild_debug.png" 800 800 patch
  where
    patch = toCoon (V2 50 130 ^* 2)
        (ParametricValues red yellow orange green) $
        fmap (^* 2) <$>
        [ [V2 150  0, V2 300 (-100), V2 120 (-100)]
        , [V2 0  100, V2  40   200 , V2  40  220]
        , [V2 (-250) (-100), V2 50 60, V2 (-160) 0]
        , [V2 (-20) (-80), V2 20 (-40)]
        ]

main :: IO ()
main = do
  grid
  debugCubic 
  coonTest
  coonTestColorStop 
  coonTestWild 
  coonTensorTest
  tensorSplit

