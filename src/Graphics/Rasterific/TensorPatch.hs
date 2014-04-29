module Graphics.Rasterific.TensorPatch where

import Control.Applicative( (<$>) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.CubicBezier

data TensorPatchInternal = TensorPatchInternal
    { _tp0 :: !CubicBezier
    , _tp1 :: !CubicBezier
    , _tp2 :: !CubicBezier
    , _tp3 :: !CubicBezier

    , _w00 :: {-# UNPACK #-} !Float
    , _w01 :: {-# UNPACK #-} !Float
    , _w10 :: {-# UNPACK #-} !Float
    , _w11 :: {-# UNPACK #-} !Float
    }
    deriving (Eq, Show)

data TensorPatchSubdivision = TensorPatchSubdivision 
    { _subdivTopLeft     :: TensorPatchInternal
    , _subdivTopRight    :: TensorPatchInternal
    , _subdivBottomLeft  :: TensorPatchInternal
    , _subdivBottomRight :: TensorPatchInternal
    }
    deriving (Eq, Show)

mapSubdivision :: (TensorPatchInternal -> TensorPatchInternal) -> TensorPatchSubdivision
               -> TensorPatchSubdivision
mapSubdivision f (TensorPatchSubdivision a b c d) =
    TensorPatchSubdivision (f a) (f b) (f c) (f d)

toPrimitives :: TensorPatchInternal -> [Primitive]
toPrimitives
    (TensorPatchInternal
        topLine@(CubicBezier p00 _01 _02 p03)
                (CubicBezier p10   _   _ p13)
                (CubicBezier p20   _   _ p23)
     bottomLine@(CubicBezier p30 _31 _32 p33) _ _ _ _) =
    CubicBezierPrim <$> [
        topLine,
        (CubicBezier p03 p13 p23 p33),
        (CubicBezier p30 p20 p10 p00),
        bottomLine
    ]

transposePatch :: TensorPatchInternal -> TensorPatchInternal
transposePatch
    (TensorPatchInternal
        (CubicBezier p00 p01 p02 p03)
        (CubicBezier p10 p11 p12 p13)
        (CubicBezier p20 p21 p22 p23)
        (CubicBezier p30 p31 p32 p33)
        w00 w01 w10 w11) =

    (TensorPatchInternal
            (CubicBezier p00 p10 p20 p30)
            (CubicBezier p01 p11 p21 p31)
            (CubicBezier p02 p12 p22 p32)
            (CubicBezier p03 p13 p23 p33)
            w00 w10 w01 w11)

lerp :: Num a => a -> a -> a -> a
lerp = undefined

splitTensorPatchInUDirection :: TensorPatchInternal -> Float -> (TensorPatchInternal, TensorPatchInternal)
splitTensorPatchInUDirection
    (TensorPatchInternal c0 c1 c2 c3 w00 w01 w10 w11) at = (patchLeft, patchRight)
  where
    (l0, h0) = c0 `cubicBezierBreakAt` at
    (l1, h1) = c1 `cubicBezierBreakAt` at
    (l2, h2) = c2 `cubicBezierBreakAt` at
    (l3, h3) = c3 `cubicBezierBreakAt` at

    middleTop = lerp w00 w01 at
    middleBottom = lerp w10 w11 at

    patchLeft =
        TensorPatchInternal l0 l1 l2 l3
                    w00 middleTop
                    w10 middleBottom
    patchRight =
        TensorPatchInternal h0 h1 h2 h3
                    middleTop    w01
                    middleBottom w11

subdividePatch :: TensorPatchInternal -> Float -> Float -> TensorPatchSubdivision
subdividePatch patch atU atV = mapSubdivision transposePatch $ TensorPatchSubdivision 
    { _subdivTopLeft     = topLeft
    , _subdivTopRight    = topRight
    , _subdivBottomLeft  = bottomLeft
    , _subdivBottomRight = bottomRight
    }
  where
    (left, right) = patch `splitTensorPatchInUDirection` atU

    (topLeft, bottomLeft) =
        transposePatch left `splitTensorPatchInUDirection` atV
    (topRight, bottomRight) =
        transposePatch right `splitTensorPatchInUDirection` atV

