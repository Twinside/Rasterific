-- | This module provide some helpers in order
-- to perform basic geometric transformation on
-- the drawable primitives.
--
-- You can combine the transformation is `mappend` or
-- the `(\<\>)` operator from "Data.Monoid" .
module Graphics.Rasterific.Transformations
    ( Transformation( .. )
    , applyTransformation
    , applyVectorTransformation
    , translate
    , scale
    , rotate
    , rotateCenter
    , inverseTransformation
    ) where

import Data.Monoid( Monoid( .. ), (<>) )
import Graphics.Rasterific.Types
import Graphics.Rasterific.Linear( V2( .. ) )

-- | Represent a 3*3 matrix for homogenous coordinates.
--
-- > | A C E |
-- > | B D F |
-- > | 0 0 1 |
--
data Transformation = Transformation
    { _transformA :: {-# UNPACK #-} !Float
    , _transformC :: {-# UNPACK #-} !Float
    , _transformE :: {-# UNPACK #-} !Float -- ^ X translation

    , _transformB :: {-# UNPACK #-} !Float
    , _transformD :: {-# UNPACK #-} !Float
    , _transformF :: {-# UNPACK #-} !Float -- ^ Y translation
    }
    deriving (Eq, Show)

transformCombine :: Transformation -> Transformation -> Transformation
transformCombine (Transformation a c e
                                 b d f)

                 (Transformation a' c' e'
                                 b' d' f') =
    Transformation (a * a' + c * b' {- below b' is zero -})
              (a * c' + c * d' {- below d' is zero -})
              (a * e' + c * f' + e {- below f' is one -})

              (b * a' + d * b' {- below b' is zero -})
              (b * c' + d * d' {- below d' is zero -})
              (b * e' + d * f' + f {- below f' is one -})

instance Monoid Transformation where
    mappend = transformCombine
    mempty = Transformation 1 0 0
                            0 1 0

-- | Effectively transform a point given a transformation.
applyTransformation :: Transformation -> Point -> Point
applyTransformation (Transformation a c e
                                    b d f) (V2 x y) =
    V2 (a * x + y * c + e) (b * x + d * y + f)

-- | Effectively transform a vector given a transformation.
-- The translation part won't be applied.
applyVectorTransformation :: Transformation -> Vector -> Vector
applyVectorTransformation
    (Transformation a c _e
                    b d _f) (V2 x y) =
    V2 (a * x + y * c) (b * x + d * y)


-- | Create a transformation representing a rotation
-- on the plane.
--
-- > fill . transform (applyTransformation $ rotate 0.2)
-- >      $ rectangle (V2 40 40) 120 120
--
-- <<docimages/transform_rotate.png>>
--
rotate :: Float  -- ^ Rotation angle in radian.
       -> Transformation
rotate angle = Transformation ca (-sa) 0
                              sa   ca  0
  where ca = cos angle
        sa = sin angle

-- | Create a transformation representing a rotation
-- on the plane. The rotation center is given in parameter
--
-- > fill . transform (applyTransformation $ rotateCenter 0.2 (V2 200 200))
-- >      $ rectangle (V2 40 40) 120 120
--
-- <<docimages/transform_rotate_center.png>>
--
rotateCenter :: Float -- ^ Rotation angle in radian
             -> Point -- ^ Rotation center
             -> Transformation
rotateCenter angle p =
    translate p <> rotate angle <> translate (negate p)


-- | Perform a scaling of the given primitives.
--
-- > fill . transform (applyTransformation $ scale 2 2)
-- >      $ rectangle (V2 40 40) 40 40
--
-- <<docimages/transform_scale.png>>
--
scale :: Float -> Float -> Transformation
scale scaleX scaleY =
    Transformation scaleX      0 0
                        0 scaleY 0

-- | Perform a translation of the given primitives.
--
-- > fill . transform (applyTransformation $ translate (V2 100 100))
-- >      $ rectangle (V2 40 40) 40 40
--
-- <<docimages/transform_translate.png>>
--
translate :: Vector -> Transformation
translate (V2 x y) =
    Transformation 1 0 x
                   0 1 y

transformationDeterminant :: Transformation -> Float
transformationDeterminant (Transformation a c _e
                                          b d _f) = a * d - c * b

-- | Inverse a transformation (if possible)
inverseTransformation :: Transformation -> Maybe Transformation
inverseTransformation trans
    | transformationDeterminant trans == 0 = Nothing
inverseTransformation (Transformation a c e
                                      b d f) =
    Just $ Transformation a' c' e' b' d' f'
  where det = a * d - b * c
        a' = d / det
        c' = (- c) / det
        e' = (c * f - e * d) / det

        b' = (- b) / det
        d' = a / det
        f' = (e * b - a * f) / det

