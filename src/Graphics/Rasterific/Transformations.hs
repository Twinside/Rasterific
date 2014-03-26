module Graphics.Rasterific.Transformations
    ( Transformation( .. )
    , applyTransformation
    , translate
    , scale
    , rotate
    , rotateCenter
    ) where

import Data.Monoid( Monoid( .. ), (<>) )
import Graphics.Rasterific.Types
import Linear( V2( .. ) )

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

applyTransformation :: Transformation -> Point -> Point
applyTransformation (Transformation a c e
                                    b d f) (V2 x y) =
    V2 (a * x + y * c + e) (b * x + d * y + f)

rotate :: Float -> Transformation
rotate angle = Transformation ca (-sa) 0
                              sa   ca  0
  where ca = cos angle
        sa = sin angle

rotateCenter :: Float -> Point -> Transformation
rotateCenter angle p =
    translate p <> rotate angle <> translate (negate p)

scale :: Float -> Float -> Transformation
scale scaleX scaleY =
    Transformation scaleX      0 0
                        0 scaleY 0

translate :: Vector -> Transformation
translate (V2 x y) =
    Transformation 1 0 x
                   0 1 y

