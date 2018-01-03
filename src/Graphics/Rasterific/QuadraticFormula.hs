module Graphics.Rasterific.QuadraticFormula( QuadraticFormula( .. )
                                           , discriminant
                                           , formulaRoots
                                           ) where

-- | Represent an equation `a * x^2 + b * x + c = 0`
data QuadraticFormula a = QuadraticFormula
    { _coeffA :: !a -- ^ Coefficient for the square part (x^2)
    , _coeffB :: !a -- ^ Coefficient the linear part (x)
    , _coeffC :: !a -- ^ Constant
    }

instance Functor QuadraticFormula where
    {-# INLINE fmap #-}
    fmap f (QuadraticFormula a b c) =
        QuadraticFormula (f a) (f b) (f c)

instance Applicative QuadraticFormula where
  pure a = QuadraticFormula a a a
  {-# INLINE pure #-}

  QuadraticFormula a b c <*> QuadraticFormula d e f =
      QuadraticFormula (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

-- | Discriminant equation, if the result is:
--
--  * Below 0, then the formula doesn't have any solutions
--
--  * Equal to 0, then the formula has a unique root.
--
--  * Above 0, the formula has two solutions
--
discriminant :: Num a => QuadraticFormula a -> a
discriminant (QuadraticFormula a b c) = b * b - 4 * a *c

-- | Extract all the roots of the formula ie. where the
-- unknown gives a result of 0
formulaRoots :: (Ord a, Floating a) => QuadraticFormula a -> [a]
formulaRoots formula@(QuadraticFormula a b _)
  | disc < 0 = []
  | disc == 0 = [positiveResult]
  | otherwise = [positiveResult, negativeResult]
  where
    disc = discriminant formula
    squarePart = sqrt disc
    positiveResult = (negate b + squarePart) / (2 * a)
    negativeResult = (negate b - squarePart) / (2 * a)

