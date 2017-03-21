{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arbitrary( randomTests ) where

import Control.DeepSeq
import Test.QuickCheck
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary PathCommand where
    arbitrary = oneof
        [ PathLineTo <$> arbitrary
        , PathQuadraticBezierCurveTo <$> arbitrary <*> arbitrary
        , PathCubicBezierCurveTo <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Arbitrary Path where
    arbitrary = Path <$> arbitrary <*> pure True <*> arbitrary

instance Arbitrary SamplerRepeat where
    arbitrary = oneof $ map pure [toEnum 0 ..]

instance Arbitrary FillMethod where
    arbitrary = oneof $ map pure [toEnum 0 ..]

instance Arbitrary Join where
    arbitrary = oneof [pure JoinRound, JoinMiter <$> arbitrary]

instance Arbitrary Cap where
    arbitrary = oneof [pure CapRound, CapStraight <$> arbitrary]

newtype StrokeTest = StrokeTest (Drawing PixelRGBA8 ())

instance Show StrokeTest where
    show (StrokeTest sub) =
        "StrokeTest " ++ dumpDrawing sub

instance Arbitrary StrokeTest where
    arbitrary = StrokeTest <$>
        (stroke <$> (getPositive <$> arbitrary)
                <*> arbitrary
                <*> arbitrary
                <*> (pathToPrimitives <$> arbitrary))

newtype DashedStrokeTest = DashedStrokeTest (Drawing PixelRGBA8 ())

instance Show DashedStrokeTest where
    show (DashedStrokeTest sub) =
        "StrokeTest " ++ dumpDrawing sub


instance Arbitrary DashedStrokeTest where
    arbitrary = DashedStrokeTest <$>
        (dashedStroke <$> (fmap getPositive <$> arbitrary)
                      <*> (getPositive <$> arbitrary)
                      <*> arbitrary <*> arbitrary
                      <*> (pathToPrimitives <$> arbitrary))

backgroundColor :: PixelRGBA8
backgroundColor = PixelRGBA8 255 255 255 255

frontTexture :: Texture PixelRGBA8
frontTexture = uniformTexture $ PixelRGBA8 0 0x86 0xc1 255

fillTest :: Path -> Bool
fillTest path = deepseq img True
  where img = renderDrawing 200 200 backgroundColor $
                    withTexture frontTexture $
                        fill $ pathToPrimitives path

strokeTest :: StrokeTest -> Bool
strokeTest (StrokeTest test) = deepseq img True
  where img = renderDrawing 200 200 backgroundColor $
                    withTexture frontTexture test

dashedStrokeTest :: DashedStrokeTest -> Bool
dashedStrokeTest (DashedStrokeTest test) = deepseq img True
  where img = renderDrawing 200 200 backgroundColor $
                    withTexture frontTexture test

randomTests :: IO ()
randomTests = do
    quickCheck fillTest
    quickCheck strokeTest
    quickCheck dashedStrokeTest

