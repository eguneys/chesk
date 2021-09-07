module Test.Arbitrary where

import MyLib

import Control.Monad
import Test.Tasty.QuickCheck as QC

instance Arbitrary Pos where
  arbitrary = fmap toEnum $ choose(1, 8)

instance Arbitrary File where
  arbitrary = liftM File arbitrary

instance Arbitrary Rank where
  arbitrary = liftM Rank arbitrary

instance Arbitrary Coord where
  arbitrary = liftM2 Coord arbitrary arbitrary


coordList :: Int -> Gen [Coord]
coordList n = frequency [(1, return []),
                         (n, (:) <$> arbitrary <*> coordList (n-1))]
  

instance Arbitrary Vision where
  arbitrary = liftM3 Vision arbitrary arbitrary (coordList 6)
