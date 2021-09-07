module Test.QC (qcProps) where

import Test.Arbitrary()
import Test.Tasty.QuickCheck as QC
import Test.Tasty


qcProps = testGroup "quickcheck prop"
  [
    QC.testProperty "sort" $
    \list -> (list:: [Int]) /= list
  ]
