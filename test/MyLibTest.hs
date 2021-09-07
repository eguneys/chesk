module Main (main) where

import Test.QC
import Test.Tasty

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [qcProps]
