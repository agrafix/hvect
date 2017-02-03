module  Main where

import qualified Data.HVectSpec

import Test.Hspec

main :: IO ()
main =
    hspec $
    Data.HVectSpec.spec
