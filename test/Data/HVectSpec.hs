{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-overlapping-patterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Data.HVectSpec (spec) where

import Data.HVect
import Test.Hspec
import qualified Data.HVect as HV

spec :: Spec
spec =
    do teqInstance
       tordInstance
       treverse
       thead
       ttail
       tnull
       tconcat
       tcurryUncurry
       tlength
       tidxAccess
       tgetFirst
       tallHave

teqInstance :: Spec
teqInstance =
    it "eqInstance" $
    do ("foo" :&: "bar" :&: empty == "foo" :&: "bar" :&: empty) `shouldBe` True
       ("foo" :&: True :&: empty == "foo" :&: True :&: empty) `shouldBe` True
       ("foo" :&: False :&: empty /= "foo" :&: True :&: empty) `shouldBe` True

tordInstance :: Spec
tordInstance =
    it "ordInstance" $
   do ((1 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 2 :&: 3 :&: empty))
          `shouldBe` ([1, 2, 3] `compare` [1, 2, 3])
      ((3 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 2 :&: 3 :&: empty))
          `shouldBe` ([3, 2, 3] `compare` [1, 2, 3])
      ((3 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 1 :&: 3 :&: empty))
          `shouldBe` ([1, 2, 3] `compare` [1, 1, 3])
      ((1 :&: 2 :&: 3 :&: empty) <= (1 :&: 2 :&: 3 :&: empty))
          `shouldBe` ([1, 2, 3] <= [1, 2, 3])
      ((3 :&: 2 :&: 3 :&: empty) <= (1 :&: 2 :&: 3 :&: empty))
          `shouldBe` ([3, 2, 3] <= [1, 2, 3])
      ((3 :&: 2 :&: 3 :&: empty) <= (1 :&: 1 :&: 3 :&: empty))
          `shouldBe` ([1, 2, 3] <= [1, 1, 3])
      ((1 :&: 2 :&: True :&: empty) `compare` (1 :&: 2 :&: True :&: empty))
          `shouldBe` EQ
      ((1 :&: "foo" :&: True :&: empty) `compare` (1 :&: "bar" :&: True :&: empty))
          `shouldBe` ("foo" `compare` "bar")

treverse :: Spec
treverse =
    it "reverse" $
    do (HV.reverse empty) `shouldBe` empty
       (HV.reverse $ 1 :&: 2 :&: empty) `shouldBe` (2 :&: 1 :&: empty)
       (HV.reverse $ 1 :&: "foo" :&: True :&: empty) `shouldBe` (True :&: "foo" :&: 1 :&: empty)

thead :: Spec
thead =
    it "head" $
    do (HV.head $ 1 :&: empty) `shouldBe` 1
       (HV.head $ 1 :&: 2 :&: empty) `shouldBe` 1

ttail :: Spec
ttail =
    it "tail" $
    do (HV.tail $ 1 :&: empty) `shouldBe` empty
       (HV.tail $ 1 :&: 2 :&: empty) `shouldBe` (2 :&: empty)

tnull :: Spec
tnull =
    it "null" $
    do (HV.null empty) `shouldBe` True
       (not $ HV.null $ 1 :&: empty) `shouldBe`  True

tconcat :: Spec
tconcat =
    it "concat" $
    do ((1 :&: 2 :&: empty) <++> ("foo" :&: "bar" :&: empty))
           `shouldBe` (1 :&: 2 :&: "foo" :&: "bar" :&: empty)
       ((1 :&: 2 :&: empty) <++> empty)
           `shouldBe` (1 :&: 2 :&: empty)

tcurryUncurry :: Spec
tcurryUncurry =
    it "curry uncurry" $
    do (fun (1 :&: 2 :&: empty)) `shouldBe` "12"
       (HV.curry fun 1 2) `shouldBe` "12"
       (HV.uncurry (HV.curry fun) (1 :&: 2 :&: empty)) `shouldBe` "12"
    where
      fun :: HVect [Int, Int] -> String
      fun (a :&: b  :&: HNil) = show a ++ show b
      fun _ = "OOPS!"

tlength :: Spec
tlength =
    it "length" $
    do (sNatToInt $ HV.length empty)  `shouldBe` 0
       (sNatToInt $ HV.length ("foo" :&: "bar" :&: empty)) `shouldBe` 2
       (sNatToInt $ HV.length ("aaa" :&: False :&: True :&: "foo" :&: "bar" :&: empty)) `shouldBe` 5

tidxAccess :: Spec
tidxAccess =
    it "idxAccess" $
    do (SZero HV.!! ("foo" :&: "bar" :&: empty))  `shouldBe` "foo"
       (SSucc SZero HV.!! ("foo" :&: "bar" :&: empty))  `shouldBe` "bar"
       (SSucc (SSucc SZero) HV.!! (True :&: "foo" :&: "bar" :&: empty)) `shouldBe` "bar"

tgetFirst :: Spec
tgetFirst =
    it "getFirst" $
    do (findFirst (intOne :&: True :&: False :&: "foo" :&: empty)) `shouldBe` True
       (findFirst (intOne :&: True :&: False :&: "foo" :&: empty) == intOne) `shouldBe` True
    where
      intOne :: Int
      intOne = 1

tallHave :: Spec
tallHave =
    it "allHave" $
    (showLocal $ 1 :&: 2 :&: True :&: empty) `shouldBe` ["1", "2", "True"]
    where
        showLocal :: AllHave Show ts => HVect ts -> [String]
        showLocal HNil = []
        showLocal (t :&: ts) = (show t : showLocal ts)
