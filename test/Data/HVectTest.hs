{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Data.HVectTest (htf_thisModulesTests) where

import Test.Framework
import Data.HVect
import qualified Data.HVect as HV

test_eqInstance :: IO ()
test_eqInstance =
    do assertBool ("foo" :&: "bar" :&: empty == "foo" :&: "bar" :&: empty)
       assertBool ("foo" :&: True :&: empty == "foo" :&: True :&: empty)
       assertBool ("foo" :&: False :&: empty /= "foo" :&: True :&: empty)

test_ordInstance :: IO ()
test_ordInstance =
   do assertEqual ([1, 2, 3] `compare` [1, 2, 3]) ((1 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 2 :&: 3 :&: empty))
      assertEqual ([3, 2, 3] `compare` [1, 2, 3]) ((3 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 2 :&: 3 :&: empty))
      assertEqual ([1, 2, 3] `compare` [1, 1, 3]) ((3 :&: 2 :&: 3 :&: empty) `compare` (1 :&: 1 :&: 3 :&: empty))
      assertEqual ([1, 2, 3] <= [1, 2, 3]) ((1 :&: 2 :&: 3 :&: empty) <= (1 :&: 2 :&: 3 :&: empty))
      assertEqual ([3, 2, 3] <= [1, 2, 3]) ((3 :&: 2 :&: 3 :&: empty) <= (1 :&: 2 :&: 3 :&: empty))
      assertEqual ([1, 2, 3] <= [1, 1, 3]) ((3 :&: 2 :&: 3 :&: empty) <= (1 :&: 1 :&: 3 :&: empty))
      assertEqual EQ ((1 :&: 2 :&: True :&: empty) `compare` (1 :&: 2 :&: True :&: empty))
      assertEqual ("foo" `compare` "bar") ((1 :&: "foo" :&: True :&: empty) `compare` (1 :&: "bar" :&: True :&: empty))

test_reverse :: IO ()
test_reverse =
    do assertEqual empty (HV.reverse empty)
       assertEqual (2 :&: 1 :&: empty) (HV.reverse $ 1 :&: 2 :&: empty)
       assertEqual (True :&: "foo" :&: 1 :&: empty) (HV.reverse $ 1 :&: "foo" :&: True :&: empty)

test_head :: IO ()
test_head =
    do assertEqual 1 (HV.head $ 1 :&: empty)
       assertEqual 1 (HV.head $ 1 :&: 2 :&: empty)

test_tail :: IO ()
test_tail =
    do assertEqual empty (HV.tail $ 1 :&: empty)
       assertEqual (2 :&: empty) (HV.tail $ 1 :&: 2 :&: empty)

test_null :: IO ()
test_null =
    do assertBool (HV.null empty)
       assertBool (not $ HV.null $ 1 :&: empty)

test_concat :: IO ()
test_concat =
    do assertEqual (1 :&: 2 :&: "foo" :&: "bar" :&: empty) ((1 :&: 2 :&: empty) <++> ("foo" :&: "bar" :&: empty))
       assertEqual (1 :&: 2 :&: empty) ((1 :&: 2 :&: empty) <++> empty)


test_curryUncurry :: IO ()
test_curryUncurry =
    do assertEqual "12" (fun (1 :&: 2 :&: empty))
       assertEqual "12" (HV.curry fun 1 2)
       assertEqual "12" (HV.uncurry (HV.curry fun) (1 :&: 2 :&: empty))
    where
      fun :: HVect [Int, Int] -> String
      fun (a :&: b  :&: HNil) = show a ++ show b
      fun _ = "OOPS!"

test_length :: IO ()
test_length =
    do assertEqual 0 (sNatToInt $ HV.length empty)
       assertEqual 2 (sNatToInt $ HV.length ("foo" :&: "bar" :&: empty))
       assertEqual 5 (sNatToInt $ HV.length ("aaa" :&: False :&: True :&: "foo" :&: "bar" :&: empty))

test_idxAccess :: IO ()
test_idxAccess =
    do assertEqual "foo" (SZero HV.!! ("foo" :&: "bar" :&: empty))
       assertEqual "bar" (SSucc SZero HV.!! ("foo" :&: "bar" :&: empty))
       assertEqual "bar" (SSucc (SSucc SZero) HV.!! (True :&: "foo" :&: "bar" :&: empty))
