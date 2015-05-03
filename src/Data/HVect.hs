{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for ReverseLoop type family
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.HVect
  ( -- * typesafe vector
    HVect (..)
  , empty, null, head
  , singleton
  , length, HVectLen (..)
  , (!!), HVectIdx (..)
  , HVectElim
  , Append, (<++>)
  , ReverseLoop, Reverse, reverse
  , uncurry
  , Rep (..), HasRep (..)
  , curryExpl, curry
  , packExpl, pack
    -- * type level numeric utilities
  , Nat (..), SNat (..), sNatToInt
  , intToSNat, AnySNat (..)
  , (:<)
  ) where

import Prelude hiding (reverse, uncurry, curry, head, null, (!!), length)

-- | Heterogeneous vector
data HVect (ts :: [*]) where
  HNil :: HVect '[]
  (:&:) :: t -> HVect ts -> HVect (t ': ts)

instance Eq (HVect '[]) where
    _ == _ =
        True

instance (Eq (HVect ts), Eq t) => Eq (HVect (t ': ts)) where
    a :&: as == b :&: bs =
        a == b && as == bs

instance Show (HVect '[]) where
    showsPrec d HNil =
        showParen (d > 10) $ showString "[]"

instance (Show (HVect ts), Show t) => Show (HVect (t ': ts)) where
    showsPrec d (a :&: as) =
        showParen (d > 5) $
           showsPrec 6 a .
           showString " <:> " .
           showsPrec 6 as

instance Ord (HVect '[]) where
    _ `compare` _ = EQ
    _ <= _ = True

instance (Ord (HVect ts), Ord t) => Ord (HVect (t ': ts)) where
    (a :&: as) `compare` (b :&: bs) =
        case a `compare` b of
          EQ -> as `compare` bs
          o -> o
    a :&: as <= b :&: bs =
        a <= b && as <= bs

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family HVectElim (ts :: [*]) (a :: *) :: *
type instance HVectElim '[] a = a
type instance HVectElim (t ': ts) a = t -> HVectElim ts a

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family Append (as :: [*]) (bs :: [*]) :: [*]
type instance Append '[] bs = bs
type instance Append (a ': as) bs = a ': (Append as bs)

singleton :: a -> HVect '[a]
singleton el = el :&: HNil

empty :: HVect '[]
empty = HNil

null :: HVect as -> Bool
null HNil = True
null _ = False

head :: HVect (t ': ts) -> t
head (a :&: as) = a

length :: HVect as -> SNat (HVectLen as)
length HNil = SZero
length (a :&: as) = SSucc (length as)

sNatToInt :: SNat n -> Int
sNatToInt SZero = 0
sNatToInt (SSucc n) = 1 + (sNatToInt n)

intToSNat :: Int -> AnySNat
intToSNat 0 = AnySNat SZero
intToSNat n =
    case intToSNat (n - 1) of
      AnySNat n -> AnySNat (SSucc n)

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

data AnySNat where
    AnySNat :: forall n. SNat n -> AnySNat

type family HVectLen (ts :: [*]) :: Nat
type instance HVectLen '[] = Zero
type instance HVectLen (t ': ts) = Succ (HVectLen ts)

type family HVectIdx (n :: Nat) (ts :: [*]) :: *
type instance HVectIdx Zero (a ': as) = a
type instance HVectIdx (Succ n) (a ': as) = HVectIdx n as
type instance HVectIdx a '[] = ()

type family (m :: Nat) :< (n :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< (Succ n) = True
type instance (Succ m) :< (Succ n) = m :< n

(!!) :: ((n :< HVectLen as) ~ True) => SNat n -> HVect as -> HVectIdx n as
SZero !! (a :&: as) = a
(SSucc s) !! (a :&: as) = s !! as

infixr 5 :&:
infixr 5 <++>
infixl 9 !!

(<++>) :: HVect as -> HVect bs -> HVect (Append as bs)
(<++>) HNil bs = bs
(<++>) (a :&: as) bs = a :&: (as <++> bs)

type family ReverseLoop (as :: [*]) (bs :: [*]) :: [*]
type instance ReverseLoop '[] bs = bs
type instance ReverseLoop (a ': as) bs = ReverseLoop as (a ': bs)

type Reverse as = ReverseLoop as '[]

reverse :: HVect as -> HVect (Reverse as)
reverse vs = go vs HNil
  where
    go :: HVect as -> HVect bs -> HVect (ReverseLoop as bs)
    go HNil bs = bs
    go (a :&: as) bs = go as (a :&: bs)

uncurry :: HVectElim ts a -> HVect ts -> a
uncurry f HNil = f
uncurry f (x :&: xs) = uncurry (f x) xs

data Rep (ts :: [*]) where
  RNil :: Rep '[]
  RCons :: Rep ts -> Rep (t ': ts)

class HasRep (ts :: [*]) where
  hasRep :: Rep ts

instance HasRep '[] where
  hasRep = RNil

instance HasRep ts => HasRep (t ': ts) where
  hasRep = RCons hasRep

curryExpl :: Rep ts -> (HVect ts -> a) -> HVectElim ts a
curryExpl RNil f = f HNil
curryExpl (RCons r) f = \x -> curryExpl r (f . (:&:) x)

curry :: HasRep ts => (HVect ts -> a) -> HVectElim ts a
curry = curryExpl hasRep

buildElim :: Rep ts -> (HVect ts -> HVect ss) -> HVectElim ts (HVect ss)
buildElim RNil f = f HNil
buildElim (RCons r) f = \x -> buildElim r (f . (:&:) x)

packExpl :: Rep ts -> (forall a. HVectElim ts a -> a) -> HVect ts
packExpl rep f = f (buildElim rep id)

pack :: HasRep ts => (forall a. HVectElim ts a -> a) -> HVect ts
pack = packExpl hasRep
