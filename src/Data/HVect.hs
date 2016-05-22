{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for ReverseLoop type family
module Data.HVect
  ( -- * typesafe strict vector
    HVect (..)
  , empty, null, head, tail
  , singleton
  , length, HVectLen
  , findFirst, InList, ListContains, NotInList
  , MaybeToList
  , (!!), HVectIdx
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

import Prelude hiding (reverse, uncurry, curry, head, null, (!!), length, tail)

-- | Heterogeneous vector
data HVect (ts :: [*]) where
  HNil :: HVect '[]
  (:&:) :: !t -> !(HVect ts) -> HVect (t ': ts)

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

type family HVectElim (ts :: [*]) (a :: *) :: * where
    HVectElim '[] a = a
    HVectElim (t ': ts) a = t -> HVectElim ts a

type family Append (as :: [*]) (bs :: [*]) :: [*] where
    Append '[] bs = bs
    Append (a ': as) bs = a ': (Append as bs)

type family InList (x :: *) (xs :: [*]) :: Nat where
    InList x (x ': ys) = Zero
    InList x (y ': ys) = Succ (InList x ys)

class SNatRep n where
    getSNat :: SNat n

instance SNatRep Zero where
    getSNat = SZero

instance SNatRep n => SNatRep (Succ n) where
    getSNat = SSucc getSNat

type family NotInList (x :: *) (xs :: [*]) :: Bool where
    NotInList x (x ': ys) = False
    NotInList x (y ': ys) = NotInList x ys
    NotInList x '[] = True

type ListContains n x ts = (SNatRep n, InList x ts ~ n, HVectIdx n ts ~ x)

type family MaybeToList (a :: Maybe *) :: [*] where
    MaybeToList ('Just r) = '[r]
    MaybeToList 'Nothing = '[]

-- | Find first element in 'HVect' of type x
findFirst :: forall x ts n. (ListContains n x ts) => HVect ts -> x
findFirst vect = idx !! vect
    where
      idx :: SNat n
      idx = getSNat

singleton :: a -> HVect '[a]
singleton el = el :&: HNil

empty :: HVect '[]
empty = HNil

null :: HVect as -> Bool
null HNil = True
null _ = False

head :: HVect (t ': ts) -> t
head (a :&: _) = a

tail :: HVect (t ': ts) -> HVect ts
tail (_ :&: as) = as

length :: HVect as -> SNat (HVectLen as)
length HNil = SZero
length (_ :&: as) = SSucc (length as)

sNatToInt :: SNat n -> Int
sNatToInt SZero = 0
sNatToInt (SSucc n) = 1 + (sNatToInt n)

intToSNat :: Int -> AnySNat
intToSNat 0 = AnySNat SZero
intToSNat n =
    case intToSNat (n - 1) of
      AnySNat m -> AnySNat (SSucc m)

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

data AnySNat where
    AnySNat :: forall n. SNat n -> AnySNat

type family HVectLen (ts :: [*]) :: Nat where
    HVectLen '[] = Zero
    HVectLen (t ': ts) = Succ (HVectLen ts)

type family HVectIdx (n :: Nat) (ts :: [*]) :: * where
    HVectIdx Zero (a ': as) = a
    HVectIdx (Succ n) (a ': as) = HVectIdx n as

type family (m :: Nat) :< (n :: Nat) :: Bool where
    m :< Zero = False
    Zero :< (Succ n) = True
    (Succ m) :< (Succ n) = m :< n

type family (m :: Nat) :- (n :: Nat) :: Nat where
    n :- Zero = n
    (Succ m) :- (Succ n) = m :- n

(!!) :: SNat n -> HVect as -> HVectIdx n as
SZero !! (a :&: _) = a
(SSucc s) !! (_ :&: as) = s !! as
_ !! _ = error "HVect !!: This should never happen"

infixr 5 :&:
infixr 5 <++>
infixl 9 !!

(<++>) :: HVect as -> HVect bs -> HVect (Append as bs)
(<++>) HNil bs = bs
(<++>) (a :&: as) bs = a :&: (as <++> bs)

type family ReverseLoop (as :: [*]) (bs :: [*]) :: [*] where
    ReverseLoop '[] bs = bs
    ReverseLoop (a ': as) bs = ReverseLoop as (a ': bs)

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
