{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for ReverseLoop type family
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.HVect
  ( HVect (..), (<:>)
  , empty, null, head
  , singleton
  , HVectElim
  , Append, (<++>)
  , ReverseLoop, Reverse, reverse
  , uncurry
  , Rep (..), HasRep (..)
  , curryExpl, curry
  , packExpl, pack
  ) where

import Prelude hiding (reverse, uncurry, curry, head, null)

-- | Heterogeneous vector
data HVect (ts :: [*]) where
  HNil :: HVect '[]
  HCons :: t -> HVect ts -> HVect (t ': ts)

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family HVectElim (ts :: [*]) (a :: *) :: *
type instance HVectElim '[] a = a
type instance HVectElim (t ': ts) a = t -> HVectElim ts a

-- todo: use a closed type family once GHC 7.6 compatibility is dropped
type family Append (as :: [*]) (bs :: [*]) :: [*]
type instance Append '[] bs = bs
type instance Append (a ': as) bs = a ': (Append as bs)

singleton :: a -> HVect '[a]
singleton el = HCons el HNil

empty :: HVect '[]
empty = HNil

null :: HVect as -> Bool
null HNil = True
null _ = False

head :: HVect (t ': ts) -> t
head (HCons a as) = a

(<:>) :: a -> HVect as -> HVect (a ': as)
(<:>) = HCons

infixr 5 <:>
infixr 5 <++>

(<++>) :: HVect as -> HVect bs -> HVect (Append as bs)
(<++>) HNil bs = bs
(<++>) (HCons a as) bs = HCons a (as <++> bs)

type family ReverseLoop (as :: [*]) (bs :: [*]) :: [*]
type instance ReverseLoop '[] bs = bs
type instance ReverseLoop (a ': as) bs = ReverseLoop as (a ': bs)

type Reverse as = ReverseLoop as '[]

reverse :: HVect as -> HVect (Reverse as)
reverse vs = go vs HNil
  where
    go :: HVect as -> HVect bs -> HVect (ReverseLoop as bs)
    go HNil bs = bs
    go (HCons a as) bs = go as (HCons a bs)

uncurry :: HVectElim ts a -> HVect ts -> a
uncurry f HNil = f
uncurry f (HCons x xs) = uncurry (f x) xs

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
curryExpl (RCons r) f = \x -> curryExpl r (f . HCons x)

curry :: HasRep ts => (HVect ts -> a) -> HVectElim ts a
curry = curryExpl hasRep

buildElim :: Rep ts -> (HVect ts -> HVect ss) -> HVectElim ts (HVect ss)
buildElim RNil f = f HNil
buildElim (RCons r) f = \x -> buildElim r (f . HCons x)

packExpl :: Rep ts -> (forall a. HVectElim ts a -> a) -> HVect ts
packExpl rep f = f (buildElim rep id)

pack :: HasRep ts => (forall a. HVectElim ts a -> a) -> HVect ts
pack = packExpl hasRep
