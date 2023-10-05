-- Using overloaded labels to provide nice syntactic sugar for a
-- term representation using de Bruijn indices

{-# LANGUAGE OverloadedLabels
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , NoMonomorphismRestriction
           , OverlappingInstances
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeOperators
  #-}

import GHC.OverloadedLabels
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( Symbol )

instance x ~ y => IsLabel x (Proxy y) where
  fromLabel = Proxy

data Elem (x :: Symbol) g where
  Top :: Elem x (x ': g)
  Pop :: Elem x g -> Elem x (y ': g)
deriving instance Show (Elem x g)


class IsElem x g where
  which :: Elem x g

instance IsElem x (x ': g) where
  which = Top

instance IsElem x g => IsElem x (y ': g) where
  which = Pop which


data Tm g where
  Var :: Elem x g -> Tm g
  App :: Tm g -> Tm g -> Tm g
  Lam :: Tm (x ': g) -> Tm g
deriving instance Show (Tm g)

instance IsElem x g => IsLabel x (Tm g) where
  fromLabel = Var (which :: Elem x g)

lam :: Proxy x -> Tm (x ': g) -> Tm g
lam _ = Lam

s = lam #x #x
t = lam #x (lam #y (#x `App` #y))

u :: IsElem "z" g => Tm g
u = #z `App` #z

main = do print s
          print t
          print (u :: Tm '["z"])
