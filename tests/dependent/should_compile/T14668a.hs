{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T14668a where

data G (b :: ()) = G

class C a where
  type family F a

class (C a) => C' a where
  type family F' a (b :: F a)

data CInst

instance C CInst where
  type F CInst = ()

instance C' CInst where
  type F' CInst (b :: F CInst) = G b

