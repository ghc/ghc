{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T6018rnfail where

-- IA = injectivity annotation `| foo -> bar`

-- use incorrect tyvar in LHS of IA
type family F a = r | a -> a
type family Fc a = r | a -> a where
  Fc a = a
class Fcl a where
  type Ft a = r | a -> a

-- declare result tyvar to be duplicate (without IA)
type family G a = a
type family Gc a = a where
  Gc a = a

-- declare result tyvar to be duplicate (with IA)
type family Gb a = a | a -> a
type family Gcb a = a | a -> a where
  Gcb a = a
class Gclb a where -- here we want two errors
  type Gtb a = a | a -> a

-- not in-scope tyvar in RHS of IA
type family I a b = r | r -> c
type family Ic a b = r | r -> c where
  Ic a b = a
class Icl a b where
  type It a b = r | r -> c

-- not in-scope tyvar in LHS of IA
type family L a b = r | c -> a
type family Lc a b = r | c -> a where
  Lc a b = a
class Lcl a b where
  type Lt a b = r | c -> a

-- result variable shadows variable in class head
class M a b where
  type Mt b = a | a -> b

-- here b is out-of-scope
class N a b where
  type Nt a = r | r -> a b

-- result is out of scope. Not possible for associated types
type family O1  a | r -> a
type family Oc1 a | r -> a where
    Oc1 a = a
type family O2  a :: * | r -> a
type family Oc2 a :: * | r -> a where
    Oc2 a = a
