{-# LANGUAGE TypeFamilyDependencies, DataKinds, PolyKinds,
             UndecidableInstances #-}
module T6018failclosed where

-- Id is injective...
type family IdClosed a = result | result -> a where
    IdClosed a = a

-- ...but despite that we disallow a call to Id
type family IdProxyClosed (a :: *) = r | r -> a where
    IdProxyClosed a = IdClosed a

data N = Z | S N

-- PClosed is not injective, although the user declares otherwise. This
-- should be rejected on the grounds of calling a type family in the
-- RHS.
type family PClosed (a :: N) (b :: N) = (r :: N) | r -> a b where
    PClosed  Z    m = m
    PClosed (S n) m = S (PClosed n m)

-- this is not injective - not all injective type variables mentioned
-- on LHS are mentioned on RHS
type family JClosed a b c = r | r -> a b where
    JClosed Int b c = Char

-- this is not injective - not all injective type variables mentioned
-- on LHS are mentioned on RHS (tyvar is now nested inside a tycon)
type family KClosed (a :: N) (b :: N) = (r :: N) | r -> a b where
    KClosed (S n) m = S m

-- hiding a type family application behind a type synonym should be rejected
type MaybeSynClosed a = IdClosed a
type family LClosed a = r | r -> a where
    LClosed a = MaybeSynClosed a

type family FClosed a b c = (result :: *) | result -> a b c where
    FClosed Int  Char Bool = Bool
    FClosed Char Bool Int  = Int
    FClosed Bool Int  Char = Int

type family IClosed a b c = r | r -> a b where
    IClosed Int  Char Bool = Bool
    IClosed Int  Int  Int  = Bool
    IClosed Bool Int  Int  = Int

type family E2 (a :: Bool) = r | r -> a where
  E2 False = True
  E2 True  = False
  E2 a     = False

-- This exposed a subtle bug in the implementation during development. After
-- unifying the RHS of (1) and (2) the LHS substitution was done only in (2)
-- which made it look like an overlapped equation. This is not the case and this
-- definition should be rejected. The first two equations are here to make sure
-- that the internal implementation does list indexing corrcectly (this is a bit
-- tricky because the list is kept in reverse order).
type family F a b  = r | r -> a b where
  F Float  IO      = Float
  F Bool   IO      = Bool
  F a      IO      = IO a   -- (1)
  F Char   b       = b Int  -- (2)

-- This should fail because there is no way to determine a, b and k from the RHS
type family Gc (a :: k) (b :: k) = r | r -> k where
    Gc a b = Int
