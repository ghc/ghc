{-# LANGUAGE OverloadedRecordFields, DataKinds, PolyKinds, GADTs,
             StandaloneDeriving, TypeFamilies, UndecidableInstances,
             MagicHash #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records

type family Foo b
type instance Foo Int = Bool
type instance Foo Bool = Int

data W a = MkW { foo :: Foo a }

deriving instance Show (Foo a) => Show (W a)

data X b = MkX { bar :: W (Foo b) }

deriving instance Show (Foo (Foo a)) => Show (X a)

r :: W Int
r = MkW { foo = True }

-- Updates cannot change types, since the variables are not rigid
z :: X Bool
z = setField (proxy# :: Proxy# "bar") (MkX r) $
      setField (proxy# :: Proxy# "foo") r False

main = print z
