{-# LANGUAGE OverloadedRecordFields, DataKinds, KindSignatures,
             ExistentialQuantification, RankNTypes, TypeFamilies,
             MagicHash #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-unused-binds #-}

import GHC.Prim (proxy#, Proxy#)
import GHC.Records
import OverloadedRecFldsRun01_A as I (U(MkU, x), V(..), Unused(unused))

data S = MkS { x :: Int }
  deriving Show

data T = MkT { x :: Bool, y :: Bool -> Bool, tField :: Bool }

-- Updates to `x` may change only the type of `c`
data W a b c d = MkW { x :: (a, b, c), y :: a, z :: d }
               | MkW2 { x :: (a, b, c), foo :: b }
  deriving Show

-- Only the `okay` field generates Has/Upd instances
data X a = forall e . MkX { existential :: (Int, e)
                          , universal   :: (forall b . b) -> ()
                          , x           :: a }

-- We can have data families too, provided a single data family
-- doesn't overload the same field name
data family F (a :: *) (b :: *) :: * -> *
data instance F Int b Int = MkF { foo :: Int } | MkF' { foo :: Int }
data instance F Int b Bool = MkF2 { bar :: Bool }


s = MkS 42
t = MkT True id False
w = MkW { x = (True, True, True), y = True, z = True }

-- Resolving ambiguous monomorphic updates
a = t { x = False, y = not, tField = True } -- only T has all these fields
b = s { x = 3 } :: S         -- type being pushed in
c = (t :: T) { x = False }   -- type signature on record expression

-- Specialised getter and setter
get_x :: r { x :: a } => r -> a
get_x r = x r

set_x :: Upd r "x" a => r -> a -> UpdTy r "x" a
set_x   = setField (proxy# :: Proxy# "x")

-- Type-changing update is possible in places
d = set_x w (False, False, 'x')
e = setField (proxy# :: Proxy# "z") d 42

f :: Int
f = x (set_x (MkX {x = True}) 42)

g = foo (MkF 3)
h = bar (MkF2 True)

main = do  print (x s)
           print (x (MkT False id True))
           print (y t (x t))
           print (x (MkU True False))
           print (x (MkV 3))
           print (get_x a)
           print b
           print (get_x c)
           print d
           print e
           print f
           print g
           print h
