{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module T17301 where

import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

data A = MkA
data B (a :: A)

data TySing ty where
  SB :: TySing (B a)

data ATySing where
  MkATySing :: TySing ty -> ATySing

type family Forget ty :: ATySing where
  Forget (B a) = MkATySing SB

type family Message ty where
  Message (MkATySing (_ :: TySing ty)) =
    TypeError ( ShowType ty )



type KnownType = B MkA

foo :: Message (Forget KnownType) => ()
foo = ()

bar :: ()
bar = foo
