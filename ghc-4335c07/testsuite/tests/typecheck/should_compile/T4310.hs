{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables, KindSignatures #-}

module T4310 where
import GHC.ST

type family Mutable a :: * -> * -> *

data New v a = New (forall s. ST s (Mutable v s a))

create :: (forall s. ST s (Mutable v s a)) -> New v a
create = New
