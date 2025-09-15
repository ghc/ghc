{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module T24090a where

import GHC.Exts (Any)

type Foo :: a
type Foo = Any :: a
