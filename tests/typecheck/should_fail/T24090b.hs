{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

module T24090b where

import GHC.Exts (Any)

type Foo = Any :: a
