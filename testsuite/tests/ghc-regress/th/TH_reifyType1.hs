-- test reification of monomorphic types

module TH_reifyType1
where

import Language.Haskell.THSyntax

foo :: Int -> Int
foo x = x + 1

type_foo :: TypeQ
type_foo = reifyType foo

