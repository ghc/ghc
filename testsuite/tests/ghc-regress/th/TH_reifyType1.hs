-- test reification of monomorphic types

module TH_reifyType1
where

import Language.Haskell.THSyntax

foo :: Int -> Int
foo x = x + 1

type_foo :: TypQ
type_foo = reifyType foo

