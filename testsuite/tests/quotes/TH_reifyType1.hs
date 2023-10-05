-- test reification of monomorphic types

module TH_reifyType1
where

import Language.Haskell.TH

foo :: Int -> Int
foo x = x + 1

type_foo :: InfoQ
type_foo = reify 'foo

