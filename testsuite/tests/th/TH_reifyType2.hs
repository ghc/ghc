-- test reification of polymorphic types

module TH_reifyType1
where

import Language.Haskell.TH

type_length :: InfoQ
type_length = reify 'length
