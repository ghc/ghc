-- test reification of polymorphic types

module TH_reifyType1
where

import Language.Haskell.THSyntax

type_length :: Type
type_length = reifyType length
