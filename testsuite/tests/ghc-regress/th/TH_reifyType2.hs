-- test reification of polymorphic types

module TH_reifyType1
where

import Language.Haskell.THSyntax

type_length :: TypeQ
type_length = reifyType length
