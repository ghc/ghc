-- test reification of type synonym declarations

module TH_reifyDecl2
where

import Language.Haskell.THSyntax

-- type declaration
type IntList = [Int]

decl_IntList :: DecQ
decl_IntList = reifyDecl IntList
