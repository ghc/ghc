-- test reification of declarations

module TH_reifyDecl1
where

import Language.Haskell.THSyntax

data T = A | B

decl_T :: Decl
decl_T = reifyDecl T
