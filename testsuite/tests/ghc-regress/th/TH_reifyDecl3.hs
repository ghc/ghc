-- test reification of newtype declarations

module TH_reifyDecl3
where

import Language.Haskell.THSyntax

-- newtype declaration
newtype Length = Length Int

decl_Length :: DecQ
decl_Length = reifyDecl Length
