-- test reification of class declarations

module TH_reifyDecl4
where

import Language.Haskell.THSyntax

-- simple class
class C a where
  m :: a -> Int

decl_C :: Decl
decl_C = reifyDecl C
