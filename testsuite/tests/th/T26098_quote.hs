{-# LANGUAGE ExplicitLevelImports, DataKinds, TemplateHaskell #-}
module T26098_quote where

import splice T26098A_quote
import T26098A_quote
import Language.Haskell.TH

c :: Q Type
c = [t|T|]

d :: Q Type
d = [t|'MkT|]
