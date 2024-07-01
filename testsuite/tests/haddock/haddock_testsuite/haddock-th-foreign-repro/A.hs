module A where

import Language.Haskell.TH
import F


foo :: Exp
foo = LitE (StringL "foo")
