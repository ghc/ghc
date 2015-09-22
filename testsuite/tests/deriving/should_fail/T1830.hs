module T1830 where

import Language.Haskell.TH.Syntax (Lift)

data Foo a = Foo a deriving Lift
