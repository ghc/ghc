module E where

import Language.Haskell.TH.Syntax

e :: QExp Int
e = lift (5 :: Integer)
