module E where

import Language.Haskell.TH.Syntax
import Dep
import DepPublic

e :: Q Exp
e = lift (5 :: Integer)

edep :: ()
edep = dep

edepPub :: ()
edepPub = depPub
