module HsUtils where
import GHC.Hs.Binds
import SrcLoc

addPatSynSelector:: LHsBind p -> [a]
addPatSynSelector bind
  | PatSynBind _ _ <- unLoc bind
  = []
