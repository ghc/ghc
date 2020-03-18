module HsUtils where
import GHC.Hs.Binds
import GHC.Types.SrcLoc

addPatSynSelector:: LHsBind p -> [a]
addPatSynSelector bind
  | PatSynBind _ _ <- unLoc bind
  = []
