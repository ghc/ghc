module HsUtils where
import GHC.Hs.Binds
import GHC.Types.SrcLoc

addPatSynSelector:: GenLocated l (HsBindLR idL idR) -> [a]
addPatSynSelector bind
  | PatSynBind _ _ <- unLoc bind
  = []
