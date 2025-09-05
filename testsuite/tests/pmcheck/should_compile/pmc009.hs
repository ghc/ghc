module HsUtils where
import GHC.Hs.Binds
import Language.Haskell.Textual.Location

addPatSynSelector:: GenLocated l (HsBindLR idL idR) -> [a]
addPatSynSelector bind
  | PatSynBind _ _ <- unLoc bind
  = []
