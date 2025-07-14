
module TH_Depends_Dir_External where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.Directory (listDirectory)

checkDirectoryContent :: Q Exp
checkDirectoryContent = do
  qAddDependentDirectory "TH_Depends_external"
  l <- qRunIO $ listDirectory "TH_Depends_external"
  let s = case l of
        [] -> "no files?"
        _  -> "yes files!"
  stringE s