
module TH_Depends_External where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.Directory (listDirectory)
import Control.Monad.IO.Class (liftIO)

checkDirectoryContent :: Q Exp
checkDirectoryContent = do
  let externalDependency = "TH_Depends_external"
  qAddDependentDirectory externalDependency
  files <- liftIO $ listDirectory externalDependency

  let s = case files
    []  -> "no files?"
    _   -> "yes files!"
  stringE s