module Rules.Install (installTargets, installRules) where

import Base
import Expression
import GHC
import Rules.Generate

installTargets :: [FilePath]
installTargets = [ "inplace/lib/template-hsc.h"
                 , "inplace/lib/platformConstants" ]

installRules :: Rules ()
installRules = do
    "inplace/lib/template-hsc.h"    <~ pkgPath hsc2hs
    "inplace/lib/platformConstants" <~ derivedConstantsPath
  where
    file <~ dir = file %> \out -> do
        let source = dir -/- takeFileName out
        copyFileChanged source out
        putSuccess $ "| Installed " ++ source ++ " -> " ++ out
