module Rules.Copy (installTargets, copyRules) where

import Base
import Expression
import GHC
import Rules.Actions
import Rules.Generate

installTargets :: [FilePath]
installTargets = [ "inplace/lib/template-hsc.h"
                 , "inplace/lib/platformConstants"
                 , "inplace/lib/settings" ]

copyRules :: Rules ()
copyRules = do
    "inplace/lib/template-hsc.h"    <~ pkgPath hsc2hs
    "inplace/lib/platformConstants" <~ derivedConstantsPath
    "inplace/lib/settings"          <~ "."
  where
    file <~ dir = file %> \_ -> copyFile (dir -/- takeFileName file) file
