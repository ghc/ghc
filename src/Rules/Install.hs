module Rules.Install (installRules) where

import Expression
import GHC

installRules :: Rules ()
installRules = do
    "inplace/lib/template-hsc.h" %> \out -> do
        let source = pkgPath hsc2hs -/- "template-hsc.h"
        putBuild $ "| Copying " ++ source ++ " -> " ++ out
        copyFileChanged source out
