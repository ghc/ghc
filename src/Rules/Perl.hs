module Rules.Perl (perlScriptRules) where

import Base
import Builder
import Utilities

-- TODO: Do we need this build rule?
-- | Build Perl scripts, such as @ghc-split@, from their literate Perl sources.
perlScriptRules :: Rules ()
perlScriptRules = do
    "//*.prl" %> \out -> do
        let src = out -<.> "lprl"
        need [src]
        runBuilder Unlit [src, out]
