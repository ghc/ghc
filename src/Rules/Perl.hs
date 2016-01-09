module Rules.Perl (perlScriptRules) where

import Base
import Expression
import Rules.Actions (runBuilder)

-- TODO: get rid of perl scripts
-- | Generate perl scripts the build system requires, such as @ghc-split@,
-- from the corresponding literate perl source.
perlScriptRules :: Rules ()
perlScriptRules = do
    "//*.prl" %> \out -> do
        let src = out -<.> "lprl"
        runBuilder Unlit [src, out]
