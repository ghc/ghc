module Rules.Perl (perlScriptRules) where

import Base
import Expression
import Rules.Actions (runBuilder)
import Rules.Generate (generateExec, emptyTarget)
import Rules.Generators.GhcSplit (generateGhcSplit)

-- | Generate scripts the build system requires. For now we generate the
-- @ghc-split@ script from it's literate perl source.
perlScriptRules :: Rules ()
perlScriptRules = do
    -- how to translate literate perl to perl.
    -- this is a hack :-/
    "//*.prl" %> \out -> do
        let src = out -<.> "lprl"
        runBuilder Unlit [src, out]

    -- ghc-split is only a perl script.
    let ghcSplit = "inplace/lib/bin/ghc-split"

    ghcSplit <~ generateGhcSplit

    where
        file <~ gen = file %> \out -> generateExec out emptyTarget gen
