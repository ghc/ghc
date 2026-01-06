module Rules.Rts (rtsRules) where

import Hadrian.Utilities
import Settings.Builders.Common

-- | This rule has priority 3 to override the general rule for generating shared
-- library files (see Rules.Library.libraryRules).
rtsRules :: Rules ()
rtsRules = priority 3 $ do
    root <- buildRootRules
    -- An import lib for the ghc-internal dll, to be linked into the rts dll.
    forM_ [Stage1, Stage2, Stage3] $ \stage -> do
        let buildPath = root -/- buildDir (rtsContext stage)
        buildPath -/- "libHSghc-internal.dll.a" %> buildGhcInternalImportLib

-- Solve the recursive dependency between rts and ghc-internal on windows
-- with an import lib libHSghc-internal.dll.a, to be linked into the rts dll.
buildGhcInternalImportLib :: FilePath -> Action ()
buildGhcInternalImportLib target = do
    let input  = "rts/win32/libHSghc-internal.def"
        output = target -- the .dll.a import lib
    need [input]
    runBuilder Dlltool ["-d", input, "-l", output] [input] [output]
