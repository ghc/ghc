module Rules.Rts (rtsRules) where

import Hadrian.Utilities
import Settings.Builders.Common

-- | This rule has priority 3 to override the general rule for generating shared
-- library files (see Rules.Library.libraryRules).
rtsRules :: Rules ()
rtsRules = priority 3 $ do
    root <- buildRootRules
    -- Solve the recursive dependency between the rts and ghc-internal
    -- on Windows by creating an import lib for the ghc-internal dll,
    -- to be linked into the rts dll.
    forM_ [Stage1, Stage2, Stage3 ] $ \ stage -> do
        let buildPath = root -/- buildDir (rtsContext stage)
        buildPath -/- "libHSghc-internal-*.def" %> buildGhcInternalImportDef
        buildPath -/- "libHSghc-internal-*.dll.a" %> buildGhcInternalImportLib

buildGhcInternalImportDef :: FilePath -> Action ()
buildGhcInternalImportDef target = do
    templateIn <- readFile' "rts/win32/libHSghc-internal.def.in"
    let dllName = takeFileName target -<.> "dll"
        templateOut = replace "@GhcInternalDll@" dllName templateIn
    writeFile' target templateOut

buildGhcInternalImportLib :: FilePath -> Action ()
buildGhcInternalImportLib target = do
    let input = dropExtension (dropExtension target) <.> "def" -- the .def file
        output = target -- the .dll.a import lib
    need [input]
    runBuilder Dlltool ["-d", input, "-l", output] [input] [output]
