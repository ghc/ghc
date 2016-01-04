module Rules.Copy (installTargets, copyRules) where

import Base
import Expression
import GHC
import Rules.Actions
import Rules.Generate
import Rules.Libffi
import Settings.TargetDirectory

installTargets :: [FilePath]
installTargets = [ "inplace/lib/template-hsc.h"
                 , "inplace/lib/platformConstants"
                 , "inplace/lib/settings" ]

copyRules :: Rules ()
copyRules = do
    targetPath Stage1 rts -/- "build/ffi*.h" %> \ffih -> do
        need [libffiLibrary]
        ffiHPaths <- getDirectoryFiles "" ["libffi/build/inst/lib/*/include/ffi.h"]
        when (length ffiHPaths /= 1) $
            putError $ "copyRules: exactly one ffi.h header expected"
                     ++ "(found: " ++ show ffiHPaths ++ ")."
        
        copyFile (takeDirectory (head ffiHPaths) -/- takeFileName ffih) ffih
        copyFile libffiLibrary (targetPath Stage1 rts -/- "build" -/- "libCffi.a")

    "inplace/lib/template-hsc.h"    <~ pkgPath hsc2hs
    "inplace/lib/platformConstants" <~ derivedConstantsPath
    "inplace/lib/settings"          <~ "."
  where
    file <~ dir = file %> \_ -> copyFile (dir -/- takeFileName file) file
