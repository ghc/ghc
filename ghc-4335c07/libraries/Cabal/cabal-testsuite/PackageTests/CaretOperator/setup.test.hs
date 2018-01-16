import Test.Cabal.Prelude

import Control.Monad
import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.Types.Dependency
import Distribution.PackageDescription
import Language.Haskell.Extension (Language(..))

-- Test that setup parses '^>=' operator correctly.
-- Don't bother with the cabal-install test as the build-depends
-- is updated by this point so that we lost the caret parsing.
main = setupTest $ do
    -- Don't run this for GHC 7.0/7.2, which doesn't have a recent
    -- enough version of pretty. (But this is pretty dumb.)
    skipUnless =<< ghcVersionIs (>= mkVersion [7,3])
    assertOutputDoesNotContain "Parse of field 'build-depends' failed"
        =<< setup' "configure" []
    lbi <- getLocalBuildInfoM

    let Just gotLib = library (localPkgDescr lbi)
        bi = libBuildInfo gotLib
    assertEqual "defaultLanguage" (Just Haskell2010) (defaultLanguage bi)
    forM_ (targetBuildDepends bi) $ \(Dependency pn vr) ->
        when (pn == mkPackageName "pretty") $
            assertEqual "targetBuildDepends/pretty"
                         vr (majorBoundVersion (mkVersion [1,1,1,0]))
    assertEqual "hsSourceDirs" ["."] (hsSourceDirs bi)
    return ()
