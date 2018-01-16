import Distribution.PackageDescription ( PackageDescription )
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , postBuild
                           , postCopy
                           , postInst
                           )
import Distribution.Simple.InstallDirs ( mandir
                                       , CopyDest (NoCopyDest)
                                       )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..)
                                          , absoluteInstallDirs
                                          )
import Distribution.Simple.Utils ( installOrdinaryFiles
                                 , notice )
import Distribution.Simple.Setup ( buildVerbosity
                                 , copyDest
                                 , copyVerbosity
                                 , fromFlag
                                 , installVerbosity
                                 )
import Distribution.Verbosity ( Verbosity )

import System.IO ( openFile
                 , IOMode (WriteMode)
                 )
import System.Process ( runProcess )
import System.FilePath ( (</>) )

-- WARNING to editors of this file:
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- At this moment (Cabal 1.23), whatever you write here must be
-- compatible with ALL Cabal libraries which we support bootstrapping
-- with.  This is because pre-setup-depends versions of cabal-install will
-- build Setup.hs against the version of Cabal which MATCHES the library
-- that cabal-install was built against.  There is no way of overriding
-- this behavior without bumping the required 'cabal-version' in our
-- Cabal file.  Travis will let you know if we fail to install from
-- tarball!

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = \ _ flags _ lbi ->
      buildManpage lbi (fromFlag $ buildVerbosity flags)
  , postCopy = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags)
  , postInst = \ _ flags pkg lbi ->
      installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
  }

buildManpage :: LocalBuildInfo -> Verbosity -> IO ()
buildManpage lbi verbosity = do
  let cabal = buildDir lbi </> "cabal/cabal"
      manpage = buildDir lbi </> "cabal/cabal.1"
  manpageHandle <- openFile manpage WriteMode
  notice verbosity ("Generating manual page " ++ manpage ++ " ...")
  _ <- runProcess cabal ["manpage"] Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy = do
  let destDir = mandir (absoluteInstallDirs pkg lbi copy) </> "man1"
  installOrdinaryFiles verbosity destDir [(buildDir lbi </> "cabal", "cabal.1")]
