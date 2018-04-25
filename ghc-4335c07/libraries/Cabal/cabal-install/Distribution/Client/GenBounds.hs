{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.GenBounds
-- Copyright   :  (c) Doug Beardsley 2015
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The cabal gen-bounds command for generating PVP-compliant version bounds.
-----------------------------------------------------------------------------
module Distribution.Client.GenBounds (
    genBounds
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Init
         ( incVersion )
import Distribution.Client.Freeze
         ( getFreezePkgs )
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )
import Distribution.Client.Setup
         ( GlobalFlags(..), FreezeFlags(..), RepoContext )
import Distribution.Package
         ( Package(..), unPackageName, packageName, packageVersion )
import Distribution.PackageDescription
         ( buildDepends )
import Distribution.PackageDescription.Configuration
         ( finalizePD )
import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription )
import Distribution.Types.ComponentRequestedSpec
         ( defaultComponentRequestedSpec )
import Distribution.Types.Dependency
import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack, compilerInfo )
import Distribution.Simple.Program
         ( ProgramDb )
import Distribution.Simple.Utils
         ( tryFindPackageDesc )
import Distribution.System
         ( Platform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Version
         ( Version, alterVersion
         , LowerBound(..), UpperBound(..), VersionRange(..), asVersionIntervals
         , orLaterVersion, earlierVersion, intersectVersionRanges )
import System.Directory
         ( getCurrentDirectory )

-- | Does this version range have an upper bound?
hasUpperBound :: VersionRange -> Bool
hasUpperBound vr =
    case asVersionIntervals vr of
      [] -> False
      is -> if snd (last is) == NoUpperBound then False else True

-- | Given a version, return an API-compatible (according to PVP) version range.
--
-- Example: @0.4.1.2@ produces the version range @>= 0.4.1 && < 0.5@.
--
-- This version is slightly different than the one in
-- 'Distribution.Client.Init'.  This one uses a.b.c as the lower bound because
-- the user could be using a new function introduced in a.b.c which would make
-- ">= a.b" incorrect.
pvpize :: Version -> VersionRange
pvpize v = orLaterVersion (vn 3)
           `intersectVersionRanges`
           earlierVersion (incVersion 1 (vn 2))
  where
    vn n = alterVersion (take n) v

-- | Show the PVP-mandated version range for this package. The @padTo@ parameter
-- specifies the width of the package name column.
showBounds :: Package pkg => Int -> pkg -> String
showBounds padTo p = unwords $
    (padAfter padTo $ unPackageName $ packageName p) :
    map showInterval (asVersionIntervals $ pvpize $ packageVersion p)
  where
    padAfter :: Int -> String -> String
    padAfter n str = str ++ replicate (n - length str) ' '

    showInterval :: (LowerBound, UpperBound) -> String
    showInterval (LowerBound _ _, NoUpperBound) =
      error "Error: expected upper bound...this should never happen!"
    showInterval (LowerBound l _, UpperBound u _) =
      unwords [">=", display l, "&& <", display u]

-- | Entry point for the @gen-bounds@ command.
genBounds
    :: Verbosity
    -> PackageDBStack
    -> RepoContext
    -> Compiler
    -> Platform
    -> ProgramDb
    -> Maybe SandboxPackageInfo
    -> GlobalFlags
    -> FreezeFlags
    -> IO ()
genBounds verbosity packageDBs repoCtxt comp platform progdb mSandboxPkgInfo
      globalFlags freezeFlags = do

    let cinfo = compilerInfo comp

    cwd <- getCurrentDirectory
    path <- tryFindPackageDesc cwd
    gpd <- readGenericPackageDescription verbosity path
    -- NB: We don't enable tests or benchmarks, since often they
    -- don't really have useful bounds.
    let epd = finalizePD mempty defaultComponentRequestedSpec
                    (const True) platform cinfo [] gpd
    case epd of
      Left _ -> putStrLn "finalizePD failed"
      Right (pd,_) -> do
        let needBounds = filter (not . hasUpperBound . depVersion) $
                         buildDepends pd

        if (null needBounds)
          then putStrLn
               "Congratulations, all your dependencies have upper bounds!"
          else go needBounds
  where
     go needBounds = do
       pkgs  <- getFreezePkgs
                  verbosity packageDBs repoCtxt comp platform progdb
                  mSandboxPkgInfo globalFlags freezeFlags

       putStrLn boundsNeededMsg

       let isNeeded pkg = unPackageName (packageName pkg)
                          `elem` map depName needBounds
       let thePkgs = filter isNeeded pkgs

       let padTo = maximum $ map (length . unPackageName . packageName) pkgs
       traverse_ (putStrLn . (++",") . showBounds padTo) thePkgs

     depName :: Dependency -> String
     depName (Dependency pn _) = unPackageName pn

     depVersion :: Dependency -> VersionRange
     depVersion (Dependency _ vr) = vr

-- | The message printed when some dependencies are found to be lacking proper
-- PVP-mandated bounds.
boundsNeededMsg :: String
boundsNeededMsg = unlines
  [ ""
  , "The following packages need bounds and here is a suggested starting point."
  , "You can copy and paste this into the build-depends section in your .cabal"
  , "file and it should work (with the appropriate removal of commas)."
  , ""
  , "Note that version bounds are a statement that you've successfully built and"
  , "tested your package and expect it to work with any of the specified package"
  , "versions (PROVIDED that those packages continue to conform with the PVP)."
  , "Therefore, the version bounds generated here are the most conservative"
  , "based on the versions that you are currently building with.  If you know"
  , "your package will work with versions outside the ranges generated here,"
  , "feel free to widen them."
  , ""
  ]
