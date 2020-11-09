
-- | Linking Haskell units
module GHC.Linker.Unit
   ( collectLinkOpts
   , collectArchives
   , collectLibraryPaths
   , getUnitLinkOpts
   , getUnitLibraryPath
   , getLibs
   , packageHsLibs
   )
where

import GHC.Prelude
import GHC.Platform.Ways
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import qualified GHC.Data.ShortText as ST

import GHC.Driver.Session

import qualified Data.Set as Set
import Data.List (isPrefixOf, stripPrefix)
import Control.Monad
import System.Directory
import System.FilePath

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getUnitLinkOpts :: DynFlags -> [UnitId] -> IO ([String], [String], [String])
getUnitLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadUnitsAnd
                                       (initSDocContext dflags defaultUserStyle)
                                       (unitState dflags)
                                       (mkHomeUnitFromFlags dflags)
                                       pkgs

collectLinkOpts :: DynFlags -> [UnitInfo] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . map ST.unpack . unitExtDepLibsSys) ps,
        concatMap (map ST.unpack . unitLinkerOptions) ps
    )

collectArchives :: DynFlags -> UnitInfo -> IO [FilePath]
collectArchives dflags pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = ordNub . filter notNull . libraryDirsForWay (ways dflags) $ pc
        libs        = packageHsLibs dflags pc ++ map ST.unpack (unitExtDepLibsSys pc)

collectLibraryPaths :: Ways -> [UnitInfo] -> [FilePath]
collectLibraryPaths ws = ordNub . filter notNull
                           . concatMap (libraryDirsForWay ws)

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: Ways -> UnitInfo -> [String]
libraryDirsForWay ws
  | WayDyn `elem` ws = map ST.unpack . unitLibraryDynDirs
  | otherwise        = map ST.unpack . unitLibraryDirs

getLibs :: DynFlags -> [UnitId] -> IO [(String,String)]
getLibs dflags pkgs = do
  ps <- getPreloadUnitsAnd
            (initSDocContext dflags defaultUserStyle)
            (unitState dflags)
            (mkHomeUnitFromFlags dflags)
            pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryPaths (ways dflags) [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> packageHsLibs dflags p ]
    filterM (doesFileExist . fst) candidates

-- | Find all the library paths in these and the preload packages
getUnitLibraryPath :: SDocContext -> UnitState -> HomeUnit -> Ways -> [UnitId] -> IO [String]
getUnitLibraryPath ctx unit_state home_unit ws pkgs =
  collectLibraryPaths ws `fmap` getPreloadUnitsAnd ctx unit_state home_unit pkgs

packageHsLibs :: DynFlags -> UnitInfo -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix . ST.unpack) (unitLibraries p)
  where
        ways0 = ways dflags

        ways1 = Set.filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug and profiled RTSs include support for -eventlog
        ways2 | WayDebug `Set.member` ways1 || WayProf `Set.member` ways1
              = Set.filter (/= WayTracing) ways1
              | otherwise
              = ways1

        tag     = waysTag (fullWays ways2)
        rts_tag = waysTag ways2

        mkDynName x
         | not (ways dflags `hasWay` WayDyn) = x
         | "HS" `isPrefixOf` x               =
              x ++ '-':programName dflags ++ projectVersion dflags
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ x)

        -- Add _thr and other rts suffixes to packages named
        -- `rts` or `rts-1.0`. Why both?  Traditionally the rts
        -- package is called `rts` only.  However the tooling
        -- usually expects a package name to have a version.
        -- As such we will gradually move towards the `rts-1.0`
        -- package name, at which point the `rts` package name
        -- will eventually be unused.
        --
        -- This change elevates the need to add custom hooks
        -- and handling specifically for the `rts` package for
        -- example in ghc-cabal.
        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix rts@"HSrts-1.0"= rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

