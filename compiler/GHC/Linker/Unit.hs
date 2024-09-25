
-- | Linking Haskell units
module GHC.Linker.Unit
   ( UnitLinkOpts (..)
   , collectLinkOpts
   , collectArchives
   , getUnitLinkOpts
   , getLibs
   )
where

import GHC.Prelude
import GHC.Platform.Ways
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Unit.Env
import GHC.Utils.Misc

import qualified GHC.Data.ShortText as ST

import GHC.Settings

import Control.Monad
import System.Directory
import System.FilePath

-- | Linker flags collected from units
data UnitLinkOpts = UnitLinkOpts
  { hsLibs     :: [String] -- ^ Haskell libraries (as a list of "-lHSfoo...")
  , extraLibs  :: [String] -- ^ External libraries (as a list of "-lfoo...")
  , otherFlags :: [String] -- ^ Extra linker options
  }
  deriving (Show)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getUnitLinkOpts :: GhcNameVersion -> Ways -> UnitEnv -> [UnitId] -> IO UnitLinkOpts
getUnitLinkOpts namever ways unit_env pkgs = do
    ps <- mayThrowUnitErr $ preloadUnitsInfo' unit_env pkgs
    return (collectLinkOpts namever ways ps)

collectLinkOpts :: GhcNameVersion -> Ways -> [UnitInfo] -> UnitLinkOpts
collectLinkOpts namever ways ps = UnitLinkOpts
  { hsLibs     = concatMap (map ("-l" ++) . unitHsLibs namever ways) ps
  , extraLibs  = concatMap (map ("-l" ++) . map ST.unpack . unitExtDepLibsSys) ps
  , otherFlags = concatMap (map ST.unpack . unitLinkerOptions) ps
  }

collectArchives :: GhcNameVersion -> Ways -> UnitInfo -> IO [FilePath]
collectArchives namever ways pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = ordNub . filter notNull . libraryDirsForWay ways $ pc
        libs        = unitHsLibs namever ways pc ++ map ST.unpack (unitExtDepLibsSys pc)

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: Ways -> UnitInfo -> [String]
libraryDirsForWay ws
  | hasWay ws WayDyn = map ST.unpack . unitLibraryDynDirs
  | otherwise        = map ST.unpack . unitLibraryDirs

getLibs :: GhcNameVersion -> Ways -> UnitEnv -> [UnitId] -> IO [(String,String)]
getLibs namever ways unit_env pkgs = do
  ps <- mayThrowUnitErr $ preloadUnitsInfo' unit_env pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryDirs ways [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> unitHsLibs namever ways p ]
    filterM (doesFileExist . fst) candidates

