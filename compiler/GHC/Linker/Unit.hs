
-- | Linking Haskell units
module GHC.Linker.Unit
   ( UnitLinkOpts (..)
   , collectLinkOpts
   , collectArchives
   , getUnitLinkOpts
   , getLibs
   )
where

import GHC.Driver.DynFlags
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
import Data.Semigroup ( Semigroup(..) )
import System.Directory
import System.FilePath

-- | Linker flags collected from units
data UnitLinkOpts = UnitLinkOpts
  { hsLibs     :: [String] -- ^ Haskell libraries (as a list of "-lHSfoo...")
  , extraLibs  :: [String] -- ^ External libraries (as a list of "-lfoo...")
  , otherFlags :: [String] -- ^ Extra linker options
  }
  deriving (Show)

instance Semigroup UnitLinkOpts where
  (UnitLinkOpts l1 el1 of1) <> (UnitLinkOpts l2 el2 of2) = (UnitLinkOpts (l1 <> l2) (el1 <> el2) (of1 <> of2))

instance Monoid UnitLinkOpts where
  mempty = UnitLinkOpts [] [] []

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getUnitLinkOpts :: GhcNameVersion -> Ways -> Maybe (ExecutableLinkMode, Bool) -> UnitEnv -> [UnitId] -> IO UnitLinkOpts
getUnitLinkOpts namever ways mExecutableLinkMode unit_env pkgs = do
    ps <- mayThrowUnitErr $ preloadUnitsInfo' unit_env pkgs
    collectLinkOpts namever ways mExecutableLinkMode ps

collectLinkOpts :: GhcNameVersion -> Ways -> Maybe (ExecutableLinkMode, Bool) -> [UnitInfo] -> IO UnitLinkOpts
collectLinkOpts namever ways mExecutableLinkMode ps = do
  fmap mconcat $ forM ps $ \pc -> do
    extraLibs <- getExtraLibs pc
    pure UnitLinkOpts
      { hsLibs     = map ("-l" ++) . unitHsLibs namever ways $ pc
      , extraLibs  = extraLibs
      , otherFlags = map ST.unpack . unitLinkerOptions $ pc
      }
 where
  -- extra libs can be represented in different ways, depending on the platform and how we link:
  --   * static linking on most system: -l:libfoo.a -l:libbar.a
  --   * static linking on e.g. mac: /some/path/libfoo.a /some/path/libbar.a
  --   * dynamic linking: -lfoo -lbar
  getExtraLibs pc
    -- We don't do anything here for 'FullyStatic', because appending '-static' to the linker is enough.
    | Just (MostlyStatic, True) <- mExecutableLinkMode
    = pure . map (\d -> "-l:lib" ++ d ++ ".a") . map ST.unpack . unitExtDepLibsStaticSys $ pc
    | Just (MostlyStatic, False) <- mExecutableLinkMode
    = do
        fmap mconcat $ forM (map ST.unpack . unitExtDepLibsStaticSys $ pc) $ \l ->
          filterM doesFileExist
            [ searchPath </> ("lib" ++ l ++ ".a")
            | searchPath <- (ordNub . filter notNull . map ST.unpack . unitLibraryDirsStatic $ pc)
            ]

    | Just (FullyStatic, _) <- mExecutableLinkMode
    = pure . map ("-l" ++) . map ST.unpack . unitExtDepLibsStaticSys $ pc
    | otherwise = pure . map ("-l" ++) . map ST.unpack . unitExtDepLibsSys $ pc

collectArchives :: GhcNameVersion -> Ways -> UnitInfo -> IO [FilePath]
collectArchives namever ways pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = ordNub . filter notNull . libraryDirsForWay ways $ pc
        libs        = unitHsLibs namever ways pc ++ (map ST.unpack . unitExtDepLibsStaticSys $ pc)

getLibs :: GhcNameVersion -> Ways -> UnitEnv -> [UnitId] -> IO [(String,String)]
getLibs namever ways unit_env pkgs = do
  ps <- mayThrowUnitErr $ preloadUnitsInfo' unit_env pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryDirs ways [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> unitHsLibs namever ways p ]
    filterM (doesFileExist . fst) candidates

