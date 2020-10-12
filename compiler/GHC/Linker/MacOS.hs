module GHC.Linker.MacOS
   ( runInjectRPaths
   , getUnitFrameworks
   , getUnitFrameworkOpts
   , getUnitFrameworkPath
   , getFrameworkOpts
   , loadFramework
   )
where

import GHC.Prelude
import GHC.Platform

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Unit.Types
import GHC.Unit.State
import GHC.Unit.Home

import GHC.SysTools.Tasks

import GHC.Runtime.Interpreter (loadDLL)

import GHC.Utils.Outputable
import GHC.Utils.Exception
import GHC.Utils.Misc (ordNub )

import qualified GHC.Data.ShortText as ST

import Data.List
import Control.Monad (join, forM, filterM)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>), (<.>))

-- | On macOS we rely on the linkers @-dead_strip_dylibs@ flag to remove unused
-- libraries from the dynamic library.  We do this to reduce the number of load
-- commands that end up in the dylib, and has been limited to 32K (32768) since
-- macOS Sierra (10.14).
--
-- @-dead_strip_dylibs@ does not dead strip @-rpath@ entries, as such passing
-- @-l@ and @-rpath@ to the linker will result in the unnecesasry libraries not
-- being included in the load commands, however the @-rpath@ entries are all
-- forced to be included.  This can lead to 100s of @-rpath@ entries being
-- included when only a handful of libraries end up being truely linked.
--
-- Thus after building the library, we run a fixup phase where we inject the
-- @-rpath@ for each found library (in the given library search paths) into the
-- dynamic library through @-add_rpath@.
--
-- See Note [Dynamic linking on macOS]
runInjectRPaths :: DynFlags -> [FilePath] -> FilePath -> IO ()
runInjectRPaths dflags lib_paths dylib = do
  info <- lines <$> askOtool dflags Nothing [Option "-L", Option dylib]
  -- filter the output for only the libraries. And then drop the @rpath prefix.
  let libs = fmap (drop 7) $ filter (isPrefixOf "@rpath") $ fmap (head.words) $ info
  -- find any pre-existing LC_PATH items
  info <- fmap words.lines <$> askOtool dflags Nothing [Option "-l", Option dylib]
  let paths = concatMap f info
        where f ("path":p:_) = [p]
              f _            = []
      lib_paths' = [ p | p <- lib_paths, not (p `elem` paths) ]
  -- only find those rpaths, that aren't already in the library.
  rpaths <- nub . sort . join <$> forM libs (\f -> filterM (\l -> doesFileExist (l </> f)) lib_paths')
  -- inject the rpaths
  case rpaths of
    [] -> return ()
    _  -> runInstallNameTool dflags $ map Option $ "-add_rpath":(intersperse "-add_rpath" rpaths) ++ [dylib]

getUnitFrameworkOpts :: DynFlags -> Platform -> [UnitId] -> IO [String]
getUnitFrameworkOpts dflags platform dep_packages
  | platformUsesFrameworks platform = do
    pkg_framework_path_opts <- do
        pkg_framework_paths <- getUnitFrameworkPath
                                 (initSDocContext dflags defaultUserStyle)
                                 (unitState dflags)
                                 (mkHomeUnitFromFlags dflags)
                                 dep_packages
        return $ map ("-F" ++) pkg_framework_paths

    pkg_framework_opts <- do
        pkg_frameworks <- getUnitFrameworks
                              (initSDocContext dflags defaultUserStyle)
                              (unitState dflags)
                              (mkHomeUnitFromFlags dflags)
                              dep_packages
        return $ concat [ ["-framework", fw] | fw <- pkg_frameworks ]

    return (pkg_framework_path_opts ++ pkg_framework_opts)

  | otherwise = return []

getFrameworkOpts :: DynFlags -> Platform -> [String]
getFrameworkOpts dflags platform
  | platformUsesFrameworks platform = framework_path_opts ++ framework_opts
  | otherwise = []
  where
    framework_paths     = frameworkPaths dflags
    framework_path_opts = map ("-F" ++) framework_paths

    frameworks     = cmdlineFrameworks dflags
    -- reverse because they're added in reverse order from the cmd line:
    framework_opts = concat [ ["-framework", fw]
                            | fw <- reverse frameworks ]


-- | Find all the package framework paths in these and the preload packages
getUnitFrameworkPath :: SDocContext -> UnitState -> HomeUnit -> [UnitId] -> IO [String]
getUnitFrameworkPath ctx unit_state home_unit pkgs = do
  ps <- getPreloadUnitsAnd ctx unit_state home_unit pkgs
  return $ map ST.unpack (ordNub (filter (not . ST.null) (concatMap unitExtDepFrameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getUnitFrameworks :: SDocContext -> UnitState -> HomeUnit -> [UnitId] -> IO [String]
getUnitFrameworks ctx unit_state home_unit pkgs = do
  ps <- getPreloadUnitsAnd ctx unit_state home_unit pkgs
  return $ map ST.unpack (concatMap unitExtDepFrameworks ps)


{-
Note [macOS Big Sur dynamic libraries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

macOS Big Sur makes the following change to how frameworks are shipped
with the OS:

> New in macOS Big Sur 11 beta, the system ships with a built-in
> dynamic linker cache of all system-provided libraries.  As part of
> this change, copies of dynamic libraries are no longer present on
> the filesystem.  Code that attempts to check for dynamic library
> presence by looking for a file at a path or enumerating a directory
> will fail.  Instead, check for library presence by attempting to
> dlopen() the path, which will correctly check for the library in the
> cache. (62986286)

(https://developer.apple.com/documentation/macos-release-notes/macos-big-sur-11-beta-release-notes/)

Therefore, the previous method of checking whether a library exists
before attempting to load it makes GHC.Linker.MacOS.loadFramework
fail to find frameworks installed at /System/Library/Frameworks.
Instead, any attempt to load a framework at runtime, such as by
passing -framework OpenGL to runghc or running code loading such a
framework with GHCi, fails with a 'not found' message.

GHC.Linker.MacOS.loadFramework now opportunistically loads the
framework libraries without checking for their existence first,
failing only if all attempts to load a given framework from any of the
various possible locations fail.  See also #18446, which this change
addresses.
-}

-- Darwin / MacOS X only: load a framework
-- a framework is a dynamic library packaged inside a directory of the same
-- name. They are searched for in different paths than normal libraries.
loadFramework :: HscEnv -> [FilePath] -> FilePath -> IO (Maybe String)
loadFramework hsc_env extraPaths rootname
   = do { either_dir <- tryIO getHomeDirectory
        ; let homeFrameworkPath = case either_dir of
                                  Left _ -> []
                                  Right dir -> [dir </> "Library/Frameworks"]
              ps = extraPaths ++ homeFrameworkPath ++ defaultFrameworkPaths
        ; errs <- findLoadDLL ps []
        ; return $ fmap (intercalate ", ") errs
        }
   where
     fwk_file = rootname <.> "framework" </> rootname

     -- sorry for the hardcoded paths, I hope they won't change anytime soon:
     defaultFrameworkPaths = ["/Library/Frameworks", "/System/Library/Frameworks"]

     -- Try to call loadDLL for each candidate path.
     --
     -- See Note [macOS Big Sur dynamic libraries]
     findLoadDLL [] errs =
       -- Tried all our known library paths, but dlopen()
       -- has no built-in paths for frameworks: give up
       return $ Just errs
     findLoadDLL (p:ps) errs =
       do { dll <- loadDLL hsc_env (p </> fwk_file)
          ; case dll of
              Nothing  -> return Nothing
              Just err -> findLoadDLL ps ((p ++ ": " ++ err):errs)
          }
