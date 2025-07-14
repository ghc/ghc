module GHC.Linker.Static
   ( linkStaticLib
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Settings

import GHC.SysTools
import GHC.SysTools.Ar

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Monad

import GHC.Linker.Unit
import GHC.Linker.Static.Utils

import GHC.Driver.Session

import System.FilePath
import System.Directory
import Control.Monad

-----------------------------------------------------------------------------
-- Static linking, of .o files

-- The list of packages passed to link is the list of packages on
-- which this program depends, as discovered by the compilation
-- manager.  It is combined with the list of packages that the user
-- specifies on the command line with -package flags.
--
-- In one-shot linking mode, we can't discover the package
-- dependencies (because we haven't actually done any compilation or
-- read any interface files), so the user must explicitly specify all
-- the packages.

-- | Linking a static lib will not really link anything. It will merely produce
-- a static archive of all dependent static libraries. The resulting library
-- will still need to be linked with any remaining link flags.
linkStaticLib :: Logger -> DynFlags -> UnitEnv -> [String] -> [UnitId] -> IO ()
linkStaticLib logger dflags unit_env o_files dep_units = do
  let platform  = ue_platform unit_env
      extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
      modules = o_files ++ extra_ld_inputs
      arch_os = platformArchOS platform
      output_fn = exeFileName arch_os True (outputFile_ dflags)
      namever = ghcNameVersion dflags
      ways_   = ways dflags

  full_output_fn <- if isAbsolute output_fn
                    then return output_fn
                    else do d <- getCurrentDirectory
                            return $ normalise (d </> output_fn)
  output_exists <- doesFileExist full_output_fn
  (when output_exists) $ removeFile full_output_fn

  pkg_cfgs_init <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)

  let pkg_cfgs
        | gopt Opt_LinkRts dflags
        = pkg_cfgs_init
        | otherwise
        = filter ((/= rtsUnitId) . unitId) pkg_cfgs_init

  archives <- concatMapM (collectArchives namever ways_) pkg_cfgs

  ar <- foldl mappend
        <$> (Archive <$> mapM loadObj modules)
        <*> mapM loadAr archives

  if toolSettings_ldIsGnuLd (toolSettings dflags)
    then writeGNUAr output_fn $ afilter (not . isGNUSymdef) ar
    else writeBSDAr output_fn $ afilter (not . isBSDSymdef) ar

  -- run ranlib over the archive. write*Ar does *not* create the symbol index.
  let ranlib_opts = configureRanlib dflags
  runRanlib logger ranlib_opts [GHC.SysTools.FileOption "" output_fn]
