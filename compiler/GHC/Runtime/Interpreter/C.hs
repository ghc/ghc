{-# LANGUAGE MultiWayIf #-}

-- | External interpreter program
module GHC.Runtime.Interpreter.C
  ( generateIservC
  )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Data.FastString
import GHC.Driver.Session
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Unit.Types
import GHC.Unit.Env
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Utils.Panic.Plain
import GHC.Linker.Executable
import GHC.Linker.Config

-- | Generate iserv program for the target
generateIservC :: DynFlags -> Logger -> TmpFs -> ExecutableLinkOpts -> UnitEnv -> IO FilePath
generateIservC dflags logger tmpfs opts unit_env = do
  -- get the unit-id of the ghci package. We need this to load the
  -- interpreter code.
  let unit_state = ue_homeUnitState unit_env
  ghci_unit_id <- case lookupPackageName unit_state (PackageName (fsLit "ghci")) of
    Nothing -> cmdLineErrorIO "C interpreter: couldn't find \"ghci\" package"
    Just i  -> pure i

  -- generate a temporary name for the iserv program
  let tmpdir = leTempDir opts
  exe_file <- newTempName logger tmpfs tmpdir TFL_GhcSession "iserv"

  let platform = ue_platform unit_env
  let os       = platformOS platform

  -- we inherit ExecutableLinkOpts for the target code (i.e. derived from
  -- DynFlags specified by the user and from settings). We need to adjust these
  -- options to generate the iserv program we want. Some settings are to be
  -- shared (e.g. ways, platform, etc.) but some other must be set specifically
  -- for iserv.
  let opts' = opts
        { -- write iserv program in some temporary directory
          leOutputFile = Just exe_file

          -- we need GHC to generate a main entry point...
        , leNoHsMain = False

          -- ...however the main symbol must be the iserv server
        , leMainSymbol = zString (zEncodeFS (unitIdFS ghci_unit_id)) ++ "_GHCiziServer_defaultServer"

          -- we need to reset inputs, otherwise one of them may be defining
          -- `main` too (with -no-hs-main).
        , leInputs = []

          -- we never know what symbols GHC will look up in the future, so we
          -- must retain CAFs for running interpreted code.
        , leKeepCafs = True

          -- link with -threaded if target has threaded RTS
        , leWays =
            let ways = leWays opts
                ways' = addWay WayThreaded ways
            in if targetHasRTSWays dflags ways' then ways' else ways

          -- enable all rts options
        , leRtsOptsEnabled = RtsOptsAll

          -- Add -Wl,--export-dynamic enables GHCi to load dynamic objects that
          -- refer to the RTS.  This is harmless if you don't use it (adds a bit
          -- of overhead to startup and increases the binary sizes) but if you
          -- need it there's no alternative.
          --
          -- The Solaris linker does not support --export-dynamic option. It also
          -- does not need it since it exports all dynamic symbols by default
        , leLinkerConfig = if
            | osElfTarget os
            , os /= OSFreeBSD
            , os /= OSSolaris2
            -> (leLinkerConfig opts)
                { linkerOptionsPost = linkerOptionsPost (leLinkerConfig opts) ++ [Option "-Wl,--export-dynamic"]
                }
            | otherwise
            -> leLinkerConfig opts
        }
  linkExecutable logger tmpfs opts' unit_env [] [ghci_unit_id]

  pure exe_file
