{-# LANGUAGE RecordWildCards #-}

-- | This module manages storing the various GHC option flags in a modules
-- interface file as part of the recompilation checking infrastructure.
module FlagChecker (
        fingerprintDynFlags
      , fingerprintOptFlags
      , fingerprintHpcFlags
    ) where

import GhcPrelude

import Binary
import BinIface ()
import DynFlags
import HscTypes
import Module
import Name
import Fingerprint
import BinFingerprint
-- import Outputable

import qualified EnumSet
import System.FilePath (normalise)

-- | Produce a fingerprint of a @DynFlags@ value. We only base
-- the finger print on important fields in @DynFlags@ so that
-- the recompilation checker can use this fingerprint.
--
-- NB: The 'Module' parameter is the 'Module' recorded by the
-- *interface* file, not the actual 'Module' according to our
-- 'DynFlags'.
fingerprintDynFlags :: DynFlags -> Module
                    -> (BinHandle -> Name -> IO ())
                    -> IO Fingerprint

fingerprintDynFlags dflags@DynFlags{..} this_mod nameio =
    let mainis   = if mainModIs == this_mod then Just mainFunIs else Nothing
                      -- see #5878
        -- pkgopts  = (thisPackage dflags, sort $ packageFlags dflags)
        safeHs   = setSafeMode safeHaskell
        -- oflags   = sort $ filter filterOFlags $ flags dflags

        -- *all* the extension flags and the language
        lang = (fmap fromEnum language,
                map fromEnum $ EnumSet.toList extensionFlags)

        -- -I, -D and -U flags affect CPP
        cpp = (map normalise includePaths, opt_P dflags ++ picPOpts dflags)
            -- normalise: eliminate spurious differences due to "./foo" vs "foo"

        -- Note [path flags and recompilation]
        paths = [ hcSuf ]

        -- -fprof-auto etc.
        prof = if gopt Opt_SccProfilingOn dflags then fromEnum profAuto else 0

        flags = (mainis, safeHs, lang, cpp, paths, prof)

    in -- pprTrace "flags" (ppr flags) $
       computeFingerprint nameio flags

-- Fingerprint the optimisation info. We keep this separate from the rest of
-- the flags because GHCi users (especially) may wish to ignore changes in
-- optimisation level or optimisation flags so as to use as many pre-existing
-- object files as they can.
-- See Note [Ignoring some flag changes]
fingerprintOptFlags :: DynFlags
                      -> (BinHandle -> Name -> IO ())
                      -> IO Fingerprint
fingerprintOptFlags DynFlags{..} nameio =
      let
        -- See https://ghc.haskell.org/trac/ghc/ticket/10923
        -- We used to fingerprint the optimisation level, but as Joachim
        -- Breitner pointed out in comment 9 on that ticket, it's better
        -- to ignore that and just look at the individual optimisation flags.
        opt_flags = map fromEnum $ filter (`EnumSet.member` optimisationFlags)
                                          (EnumSet.toList generalFlags)

      in computeFingerprint nameio opt_flags

-- Fingerprint the HPC info. We keep this separate from the rest of
-- the flags because GHCi users (especially) may wish to use an object
-- file compiled for HPC when not actually using HPC.
-- See Note [Ignoring some flag changes]
fingerprintHpcFlags :: DynFlags
                      -> (BinHandle -> Name -> IO ())
                      -> IO Fingerprint
fingerprintHpcFlags dflags@DynFlags{..} nameio =
      let
        -- -fhpc, see https://ghc.haskell.org/trac/ghc/ticket/11798
        -- hpcDir is output-only, so we should recompile if it changes
        hpc = if gopt Opt_Hpc dflags then Just hpcDir else Nothing

      in computeFingerprint nameio hpc


{- Note [path flags and recompilation]

There are several flags that we deliberately omit from the
recompilation check; here we explain why.

-osuf, -odir, -hisuf, -hidir
  If GHC decides that it does not need to recompile, then
  it must have found an up-to-date .hi file and .o file.
  There is no point recording these flags - the user must
  have passed the correct ones.  Indeed, the user may
  have compiled the source file in one-shot mode using
  -o to specify the .o file, and then loaded it in GHCi
  using -odir.

-stubdir
  We omit this one because it is automatically set by -outputdir, and
  we don't want changes in -outputdir to automatically trigger
  recompilation.  This could be wrong, but only in very rare cases.

-i (importPaths)
  For the same reason as -osuf etc. above: if GHC decides not to
  recompile, then it must have already checked all the .hi files on
  which the current module depends, so it must have found them
  successfully.  It is occasionally useful to be able to cd to a
  different directory and use -i flags to enable GHC to find the .hi
  files; we don't want this to force recompilation.

The only path-related flag left is -hcsuf.
-}

{- Note [Ignoring some flag changes]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Normally, --make tries to reuse only compilation products that are
the same as those that would have been produced compiling from
scratch. Sometimes, however, users would like to be more aggressive
about recompilation avoidance. This is particularly likely when
developing using GHCi (see #13604). Currently, we allow users to
ignore optimisation changes using -fignore-optim-changes, and to
ignore HPC option changes using -fignore-hpc-changes. If there's a
demand for it, we could also allow changes to -fprof-auto-* flags
(although we can't allow -prof flags to differ). The key thing about
these options is that we can still successfully link a library or
executable when some of its components differ in these ways.

The way we accomplish this is to leave the optimization and HPC
options out of the flag hash, hashing them separately.
-}
