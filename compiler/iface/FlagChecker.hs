{-# LANGUAGE RecordWildCards #-}

-- | This module manages storing the various GHC option flags in a modules
-- interface file as part of the recompilation checking infrastructure.
module FlagChecker (
        fingerprintDynFlags
    ) where

import Binary
import BinIface ()
import DynFlags
import HscTypes
import Module
import Name
import Fingerprint
-- import Outputable

import qualified Data.IntSet as IntSet
import System.FilePath (normalise)

-- | Produce a fingerprint of a @DynFlags@ value. We only base
-- the finger print on important fields in @DynFlags@ so that
-- the recompilation checker can use this fingerprint.
fingerprintDynFlags :: DynFlags -> Module -> (BinHandle -> Name -> IO ())
                    -> IO Fingerprint

fingerprintDynFlags dflags@DynFlags{..} this_mod nameio =
    let mainis   = if mainModIs == this_mod then Just mainFunIs else Nothing
                      -- see #5878
        -- pkgopts  = (thisPackage dflags, sort $ packageFlags dflags)
        safeHs   = setSafeMode safeHaskell
        -- oflags   = sort $ filter filterOFlags $ flags dflags

        -- *all* the extension flags and the language
        lang = (fmap fromEnum language,
                IntSet.toList $ extensionFlags)

        -- -I, -D and -U flags affect CPP
        cpp = (map normalise includePaths, opt_P dflags ++ picPOpts dflags)
            -- normalise: eliminate spurious differences due to "./foo" vs "foo"

        -- Note [path flags and recompilation]
        paths = [ hcSuf ]

        -- -fprof-auto etc.
        prof = if gopt Opt_SccProfilingOn dflags then fromEnum profAuto else 0

    in -- pprTrace "flags" (ppr (mainis, safeHs, lang, cpp, paths)) $
       computeFingerprint nameio (mainis, safeHs, lang, cpp, paths, prof)


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
