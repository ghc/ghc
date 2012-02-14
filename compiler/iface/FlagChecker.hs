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
import Name
import Fingerprint
-- import Outputable
import StaticFlags

import qualified Data.IntSet as IntSet
import System.FilePath (normalise)

-- | Produce a fingerprint of a @DynFlags@ value. We only base
-- the finger print on important fields in @DynFlags@ so that
-- the recompilation checker can use this fingerprint.
fingerprintDynFlags :: DynFlags -> (BinHandle -> Name -> IO ())
                    -> IO Fingerprint

fingerprintDynFlags DynFlags{..} nameio =
    let mainis   = (mainModIs, mainFunIs)
        -- pkgopts  = (thisPackage dflags, sort $ packageFlags dflags)
        safeHs   = setSafeMode safeHaskell
        -- oflags   = sort $ filter filterOFlags $ flags dflags

        -- *all* the extension flags and the language
        lang = (fmap fromEnum language,
                IntSet.toList $ extensionFlags)

        -- -I, -D and -U flags affect CPP
        cpp = (map normalise includePaths, sOpt_P settings)
            -- normalise: eliminate spurious differences due to "./foo" vs "foo"

        -- -i, -osuf, -hcsuf, -hisuf, -odir, -hidir, -stubdir, -o, -ohi
        paths = (map normalise importPaths,
                   [ objectSuf, hcSuf, hiSuf ],
                   [ objectDir, hiDir, stubDir, outputHi ])
                   -- NB. not outputFile, we don't want "ghc --make M -o <file>"
                   -- to force recompilation when <file> changes.

        -- -fprof-auto etc.
        prof = if opt_SccProfilingOn then fromEnum profAuto else 0

    in -- pprTrace "flags" (ppr (mainis, safeHs, lang, cpp, paths)) $
       computeFingerprint nameio (mainis, safeHs, lang, cpp, paths, prof)

