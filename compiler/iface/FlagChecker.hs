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

import Data.List (sort)

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
                sort $ map fromEnum $ extensionFlags)

        -- -I, -D and -U flags affect CPP
        cpp = (includePaths, sOpt_P settings)

        -- -i, -osuf, -hcsuf, -hisuf, -odir, -hidir, -stubdir, -o, -ohi
        paths = (importPaths,
                   [ objectSuf, hcSuf, hiSuf ],
                   [ objectDir, hiDir, stubDir, outputFile, outputHi ])

    in computeFingerprint nameio (mainis, safeHs, lang, cpp, paths)

