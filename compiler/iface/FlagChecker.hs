-- {-# LANGUAGE StandaloneDeriving #-}

-- -----------------------------------------------------------------------------
-- | This module manages storing the various GHC option flags in a modules
-- interface file as part of the recompilation checking infrastructure.
--
module FlagChecker (
        fingerprintDynFlags
    ) where

import Binary
import BinIface ()
import DynFlags
import HscTypes
import Name
import Fingerprint

-- import Data.List (sort)

{-
Note [DynFlags Hash]
~~~~~~~~~~~~~~~~~~~
We only hash fields from DynFlags that are of high importance as they stop
link and build errors occurring (e.g the '--main-is' flag, see trac #437).

An alternative design would be to return two fingerprints where the first one
encodes flags that if different mean that the module itself should be
recompiled but only this module. The second fingerprint is for flags that mean
that not only the module itself should be recompiled but also modules that
depend on it. (i.e the second fingerprint affects the modules ABI).

This design hasn't been implemented as it's tricky to fit in with the current
recompilation manager and its not sure how beneficial it is.
-}

-- | Produce a fingerprint of a @DynFlags@ value. We only base
-- the finger print on important fields in @DynFlags@ so that
-- the recompilation checker can use this fingerprint.
fingerprintDynFlags :: DynFlags -> (BinHandle -> Name -> IO ()) -> IO Fingerprint
fingerprintDynFlags dflags nameio =
    let -- DriverPipeline.getLinkInfo handles this info I believe
        -- rtsopts = (rtsOptsEnabled dflags, rtsOpts dflags)

        -- Probably not a good idea
        -- optlvl  = optLevel dflags

        mainis   = (mainModIs dflags, mainFunIs dflags)
        -- pkgopts  = (thisPackage dflags, sort $ packageFlags dflags)
        safeHs   = setSafeMode $ safeHaskell dflags
        -- oflags   = sort $ filter filterOFlags $ flags dflags
        -- eflags   = sort $ filter filterEFlags $ extensionFlags dflags

        flagOpts = (mainis, safeHs)
    in computeFingerprint nameio flagOpts

{-
-- | Should the @DynFlag@ be included in the fingerprint?
filterOFlags :: DynFlag -> Bool
filterOFlags Opt_EnableRewriteRules     = True
filterOFlags Opt_Vectorise              = True
filterOFlags Opt_IgnoreInterfacePragmas = True
filterOFlags Opt_OmitInterfacePragmas   = True
filterOFlags Opt_ExposeAllUnfoldings    = True
filterOFlags Opt_ReadUserPackageConf    = True
filterOFlags Opt_NoHsMain               = True
filterOFlags Opt_SSE2                   = True
filterOFlags Opt_SSE4_2                 = True
filterOFlags Opt_PackageTrust           = True
filterOFlags _                          = False

-- | Should the @ExtensionFlag@ be included in the fingerprint?
filterEFlags :: ExtensionFlag -> Bool
filterEFlags Opt_ExtendedDefaultRules = True
filterEFlags Opt_InterruptibleFFI     = True
filterEFlags Opt_ImplicitParams       = True
filterEFlags Opt_ImplicitPrelude      = True
filterEFlags Opt_OverloadedStrings    = True
filterEFlags Opt_RebindableSyntax     = True
filterEFlags _                        = False
-}

-- -----------------------------------------------------------------------------
-- Instances needed for Binary
-- -----------------------------------------------------------------------------

{-
deriving instance Ord DynFlag
deriving instance Enum DynFlag

deriving instance Ord ExtensionFlag
deriving instance Enum ExtensionFlag

-- NOTE: We're converting from int to byte8 here, so be careful if we
-- ever get more DynFlag or ExtensionFlag constructors than 256.
instance Binary DynFlag where
    put_ bh = (putByte bh . fromIntegral . fromEnum)
    get  bh = getByte bh >>= (return . toEnum . fromIntegral)

instance Binary ExtensionFlag where
    put_ bh = (putByte bh . fromIntegral . fromEnum)
    get  bh = getByte bh >>= (return . toEnum . fromIntegral)

-- | RtsOptsEnabled Binary Instance
instance Binary RtsOptsEnabled where
    put_ bh rtsopts =
        case rtsopts of
            RtsOptsNone     -> putByte bh 0
            RtsOptsSafeOnly -> putByte bh 1
            RtsOptsAll      -> putByte bh 2

    get bh = do
        x <- getByte bh
        case x of
            0 -> return RtsOptsNone
            1 -> return RtsOptsSafeOnly
            2 -> return RtsOptsAll
            _ -> error "Unhandled RtsOptsEnabled serilization"

-- | PackageFlag Binary Instance
instance Binary PackageFlag where
    put_ bh pflag =
        case pflag of
            (ExposePackage   s) -> store 0 s
            (ExposePackageId s) -> store 1 s
            (HidePackage     s) -> store 2 s
            (IgnorePackage   s) -> store 3 s
            (TrustPackage    s) -> store 4 s
            (DistrustPackage s) -> store 5 s
        where store n s = putByte bh n >> put_ bh s

    get bh = do
        n <- getByte bh
        s <- get bh
        return $ case n of
            0 -> ExposePackage   s
            1 -> ExposePackageId s
            2 -> HidePackage     s
            3 -> IgnorePackage   s
            4 -> TrustPackage    s
            5 -> DistrustPackage s
            _ -> error "Unhandled PackageFlag serilization"
-}

