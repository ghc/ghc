{-# LANGUAGE MultiWayIf #-}

module Oracles.Flag (
    Flag (..), FlagName (..), flag, getFlag, platformSupportsSharedLibs,
    ghcWithNativeCodeGen, targetSupportsSMP
    ) where

import Hadrian.Oracles.TextFile
import Hadrian.Expression

import Base
import Context
import Oracles.Setting

-- Flags can be either staged or not.  Global flags are assumed to be identical
-- across all stages.  While staged flags are specific to a given stage.  Flags
-- are read from the @cfg/system.config@ (the result of configure processing
-- @cfg/system.config.in@).  See @flag@ below for the actual FlagName to lookup
-- key mapping.  If staged flags can not be found, they fall back to the Global
-- flag.  Thus we can special case only single stages while falling back to the
-- global value.
--
-- Example: When cross compiling the Stage0 (bootstrap) @ar@ might be bsd ar and
-- thus not support \@ response files.  However the the Stage1+ toolchain might.
-- Therefore we must special case @ArSupportsAtFile@ for stage0 to be NO, while
-- it can be YES for stage1+.
data FlagName = ArSupportsAtFile
              | CrossCompiling
              | CcLlvmBackend
              | GhcUnregisterised
              | TablesNextToCode
              | GmpInTree
              | GmpFrameworkPref
              | LeadingUnderscore
              | SolarisBrokenShld
              | WithLibdw
              | WithLibnuma
              | HaveLibMingwEx
              | UseSystemFfi
              | BootstrapThreadedRts

-- Use Global if you are certain the flag is global across all stages (or there
-- simply is no stage/context available).  Use of Staged is preferred as it
-- provides more precise information about the use of the Flag.
data Flag = Global FlagName
          | Staged Stage FlagName

-- Note, if a flag is set to empty string we treat it as set to NO. This seems
-- fragile, but some flags do behave like this.
flag :: Flag -> Action Bool
flag f = do
    let configName flagName = case flagName of
            ArSupportsAtFile     -> "ar-supports-at-file"
            CrossCompiling       -> "cross-compiling"
            CcLlvmBackend        -> "cc-llvm-backend"
            GhcUnregisterised    -> "ghc-unregisterised"
            TablesNextToCode     -> "tables-next-to-code"
            GmpInTree            -> "intree-gmp"
            GmpFrameworkPref     -> "gmp-framework-preferred"
            LeadingUnderscore    -> "leading-underscore"
            SolarisBrokenShld    -> "solaris-broken-shld"
            WithLibdw            -> "with-libdw"
            WithLibnuma          -> "with-libnuma"
            HaveLibMingwEx       -> "have-lib-mingw-ex"
            UseSystemFfi         -> "use-system-ffi"
            BootstrapThreadedRts -> "bootstrap-threaded-rts"

    (key, value) <- case f of
            Global fn   -> let key = configName fn in (key,) <$> lookupValueOrError configFile key
            Staged s fn -> do
                let key = configName fn
                    stagedKey = key ++ "-" ++ stageString s
                    msg = "Key " ++ quote stagedKey ++ " or " ++ quote key ++ " not found in file " ++ quote configFile
                mStagedVal <- fmap (stagedKey,) <$> lookupValue configFile stagedKey
                mGlobalVal <- fmap (key,) <$> lookupValue configFile key
                return $ fromMaybe (error msg) (mStagedVal <|> mGlobalVal)

    when (value `notElem` ["YES", "NO", ""]) . error $ "Configuration flag "
        ++ quote (key ++ " = " ++ value) ++ " cannot be parsed."
    return $ value == "YES"

-- | Get a configuration setting.
getFlag :: Flag -> Expr Context b Bool
getFlag = expr . flag

platformSupportsSharedLibs :: Action Bool
platformSupportsSharedLibs = do
    badPlatform   <- anyTargetPlatform [ "powerpc-unknown-linux"
                                       , "x86_64-unknown-mingw32"
                                       , "i386-unknown-mingw32" ]
    solaris       <- anyTargetPlatform [ "i386-unknown-solaris2" ]
    solarisBroken <- flag (Global SolarisBrokenShld)
    return $ not (badPlatform || solaris && solarisBroken)

-- | Does the target support the threaded runtime system?
targetSupportsSMP :: Action Bool
targetSupportsSMP = do
  unreg <- flag (Global GhcUnregisterised)
  armVer <- targetArmVersion
  goodArch <- anyTargetArch ["i386", "x86_64", "sparc", "powerpc", "arm", "aarch64", "s390x"]
  if   -- The THREADED_RTS requires `BaseReg` to be in a register and the
       -- Unregisterised mode doesn't allow that.
     | unreg                -> return False
       -- We don't support load/store barriers pre-ARMv7. See #10433.
     | Just ver <- armVer
     , ver < ARMv7          -> return False
     | goodArch             -> return True
     | otherwise            -> return False

ghcWithNativeCodeGen :: Action Bool
ghcWithNativeCodeGen = do
    goodArch <- anyTargetArch ["i386", "x86_64", "sparc", "powerpc"]
    badOs    <- anyTargetOs ["ios", "aix"]
    ghcUnreg <- flag (Global GhcUnregisterised)
    return $ goodArch && not badOs && not ghcUnreg
