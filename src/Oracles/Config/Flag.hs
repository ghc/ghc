module Oracles.Config.Flag (
    Flag (..), flag, getFlag,
    crossCompiling, gccIsClang, gccGe46,
    platformSupportsSharedLibs, ghcWithSMP, ghcWithNativeCodeGen
    ) where

import Base
import Oracles.Config
import Oracles.Config.Setting

data Flag = CrossCompiling
          | GccIsClang
          | GccLt46
          | GhcUnregisterised
          | SolarisBrokenShld
          | SplitObjectsBroken
          | SupportsPackageKey

-- Note, if a flag is set to empty string we treat it as set to NO. This seems
-- fragile, but some flags do behave like this, e.g. GccIsClang.
flag :: Flag -> Action Bool
flag f = do
    key <- return $ case f of
        CrossCompiling     -> "cross-compiling"
        GccIsClang         -> "gcc-is-clang"
        GccLt46            -> "gcc-lt-46"
        GhcUnregisterised  -> "ghc-unregisterised"
        SolarisBrokenShld  -> "solaris-broken-shld"
        SplitObjectsBroken -> "split-objects-broken"
        SupportsPackageKey -> "supports-package-key"
    value <- askConfigWithDefault key . putError
        $ "\nFlag '" ++ key ++ "' not set in configuration files."
    unless (value == "YES" || value == "NO" || value == "") . putError
        $ "\nFlag '" ++ key ++ "' is set to '" ++ value
        ++ "' instead of 'YES' or 'NO'."
    return $ value == "YES"

getFlag :: Flag -> ReaderT a Action Bool
getFlag = lift . flag

crossCompiling :: Action Bool
crossCompiling = flag CrossCompiling

gccIsClang :: Action Bool
gccIsClang = flag GccIsClang

gccGe46 :: Action Bool
gccGe46 = fmap not $ flag GccLt46

platformSupportsSharedLibs :: Action Bool
platformSupportsSharedLibs = do
    badPlatform   <- targetPlatforms [ "powerpc-unknown-linux"
                                     , "x86_64-unknown-mingw32"
                                     , "i386-unknown-mingw32" ]
    solaris       <- targetPlatform    "i386-unknown-solaris2"
    solarisBroken <- flag SolarisBrokenShld
    return $ not (badPlatform || solaris && solarisBroken)

ghcWithSMP :: Action Bool
ghcWithSMP = do
    goodArch <- targetArchs ["i386", "x86_64", "sparc", "powerpc", "arm"]
    ghcUnreg <- flag GhcUnregisterised
    return $ goodArch && not ghcUnreg

ghcWithNativeCodeGen :: Action Bool
ghcWithNativeCodeGen = do
    goodArch <- targetArchs ["i386", "x86_64", "sparc", "powerpc"]
    badOs    <- targetOss ["ios", "aix"]
    ghcUnreg <- flag GhcUnregisterised
    return $ goodArch && not badOs && not ghcUnreg
