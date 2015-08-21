module Oracles.Flag (
    Flag (..), flag,
    crossCompiling, gccIsClang, gccLt46,
    platformSupportsSharedLibs, ghcWithSMP, ghcWithNativeCodeGen
    ) where

import Base
import Util
import Oracles.Base
import Oracles.Setting
import Control.Monad

data Flag = GccIsClang
          | GccLt46
          | CrossCompiling
          | SupportsPackageKey
          | SolarisBrokenShld
          | SplitObjectsBroken
          | GhcUnregisterised

flag :: Flag -> Action Bool
flag f = do
    key <- return $ case f of
        GccIsClang         -> "gcc-is-clang"
        GccLt46            -> "gcc-lt-46"
        CrossCompiling     -> "cross-compiling"
        SupportsPackageKey -> "supports-package-key"
        SolarisBrokenShld  -> "solaris-broken-shld"
        SplitObjectsBroken -> "split-objects-broken"
        GhcUnregisterised  -> "ghc-unregisterised"
    value <- askConfigWithDefault key . putError
        $ "\nFlag '" ++ key ++ "' not set in configuration files."
    unless (value == "YES" || value == "NO") . putError
        $ "\nFlag '" ++ key ++ "' is set to '" ++ value
        ++ "' instead of 'YES' or 'NO'."
    return $ value == "YES"

crossCompiling :: Action Bool
crossCompiling = flag CrossCompiling

gccIsClang :: Action Bool
gccIsClang = flag GccIsClang

gccLt46 :: Action Bool
gccLt46 = flag GccLt46

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
