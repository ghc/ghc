module Oracles.Flag (
    Flag (..),
    test
    ) where

import Base
import Util
import Oracles.Base

data Flag = LaxDeps
          | DynamicGhcPrograms
          | GccIsClang
          | GccLt46
          | CrossCompiling
          | Validating
          | SupportsPackageKey
          | SolarisBrokenShld
          | SplitObjectsBroken
          | GhcUnregisterised

-- TODO: Give the warning *only once* per key
test :: Flag -> Action Bool
test flag = do
    (key, defaultValue) <- return $ case flag of
        LaxDeps            -> ("lax-dependencies"     , False)
        DynamicGhcPrograms -> ("dynamic-ghc-programs" , False)
        GccIsClang         -> ("gcc-is-clang"         , False)
        GccLt46            -> ("gcc-lt-46"            , False)
        CrossCompiling     -> ("cross-compiling"      , False)
        Validating         -> ("validating"           , False)
        SupportsPackageKey -> ("supports-package-key" , False)
        SolarisBrokenShld  -> ("solaris-broken-shld"  , False)
        SplitObjectsBroken -> ("split-objects-broken" , False)
        GhcUnregisterised  -> ("ghc-unregisterised"   , False)
    let defaultString = if defaultValue then "YES" else "NO"
    value <- askConfigWithDefault key $ -- TODO: warn just once
        do putColoured Red $ "\nFlag '"
                ++ key
                ++ "' not set in configuration files. "
                ++ "Proceeding with default value '"
                ++ defaultString
                ++ "'.\n"
           return defaultString
    return $ value == "YES"
