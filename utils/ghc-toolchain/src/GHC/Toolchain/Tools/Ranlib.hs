{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Ranlib
    ( Ranlib(..)
    , findRanlib
    ) where

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

newtype Ranlib = Ranlib { ranlibProgram :: Program
                        }
    deriving (Show, Read, Eq, Ord)

findRanlib :: ProgOpt -> M Ranlib
findRanlib progOpt = checking "for 'ranlib'" $ do
    ranlibProgram <- findProgram "ranlib archiver" progOpt ["ranlib", "llvm-ranlib"]
    return $ Ranlib {ranlibProgram}

