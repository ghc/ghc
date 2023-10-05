{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.Nm (Nm(..), findNm) where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

newtype Nm = Nm { nmProgram :: Program
                }
    deriving (Show, Read, Eq, Ord)

findNm :: ProgOpt -> M Nm
findNm progOpt = checking "for 'nm'" $ do
    nmProgram <- findProgram "nm utility" progOpt ["nm", "llvm-nm"]
    return Nm {..}

