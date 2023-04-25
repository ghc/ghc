{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.Nm (Nm(..), findNm) where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

data Nm = Nm { nmProgram :: Program
             }
    deriving (Show, Read)

findNm :: ProgOpt -> M Nm
findNm progOpt = checking "for 'nm'" $ do
    nmProgram <- findProgram "nm utility" progOpt ["nm"]
    return Nm {..}

