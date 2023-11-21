{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.Otool where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

newtype Otool = Otool { otoolProgram :: Program
                      }
    deriving (Show, Read, Eq, Ord)

findOtool :: ProgOpt -> M Otool
findOtool progOpt = checking "for otool" $ do
    otoolProgram <- findProgram "otool utility" progOpt ["otool"]
    return Otool {..}
