{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cxx
    ( Cxx(..)
    , findCxx
    ) where

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

data Cxx = Cxx { cxxProgram :: Program
               }
    deriving (Show, Read)

findCxx :: ProgOpt -> M Cxx
findCxx progOpt = checking "for C++ compiler" $ do
    cxxProgram <- findProgram "C++ compiler" progOpt ["c++", "clang++", "g++"]
    return $ Cxx {cxxProgram}

