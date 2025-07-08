{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cxx
    ( Cxx(..)
    , findCxx
      -- * Helpful utilities
    , compileCxx, _cxxProgram
    ) where

import System.FilePath

import GHC.Platform.ArchOS
import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import GHC.Toolchain.Utils

newtype Cxx = Cxx { cxxProgram :: Program
                  }
    deriving (Show, Read, Eq, Ord)

_cxxProgram :: Lens Cxx Program
_cxxProgram = Lens cxxProgram (\x o -> o{cxxProgram=x})

findCxx :: ArchOS
        -> String -- ^ The llvm target to use if Cc supports --target
        -> ProgOpt -> M Cxx
findCxx archOs target progOpt = checking "for C++ compiler" $ do
    -- TODO: We use the search order in configure, but there could be a more optimal one
    cxxProgram <- findProgram "C++ compiler" progOpt ["g++", "clang++", "c++"]
    cxx        <- cxxSupportsTarget archOs target Cxx{cxxProgram}
    checkCxxWorks cxx
    return cxx

cxxSupportsTarget :: ArchOS -> String -> Cxx -> M Cxx
cxxSupportsTarget archOs target cxx =
    checking "whether C++ supports --target" $
    supportsTarget archOs _cxxProgram checkCxxWorks target cxx

checkCxxWorks :: Cxx -> M ()
checkCxxWorks cxx = withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileCxx cxx test_o $ unlines
        [ "#include <stdio.h>"
        , "int main(int argc, char **argv) {"
        , "  printf(\"hello world!\");"
        , "  return 0;"
        , "}"
        ]

compileCxx
    :: Cxx      -- ^ cxx
    -> FilePath -- ^ output path
    -> String   -- ^ C++ source
    -> M ()
compileCxx = compile "cpp" ["-c"] _cxxProgram
