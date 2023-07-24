{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cxx
    ( Cxx(..)
    , findCxx
      -- * Helpful utilities
    , compileCxx
    ) where

import System.FilePath
import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import GHC.Toolchain.Utils
import GHC.Toolchain.Tools.Cc

newtype Cxx = Cxx { cxxProgram :: Program
                  }
    deriving (Show, Read, Eq, Ord)

_cxxProgram :: Lens Cxx Program
_cxxProgram = Lens cxxProgram (\x o -> o{cxxProgram=x})

findCxx :: String -- ^ The llvm target to use if Cc supports --target
        -> ProgOpt -- ^ A user specified C++ compiler
        -> Cc      -- ^ The C compiler, to try as a fallback C++ compiler if we can't find one.
        -> M Cxx
findCxx target progOpt cc = checking "for C++ compiler" $ do
    -- TODO: We use the search order in configure, but there could be a more optimal one
    cxxProgram <- findProgram "C++ compiler" progOpt ["g++", "clang++", "c++"] <|> pure (Program (prgPath $ ccProgram cc) [])
    cxx        <- cxxSupportsTarget target Cxx{cxxProgram}
    checkCxxWorks cxx
    return cxx

cxxSupportsTarget :: String -> Cxx -> M Cxx
cxxSupportsTarget target cxx = checking "whether C++ supports --target" $
                               supportsTarget _cxxProgram checkCxxWorks target cxx

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
