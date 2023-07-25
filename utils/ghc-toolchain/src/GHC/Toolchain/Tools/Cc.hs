{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cc
    ( Cc(..)
    , _ccProgram
    , findCc
      -- * Helpful utilities
    , preprocess
    , compileC
    , compileAsm
    , addPlatformDepCcFlags
    ) where

import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program

data Cc = Cc { ccProgram :: Program
             }
    deriving (Show, Read)

_ccProgram :: Lens Cc Program
_ccProgram = Lens ccProgram (\x o -> o{ccProgram=x})

findCc :: ProgOpt -> M Cc
findCc progOpt = checking "for C compiler" $ do
    ccProgram <- findProgram "C compiler" progOpt ["cc", "clang", "gcc"] 
    cc <- ignoreUnusedArgs $ Cc {ccProgram}
    checkCcWorks cc
    checkC99Support cc
    return cc

checkCcWorks :: Cc -> M ()
checkCcWorks cc = withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileC cc test_o $ unlines 
        [ "#include <stdio.h>"
        , "int main(int argc, char **argv) {"
        , "  printf(\"hello world!\");"
        , "  return 0;"
        , "}"
        ]

-- | GHC tends to produce command-lines with unused arguments that elicit
-- warnings from Clang. Clang offers the @-Qunused-arguments@ flag to silence
-- these. See #11684.
ignoreUnusedArgs :: Cc -> M Cc
ignoreUnusedArgs cc = checking "for -Qunused-arguments support" $ do
    let cc' = over (_ccProgram % _prgFlags) (++["-Qunused-arguments"]) cc
    (cc' <$ checkCcWorks cc') <|> return cc

checkC99Support :: Cc -> M ()
checkC99Support cc = checking "for C99 support" $ withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileC cc test_o $ unlines
        [ "#include <stdio.h>"
        , "#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L"
        , "# error \"Compiler does not advertise C99 conformance\""
        , "#endif"
        ]

-- | Preprocess the given program.
preprocess
    :: Cc
    -> String   -- ^ program
    -> M String -- ^ preprocessed output
preprocess cc prog = withTempDir $ \dir -> do
    let out = dir </> "test.c"
    compile "c" ["-E"] cc out prog
    readFile out

-- | Compile a C source file to object code.
compileC
    :: Cc       -- ^ cc
    -> FilePath -- ^ output path
    -> String   -- ^ C source
    -> M ()
compileC = compile "c" ["-c"]

-- | Compile an assembler source file to object code.
compileAsm
    :: Cc       -- ^ cc
    -> FilePath -- ^ output path
    -> String   -- ^ Assembler source
    -> M ()
compileAsm = compile "S" ["-c"]

compile
    :: FilePath  -- ^ input extension
    -> [String]  -- ^ extra flags
    -> Cc
    -> FilePath  -- ^ output path
    -> String    -- ^ source
    -> M ()
compile ext extraFlags cc outPath program = do
    let srcPath = outPath <.> ext
    writeFile srcPath program
    callProgram (ccProgram cc) $ extraFlags ++ ["-o", outPath, srcPath]
    expectFileExists outPath "compiler produced no output"

-- | Add various platform-dependent compiler flags needed by GHC. We can't do
-- this in `findCc` since we need a 'Cc` to determine the 'ArchOS'.
addPlatformDepCcFlags :: ArchOS -> Cc -> M Cc
addPlatformDepCcFlags archOs cc
  | OSMinGW32 <- archOS_OS archOs = do
      checkFStackCheck cc <|> throwE "Windows requires -fstack-check support yet the C compiler appears not to support it"
  | otherwise = return cc

-- | Check that @cc@ supports @-fstack-check@.
-- See Note [Windows stack allocations].
checkFStackCheck :: Cc -> M Cc
checkFStackCheck cc = withTempDir $ \dir -> checking "that -fstack-check works" $ do
      let cc' = over (_ccProgram % _prgFlags) (++["-Wl,-fstack-checkzz"]) cc
      compileC cc' (dir </> "test.o") "int main(int argc, char **argv) { return 0; }"
      return cc'
