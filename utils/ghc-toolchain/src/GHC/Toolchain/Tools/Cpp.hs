{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cpp
  (HsCpp(..), findHsCpp
  , Cpp(..), findCpp
  , JsCpp(..), findJsCpp
  , CmmCpp(..), findCmmCpp

    -- * Lenses
  , _cppProg, _hsCppProg, _jsCppProg, _cmmCppProg
  ) where

import Control.Monad
import System.FilePath
import Data.List(isInfixOf, dropWhileEnd)
import Data.Char(isSpace)

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Utils (withTempDir, oneOf, expectFileExists)

newtype Cpp = Cpp { cppProgram :: Program
                    }
    deriving (Show, Read, Eq, Ord)

newtype HsCpp = HsCpp { hsCppProgram :: Program
                      }
    deriving (Show, Read, Eq, Ord)

newtype JsCpp = JsCpp { jsCppProgram :: Program
                      }
    deriving (Show, Read, Eq, Ord)

data CmmCpp = CmmCpp { cmmCppProgram :: Program
                     , cmmCppSupportsG0 :: Bool
                     -- ^ Whether the C-- preprocessor supports -g0.  Extracted
                     -- out as -g0 needs to be appended to the complete
                     -- invocation, rather than prefix flags, in order to
                     -- override other flags.
                     }
    deriving (Show, Read, Eq, Ord)

checkFlag :: String -> Program -> String -> [String] ->  M ()
checkFlag conftest cpp flag extra_args = checking ("for "++flag++" support") $
  -- Werror to ensure that unrecognized warnings result in an error
  callProgram cpp $ ["-Werror", flag, conftest] ++ extra_args
-- tryFlag :: String -> Program -> String -> M [String]
-- tryFlag conftest cpp flag =
--   ([flag] <$ checkFlag conftest cpp flag) <|> return []


----- Haskell Preprocessor -----

findHsCpp :: ProgOpt -> Cc -> M HsCpp
findHsCpp progOpt cc = checking "for Haskell C preprocessor" $ do
  -- Use the specified Hs Cpp or try to use the c compiler
  foundHsCppProg <- findProgram "Haskell C preprocessor" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
  -- Always add the -E flag to the CPP, regardless of the user options
  let rawHsCppProgram = addFlagIfNew "-E" foundHsCppProg
  -- Always try to add the Haskell-specific CPP flags, regardless of the user options
  hppArgs <- findHsCppArgs rawHsCppProgram
  let hsCppProgram = over _prgFlags (++hppArgs) rawHsCppProgram
  return HsCpp{hsCppProgram}

-- | Given a C preprocessor, figure out how it should be invoked to preprocess
-- Haskell source.
findHsCppArgs :: Program -> M [String]
findHsCppArgs cpp = withTempDir $ \dir -> do

  let tmp_c = dir </> "tmp.c"
  writeFile tmp_c ""
  (_, stdout0, stderr0) <- readProgram cpp ["-x", "c", tmp_c, "-dM", "-E"]

  if "__clang__" `isInfixOf` stdout0 || "__clang__" `isInfixOf` stderr0
     then return ["-undef", "-traditional", "-Wno-invalid-pp-token", "-Wno-unicode", "-Wno-trigraphs"]
     else do
        (_, stdout1, stderr1) <- readProgram cpp ["-v"]
        if "gcc" `isInfixOf` stdout1 || "gcc" `isInfixOf` stderr1
          then return ["-undef", "-traditional"]
          else do
            logDebug "Can't recognize your CPP program, you may need to set --with-hs-cpp-flags=FLAGS explicitly"
            return []


{- TODO: We want to just check which flags are accepted rather than branching on which compiler
         we are using but this does not match what ./configure does (#23720)

         When we retire configure then this more precise logic can be reinstated.
  withTmpDir $ \dir -> do
  let tmp_h = dir </> "tmp.h"

  writeFile tmp_h ""
  concat <$> sequence
      [ tryFlag "-undef"
      , ["-traditional"] <$ checkFlag tmp_h cpp "-traditional"
      , tryFlag tmp_h cpp "-Wno-invalid-pp-token"
      , tryFlag tmp_h cpp "-Wno-unicode"
      , tryFlag tmp_h cpp "-Wno-trigraphs"
      ]
      -}

----- JavaScript preprocessor -----

findJsCpp :: ProgOpt -> Cc -> M JsCpp
findJsCpp progOpt cc = checking "for JavaScript C preprocessor" $ do
  -- Use the specified Js Cpp
  foundJsCppProg <- findProgram "JavaScript C preprocessor" progOpt ["emcc"] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])

  _ <- withTempDir $ \tmp_dir -> do
    checkIsProcessing tmp_dir foundJsCppProg

  let jsCppProgram = foldr addFlagIfNew foundJsCppProg (reverse required_flags_to_add)
  return JsCpp{jsCppProgram}
  where
    -- See: https://gcc.gnu.org/onlinedocs/gcc/Preprocessor-Options.html#index-CC
    -- See: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-CC
    -- Emscripten supports -C and -CC options same as GCC and CLang
    -- Always try to add the JavaScript-specific CPP flags, regardless of the user options
    -- Always add the -E flag, regardless of the user options
    -- We have to use -nostdinc to prevent adding copyright headers in gcc output.
    -- This issue is known and discussed here: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=59566
    required_flags_to_add =
      [ "-E"
      , "-CC"
      , "-Wno-unicode"
      , "-nostdinc"
      ]

    flags_for_test = required_flags_to_add ++
      [ "-P"
      , "-x", "assembler-with-cpp"
      ]

    file_source = "conftest.js"
    file_output = "conftest.pp.js"

    trim = dropWhileEnd isSpace . dropWhile isSpace

    checkIsProcessing tmp_dir prog = do
      let
        file_source_in_dir = tmp_dir </> file_source
        file_output_in_dir = tmp_dir </> file_output

      writeFile file_source_in_dir "#define DEF_TEST\n#ifdef DEF_TEST\n// 1\n#endif\n"

      callProgram
        (prog{ prgFlags = [] })
        (flags_for_test ++ ["-o", file_output_in_dir, file_source_in_dir])

      expectFileExists file_output_in_dir ("JavaScript C Preprocessor didn't create the output file: " ++ file_output_in_dir)

      file_output_in_dir_content <- readFile file_output_in_dir
      unless (trim file_output_in_dir_content == "// 1")
        $ throwE "JavaScript C Preprocessor didn't provide correct output"

----- C-- preprocessor -----

findCmmCpp :: ProgOpt -> Cc -> M CmmCpp
findCmmCpp progOpt cc = checking "for a Cmm preprocessor" $ do
  -- Use the specified CPP or try to use the c compiler
  foundCppProg <- findProgram "Cmm preprocessor" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
  -- Check whether the C preprocessor needs -std=gnu99 (only very old toolchains need this)
  Cc cpp <- oneOf "cc doesn't support C99" $ map checkC99Support
        [ Cc foundCppProg
        , Cc (foundCppProg & _prgFlags %++ "-std=gnu99")
        ]

  cmmCppSupportsG0 <- withTempDir $ \dir -> do
    let conftest = dir </> "conftest.c"
    writeFile conftest "int main(void) {}"
    True <$ checkFlag conftest cpp "-g0" ["-o", dir </> "conftest"] <|> return False

  -- Always add the -E flag to the CPP, regardless of the user options
  let cmmCppProgram = foldr addFlagIfNew cpp ["-E"]
  return CmmCpp{cmmCppProgram, cmmCppSupportsG0}

----- C preprocessor -----

findCpp :: ProgOpt -> Cc -> M Cpp
findCpp progOpt cc = checking "for C preprocessor" $ do
  -- Use the specified CPP or try to use the c compiler
  foundCppProg <- findProgram "C preprocessor" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
  -- Check whether the C preprocessor needs -std=gnu99 (only very old toolchains need this)
  Cc cpp2 <- oneOf "cc doesn't support C99" $ map checkC99Support
        [ Cc foundCppProg
        , Cc (foundCppProg & _prgFlags %++ "-std=gnu99")
        ]
  -- Always add the -E flag to the CPP, regardless of the user options
  let cppProgram = addFlagIfNew "-E" cpp2
  return Cpp{cppProgram}

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

_cppProg :: Lens Cpp Program
_cppProg = Lens cppProgram (\x o -> o{cppProgram = x})

_hsCppProg :: Lens HsCpp Program
_hsCppProg = Lens hsCppProgram (\x o -> o{hsCppProgram = x})

_jsCppProg :: Lens JsCpp Program
_jsCppProg = Lens jsCppProgram (\x o -> o{jsCppProgram = x})

_cmmCppProg :: Lens CmmCpp Program
_cmmCppProg = Lens cmmCppProgram (\x o -> o{cmmCppProgram = x})
