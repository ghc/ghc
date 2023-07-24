{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cpp (HsCpp(..), findHsCpp, Cpp(..), findCpp) where

import Control.Monad
import System.FilePath

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import GHC.Toolchain.Utils (withTempDir)

import GHC.Toolchain.Tools.Cc

newtype Cpp = Cpp { cppProgram :: Program
                    }
    deriving (Show, Read, Eq, Ord)

newtype HsCpp = HsCpp { hsCppProgram :: Program
                      }
    deriving (Show, Read, Eq, Ord)

----- Haskell Preprocessor -----

findHsCpp :: ProgOpt -> Cc -> M HsCpp
findHsCpp progOpt cc = checking "for Haskell C preprocessor" $ do
  -- Use the specified Hs Cpp or try to use the c compiler
  foundHsCppProg <- findProgram "a user specified Haskell C preprocessor" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
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
  let tmp_h = dir </> "tmp.h"

      -- Werror to ensure that unrecognized warnings result in an error
      checkFlag flag =
          checking ("for "++flag++" support") $ callProgram cpp ["-Werror", flag, tmp_h]

      tryFlag flag =
          ([flag] <$ checkFlag flag) <|> return []

  writeFile tmp_h ""
  concat <$> sequence
      [ tryFlag "-undef"
      , ["-traditional"] <$ checkFlag "-traditional"
      , tryFlag "-Wno-invalid-pp-token"
      , tryFlag "-Wno-unicode"
      , tryFlag "-Wno-trigraphs"
      ]

----- C preprocessor -----

findCpp :: ProgOpt -> Cc -> M Cpp
findCpp progOpt cc = checking "for C preprocessor" $ do
  -- Use the specified CPP or try to use the c compiler
  foundCppProg <- findProgram "a user specified C preprocessor" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
  -- Always add the -E flag to the CPP, regardless of the user options
  let cppProgram = addFlagIfNew "-E" foundCppProg
  return Cpp{cppProgram}

