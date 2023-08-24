{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cpp (HsCpp(..), findHsCpp, Cpp(..), findCpp) where

import Control.Monad
import System.FilePath
import Data.List(isInfixOf)

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Utils (withTempDir, oneOf)

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

      -- Werror to ensure that unrecognized warnings result in an error
  let checkFlag flag =
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
      -}

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

