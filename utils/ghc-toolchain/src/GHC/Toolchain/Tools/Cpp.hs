{-# LANGUAGE NamedFieldPuns #-}

module GHC.Toolchain.Tools.Cpp (Cpp(..), findCpp) where

import Control.Monad

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program

import GHC.Toolchain.Tools.Cc

data Cpp = Cpp { cppProgram :: Program
               }
    deriving (Show, Read)

findCpp :: ProgOpt -> Cc -> M Cpp
findCpp progOpt cc
  | Just _ <- poPath progOpt = checking "for C preprocessor" $ do
    -- If the user specified a linker don't second-guess them
    cppProgram <- findProgram "C preprocessor" progOpt []
    return Cpp{cppProgram}
  | otherwise = checking "for C preprocessor" $ do
    let rawCppProgram = over _prgFlags (["-E"]++) (ccProgram cc)
    hppArgs <- findHsCppArgs rawCppProgram
    let cppProgram = over _prgFlags (++hppArgs) rawCppProgram
    return Cpp{cppProgram}

-- | Given a C preprocessor, figure out how it should be invoked to preprocess
-- Haskell source.
findHsCppArgs :: Program -> M [String]
findHsCppArgs cpp =
    concat <$> sequence
        [ ["-traditional"] <$ checkFlag "-traditional"
        , tryFlag "-undef"
        , tryFlag "-Wno-invalid-pp-token"
        , tryFlag "-Wno-unicode"
        , tryFlag "-Wno-trigraphs"
        ]
  where
    -- Werror to ensure that unrecognized warnings result in an error
    checkFlag flag =
        checking ("for "++flag++" support") $ callProgram cpp ["-E", "-Werror", flag, "/dev/null"]

    tryFlag flag =
        ([flag] <$ checkFlag flag) <|> return []

