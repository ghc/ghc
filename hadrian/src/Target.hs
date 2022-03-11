{-# LANGUAGE LambdaCase #-}

module Target (
    Target, target, context, builder, inputs, outputs, trackArgument,
    module Builder
    ) where

import Data.Char
import Data.List.Extra

import qualified Hadrian.Target as H
import Hadrian.Target hiding (Target)

import Builder
import Context

type Target = H.Target Context Builder

-- | Some arguments do not affect build results and therefore do not need to be
-- tracked by the build system. A notable example is "-jN" that controls Make's
-- parallelism. Given a 'Target' and an argument, this function should return
-- 'True' only if the argument needs to be tracked.
trackArgument :: Target -> String -> Bool
trackArgument target arg = case builder target of
    Make _    -> not $ threadArg arg
    Ghc _ _   -> not $ verbosityArg arg || diagnosticsColorArg arg
    Cabal _ _ -> not $ verbosityArg arg || cabal_configure_ignore arg
    _         -> True
  where
    match_str_num []     rs     = all isDigit rs
    match_str_num (x:xs) (r:rs) = x == r && match_str_num xs rs
    match_str_num (_:_)  []     = False

    threadArg s = match_str_num "-j" s || match_str_num "MAKEFLAGS=-j" s || match_str_num "THREADS=" s
    verbosityArg s = match_str_num "-v" s
    diagnosticsColorArg s = "-fdiagnostics-color=" `isPrefixOf` s -- N.B. #18672
    cabal_configure_ignore = \case
      "--configure-option=--quiet"                   -> True
      "--configure-option=--disable-option-checking" -> True
      _ -> False
