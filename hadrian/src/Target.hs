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
    Ghc _ _   -> not $ verbosityArg arg
    _         -> True
  where
    threadArg s = dropWhileEnd isDigit s `elem` ["-j", "MAKEFLAGS=-j", "THREADS="]
    verbosityArg s = dropWhileEnd isDigit s == "-v"
