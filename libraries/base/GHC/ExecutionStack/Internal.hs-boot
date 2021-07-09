{-# LANGUAGE NoImplicitPrelude #-}

module GHC.ExecutionStack.Internal where

import GHC.Show (ShowS)

data Location

showStackFrames :: [Location] -> ShowS
