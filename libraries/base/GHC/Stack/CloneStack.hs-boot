{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Stack.CloneStack where

import {-# SOURCE #-} GHC.IO (IO (..))
import GHC.Stack.CloneStack.Types

data StackSnapshot

cloneMyStack :: IO StackSnapshot

decode :: StackSnapshot -> IO [StackEntry]
