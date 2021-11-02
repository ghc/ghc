{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Stack.CloneStack.Types where

import GHC.Base
import GHC.Show

-- | Represetation for the source location where a return frame was pushed on the stack.
-- This happens every time when a @case ... of@ scrutinee is evaluated.
data StackEntry = StackEntry
  { functionName :: String,
    moduleName :: String,
    srcLoc :: String,
    closureType :: Word
  }
  deriving (Show, Eq)

pprStackEntry :: StackEntry -> String
pprStackEntry se =
  foldr
    (++)
    ""
    [ moduleName se,
      ".",
      functionName se,
      ":",
      " ",
      srcLoc se
    ]
