{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Stack where

import GHC.Base
import GHC.Stack.Types (CallStack, SrcLoc)

prettyCallStackLines :: CallStack -> [String]
prettyCallStack :: CallStack -> String
prettySrcLoc :: SrcLoc -> String
