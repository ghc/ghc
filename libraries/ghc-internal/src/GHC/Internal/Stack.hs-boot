{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Stack where

import GHC.Internal.Base
import GHC.Internal.Stack.Types (CallStack, SrcLoc)

prettyCallStackLines :: CallStack -> [String]
prettyCallStack :: CallStack -> String
prettySrcLoc :: SrcLoc -> String
