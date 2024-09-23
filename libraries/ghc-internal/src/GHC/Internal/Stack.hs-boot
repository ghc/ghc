{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module GHC.Internal.Stack where

import GHC.Internal.Base
import GHC.Internal.Stack.Types (HasCallStack, CallStack, SrcLoc)

prettyCallStackLines :: CallStack -> [String]
prettyCallStack :: CallStack -> String
prettySrcLoc :: SrcLoc -> String
withFrozenCallStack :: HasCallStack => ( HasCallStack => a ) -> a
