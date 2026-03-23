{-# LANGUAGE NoImplicitPrelude, RankNTypes, MagicHash #-}

module GHC.Internal.Stack where

import GHC.Internal.Base
import GHC.Internal.Stack.Types

prettyCallStackLines :: CallStack -> [String]
prettyCallStack :: CallStack -> String
prettySrcLoc :: SrcLoc -> String
withFrozenCallStack :: HasCallStack => ( HasCallStack => a ) -> a
