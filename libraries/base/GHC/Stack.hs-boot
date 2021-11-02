{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Stack (CallStack, HasCallStack, callStack) where

import GHC.Stack.Types

callStack :: HasCallStack => CallStack
