{-# LANGUAGE PartialTypeSignatures #-}
module T24769d where

import GHC.Exts
import GHC.Stack

f :: forall r (a :: TYPE r). (HasCallStack, _) => a
f = undefined
