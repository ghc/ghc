module Outputable.DynFlags where

import GhcPrelude

import {-# SOURCE #-} DynFlags ( DynFlags )
import {-# SOURCE #-} Outputable

import GHC.Stack( HasCallStack )

type SDoc = SDoc' DynFlags

showSDocUnsafe :: SDoc -> String

warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc -> a -> a
