{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeClosedNestedKind where

import qualified Data.Char
import Data.Proxy (Proxy(..))
import qualified NativeClosedListsLib as Lib

bad :: Proxy (Data.Char.toTitle (Prelude.reverse (Lib.badElement '[1])))
bad = Proxy
