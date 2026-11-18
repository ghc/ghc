{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module NativeClosedListsWrong where

import NativeClosedListsLib ()
import Numeric.Natural (Natural)
import Prelude

data NatListTag (ns :: [Natural]) where
    Original :: NatListTag '[2, 3, 5]

wrong :: NatListTag (Prelude.reverse '[2, 3, 5])
wrong = Original
