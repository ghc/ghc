{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}

module NativeLiteralClient where

import PackedVecLibrary (Vec, replicateNative)

three :: Vec Bool 3
three = replicateNative 3 True
