{-# LANGUAGE DisambiguateRecordFields #-}

module NFSMixed where

import NFSMixedA

test = \x -> x { foo = 0 }
