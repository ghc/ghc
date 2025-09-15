{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude

{-# LANGUAGE RecursiveDo #-}

main = pure ()

foo :: forall a. a -> a
foo x = mdo x
