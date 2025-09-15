{-# LANGUAGE DataKinds #-}
module T3 where

import GHC.TypeLits

f :: TypeError (Text "This is a type error")
f = undefined
