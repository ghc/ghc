{-# LANGUAGE TemplateHaskell #-}
-- | This tests the behaviour of TH's recover method.
-- It should behave the same in the internal and external interperter.
-- In the past, they have diverged, and the external interpreter would roll back the state of putQ/getQ whereas the internal interpreter would not.
module Main where

import Language.Haskell.TH.Syntax
main = print $(putQ "0" >> recover (pure ()) (putQ "42" >> fail "oops")  >> getQ @String >>= lift )
