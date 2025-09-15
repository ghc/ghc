{-# LANGUAGE DeriveLift #-}
module T20688 where

import Language.Haskell.TH.Syntax

data Foo = Foo Int Bool
  deriving (Lift, Show)
