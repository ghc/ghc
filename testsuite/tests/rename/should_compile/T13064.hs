{-# OPTIONS_GHC -Wunused-imports #-}
module T13064 where

import Control.Applicative
import Prelude (IO, pure)   -- Import of 'pure' is redundant

foo :: IO ()
foo = () <$ pure ()
