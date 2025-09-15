{-# LANGUAGE FlexibleContexts #-}
module T17955 where

import Data.Coerce

newtype T = Coercible () T => T ()
