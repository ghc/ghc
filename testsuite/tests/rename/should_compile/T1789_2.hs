{-# OPTIONS_GHC -fwarn-missing-import-lists #-}

-- Test Trac #1789
module T1789_2 where

import Data.Map (size)
import Data.Maybe (Maybe(Just, Nothing))
import qualified Data.Set as Set (insert)
