{-# OPTIONS_GHC -fwarn-missing-import-lists #-}

-- Test Trac #1789
module T1789 where

import Prelude
import Data.Map
import Data.Map (size)
import Data.Maybe (Maybe(..))
import Data.Maybe hiding (isJust)
import qualified Data.Set as Set