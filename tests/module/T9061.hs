{-# OPTIONS_GHC -fwarn-unused-imports #-}
module T9061 where

import Prelude hiding (log)

f = log where log = ()
