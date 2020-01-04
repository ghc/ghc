{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -fwarn-dodgy-imports #-}

module T7167 where

import Data.List hiding (foo)
