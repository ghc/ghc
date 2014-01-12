
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T1133 where

import {-# SOURCE #-} T1133

newtype X = X Int deriving Enum
