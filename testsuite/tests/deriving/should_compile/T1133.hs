
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T1133 where

import T1133a

newtype X = X Int deriving Enum
