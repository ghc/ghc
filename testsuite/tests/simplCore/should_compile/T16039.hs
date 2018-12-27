{-# language MultiParamTypeClasses #-}

module T16039 (T16039(..)) where

import GHC.Magic (noinline)

foo = 7 + 35

class T16039 where
  method :: Integer

instance T16039 where
  method = noinline foo
