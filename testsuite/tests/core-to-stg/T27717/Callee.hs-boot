{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Callee where

import Ty

instance CB a => CB (T a)
instance CB Int
