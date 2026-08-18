{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
module Callee where

import Ty
import Mid

instance CB a => CB (T a) where
  opB _ = 2

instance CB Int where
  opB _ = 8
