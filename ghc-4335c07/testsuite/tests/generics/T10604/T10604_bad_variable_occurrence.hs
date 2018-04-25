{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
module T10604_bad_variable_occurrence where

import GHC.Generics

newtype Fix f = In { out :: f (Fix f) }
  deriving Generic1
