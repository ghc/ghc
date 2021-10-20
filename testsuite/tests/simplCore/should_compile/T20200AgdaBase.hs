module T20200AgdaBase where

data QName = QName
data Definition = D

class Monad m => HasConstInfo m where
  getConstInfo :: QName -> m Definition

{-# SPECIALIZE getConstInfo :: QName -> IO Definition #-}

instance HasConstInfo IO where
  getConstInfo = undefined
