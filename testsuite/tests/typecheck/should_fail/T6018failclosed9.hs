{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T6018failclosed9 where

type family E2 (a :: Bool) = r | r -> a where
  E2 False = True
  E2 True  = False
  E2 a     = False
