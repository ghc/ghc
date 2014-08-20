module T9071a where

newtype Mu f = Mu (f (Mu f))

