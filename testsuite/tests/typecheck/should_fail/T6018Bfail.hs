{-# LANGUAGE TypeFamilyDependencies #-}

module T6018Bfail where

type family H a b c = (result :: *) | result -> a b c
