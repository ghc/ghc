{-# LANGUAGE TypeInType, GADTs #-}

module PromotedClass where

import Data.Proxy

data X a where
  MkX :: Show a => a -> X a

foo :: Proxy ('MkX 'True)
foo = Proxy
