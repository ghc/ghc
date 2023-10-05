{-# LANGUAGE DataKinds #-}
module Main where

import Data.Typeable

data A = A

main = print $ typeRep (Proxy :: Proxy A) == typeRep (Proxy :: Proxy 'A)
