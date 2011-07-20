{-# OPTIONS_GHC -fwarn-name-shadowing -fwarn-unused-binds #-}

-- Trac #1972

module Temp where

import Data.List

data Data = Data {name :: String}

h :: a -> a
h name = name

f mapAccumL x = x

mapAccumL y = y

test x = a+b 
  where
    (a,b,c) = x