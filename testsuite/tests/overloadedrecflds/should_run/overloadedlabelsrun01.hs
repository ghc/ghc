-- Basic tests of overloaded labels

{-# LANGUAGE OverloadedLabels
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , NoMonomorphismRestriction
  #-}

import GHC.OverloadedLabels

instance IsLabel "true" Bool where
  fromLabel _ = True

instance IsLabel "false" Bool where
  fromLabel _ = False

a :: IsLabel "true" t => t
a = #true

b = #false

c :: Bool
c = #true

main = do print (a :: Bool)
          print (b :: Bool)
          print c
