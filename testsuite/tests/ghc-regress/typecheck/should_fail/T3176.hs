{-# LANGUAGE ExistentialQuantification #-}

-- Trac #3176

module Foo where

data ES = forall a. Show a => ES {unES:: a}

smallPrintES f t = show $ unES $ f t 
