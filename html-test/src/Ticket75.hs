{-# LANGUAGE TypeOperators #-}
module Ticket75 where

data a :- b = Q

-- | A reference to ':-'
f :: Int
f = undefined
