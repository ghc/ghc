{-# LANGUAGE TypeData #-}
module TDRecordsH98 where

type data Record a = Cons { field :: a }
