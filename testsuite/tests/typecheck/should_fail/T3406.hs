{-# LANGUAGE ScopedTypeVariables #-}

-- Trac #3406
-- A pattern signature that discards the bound variables

module T3406 where

type ItemColID a b = Int  -- Discards a,b

get :: ItemColID a b -> a -> ItemColID a b
get (x :: ItemColID a b) = x :: ItemColID a b