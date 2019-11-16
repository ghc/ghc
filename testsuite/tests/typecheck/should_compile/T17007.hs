{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T17007 where

type ItemColID a b = Int  -- Discards a,b

get :: ItemColID a b -> ItemColID a b
get (x :: ItemColID a b) = x :: ItemColID a b

type family ItemColID' a b where ItemColID' a b = Int  -- Discards a,b

get' :: ItemColID' a b -> ItemColID' a b
get' (x :: ItemColID' a b) = x :: ItemColID' a b
