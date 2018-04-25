
{-# LANGUAGE Arrows #-}

module T5380 where

testB :: not_bool -> (() -> ()) -> () -> not_unit
testB b f = proc () -> if b then f -< () else f -< ()
