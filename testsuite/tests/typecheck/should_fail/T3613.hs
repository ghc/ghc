-- c.f Trac #3613

module T3613 where

import Control.Monad

foo :: Maybe ()
foo = return ()

bar :: IO ()
bar = return ()

fun1 = let fooThen m = foo>> m
       in fooThen (bar>> undefined)

fun2 = let fooThen m = foo>> m
       in fooThen (do {bar; undefined})


