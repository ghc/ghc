{-# LANGUAGE RankNTypes, MonoLocalBinds #-}

module T13804 where

f periph = let sr :: forall a. [a] -> [a]
               sr = if periph then reverse else id

               sr2 = sr
               -- The question: is sr2 generalised?
               -- It should be, because sr has a type sig
               -- even though it has periph free
           in
           (sr2 [True], sr2 "c")
