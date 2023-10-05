{-# LANGUAGE TemplateHaskell #-}

module T9209 where

foo = let $( [d| x = 5 |] ) in x
