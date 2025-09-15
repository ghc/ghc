{-# LANGUAGE TemplateHaskell #-}
module TTT where

a :: ()
a = let () = () in ()

b :: ()
b = let $([p|()|]) = () in ()

