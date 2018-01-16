{-# LANGUAGE TemplateHaskell #-}
module T1541 where

$( [d|  infixr 3 +++
        (+++) :: Int -> Bool -> Bool
        (+++) x y = error "ruk"
   |])

-- This definition will only typecheck if the
-- the fixity of (+++) is infixr
foo p q r = p +++ q +++ r
