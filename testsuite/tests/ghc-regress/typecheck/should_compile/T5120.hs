{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Test where

class C t where
    type TF t
    ttt :: TF t -> t

b :: (C t, ?x :: TF t) => t
b = ttt ?x 
