{-# LANGUAGE TypeData #-}
module TDMultiple01 where

data P = MkP
type data Prom = P -- P is multiply defined
