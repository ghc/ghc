{-# LANGUAGE TypeData #-}
module TDMultiple02 where

type data P = MkP
type data Prom = P -- type P is multiply defined
