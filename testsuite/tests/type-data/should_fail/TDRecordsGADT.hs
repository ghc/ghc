{-# LANGUAGE TypeData #-}
{-# LANGUAGE GADTs #-}
module TDRecordsGADT where

type data Record a where
    Cons :: { field :: a } -> Record a
