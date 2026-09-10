{-# LANGUAGE TypeData #-}
module T27732c where

type data T a = Cons {-# UNPACK #-} a
