{-# LANGUAGE TypeData #-}
module T27732d where

type data T a = Cons {-# NOUNPACK #-} a
