{-# LANGUAGE DataKinds #-}

module T8671adefs where

import GHC.TypeLits

-- Not actually a useful thing, but illustrates the point
data NotMonad (t :: Nat)

incr :: NotMonad 1
incr = undefined

(>>) :: NotMonad s -> NotMonad t -> NotMonad (s + t)
(>>) = undefined
