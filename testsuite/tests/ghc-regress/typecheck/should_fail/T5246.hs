{-# LANGUAGE ImplicitParams #-}

-- Produced a duplicated error message in 7.0

module T5246 where

foo :: (?x :: Int) => a
foo = undefined

bar = let ?x = "hello"
      in foo
