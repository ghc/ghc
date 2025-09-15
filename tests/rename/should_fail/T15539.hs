{-# LANGUAGE RankNTypes #-}
module T15539 where

foo :: String
foo = show a
  where a = baz

-- We get top level constraints
--     Show a
--     forall . <not in scope baz>
-- We want the insoluble non-in-scope error to suppress
-- the Show a, just as it does if the whole things is nested

bar :: Int
bar = 1

bam = putStrLn foo
