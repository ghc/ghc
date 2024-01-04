-- Running this program should result in two calls to overloaded functions: One
-- with the $fShowX dictionary, the next with the $fShowList dictionary
-- constructor for X.
--
-- Note that although the `$fShowList` dictionary constructor is itself
-- overloaded, it should not get an SCC since we avoid instrumenting overloaded
-- calls that result in dictionaries.
--
-- With just -fprof-late-overloaded, only `invoke` should get an SCC, since it
-- is the only overloaded top level binding. With
-- `-fprof-late-overloaded-calls`, the calls to both `invoke` and `f` (in the
-- body of invoke) should get SCCs.

module Main where

{-# NOINLINE invoke #-}
invoke :: Show a => (Show [a] => [a] -> String) -> a -> String
invoke f x = f [x]

data X = X
    deriving Show

main :: IO ()
main = putStrLn (invoke show X)
