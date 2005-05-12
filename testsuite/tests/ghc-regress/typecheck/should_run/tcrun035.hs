{-# OPTIONS -fglasgow-exts #-}

-- Tests subsumption for infix operators (in this case (.))
-- Broke GHC 6.4!

module Main(main) where

foo :: (forall m. Monad m => m a) -> IO a
foo = id . id

main :: IO ()
main = foo (return ())
