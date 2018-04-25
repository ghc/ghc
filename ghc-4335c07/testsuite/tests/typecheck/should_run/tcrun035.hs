{-# LANGUAGE Rank2Types #-}

-- Tests subsumption for infix operators (in this case (.))
-- Broke GHC 6.4!

-- Now it breaks the impredicativity story
--   (id {a}) . (id {a}) :: a -> a
-- And (forall m. Monad m => m a) /~ IO a

module Main(main) where

foo :: (forall m. Monad m => m a) -> IO a
foo = id . id

main :: IO ()
main = foo (return ())
