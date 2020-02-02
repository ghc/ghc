{-# LANGUAGE Rank2Types #-}

-- Tests subsumption for infix operators (in this case (.))
-- Broke GHC 6.4!

-- Now it breaks the impredicativity story
--   (id {a}) . (id {a}) :: a -> a
-- And (forall m. Monad m => m a) /~ IO a
--
-- Apr 20: with simple subsumption this fails.  So I
-- I eta-expanded foo, but leaving the (id . id)
-- composition.

module Main(main) where

foo :: (forall m. Monad m => m a) -> IO a
foo x = (id . id) x

main :: IO ()
main = foo (return ())
