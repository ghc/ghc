{-# OPTIONS -fglasgow-exts #-}


-- These ones failed with 5.04.  They need a coercion
-- in the pattern matching compiler, so they are a bit
-- tricky.

module ShouldCompile where

newtype Bug s a = Bug a

runBug :: (forall s. Bug s a) -> a
runBug (Bug _) = undefined

data Foo a b = Foo { foo :: a -> b }

foo :: String -> (forall a b . Foo a b) -> IO ()
foo s (Foo { foo = foo }) = putStrLn s
