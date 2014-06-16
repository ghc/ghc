{-# LANGUAGE RankNTypes #-}

-- These ones failed with 5.04.  They need a coercion
-- in the pattern matching compiler, so they are a bit
-- tricky.

-- GHC 6.3: these are back to failures, because we no longer do 
-- 	    type subsumption in pattern-matching

-- GHC 7.0: back to success

module ShouldCompile where

newtype Bug s a = Bug a

runBug :: (forall s. Bug s a) -> a
runBug (Bug _) = undefined

newtype BugN s a = BugN a

runBugN :: (forall s. BugN s a) -> a
runBugN (BugN _) = undefined

data Foo a b = Foo { foo :: a -> b }

baz :: String -> (forall a b . Foo a b) -> IO ()
baz s (Foo { foo = foo }) = putStrLn s
