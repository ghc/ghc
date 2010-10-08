{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Tests newtype deriving with
-- a non-type constructor in the representation

module Main where

newtype Wrap m a = Wrap { unWrap :: m a } 
    deriving (Monad, Eq)

foo :: Int -> Wrap IO a -> Wrap IO ()
foo 0 a = return ()
foo n a = do { a; foo (n-1) a }

main = do { unWrap (foo 3 (Wrap (putChar 'x'))); putChar '\n' }
