{-# LANGUAGE RankNTypes #-}

module ShouldCompile where

data Foo a b = Foo { foo :: a -> b }

-- kind error in here
bar :: String -> (forall a . Foo a) -> IO ()
bar s _ = putStrLn s
