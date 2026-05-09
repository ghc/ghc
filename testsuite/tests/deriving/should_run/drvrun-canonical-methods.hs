module Main where

import Control.Monad (ap)
import Debug.Trace (trace)

newtype S = S String deriving (Eq, Monoid)

instance Semigroup S where
  S x <> S y = S (y <> x)

newtype F a = F { unF :: [a] } deriving (Eq, Functor, Monad)

instance Applicative F where
  pure a = trace "pure was called" $ F [a]
  (<*>) = ap

main = do
  let x = S "x"
  let y = S "y"
  print $ x `mappend` y == x <> y
  unF (return ()) `seq` putStrLn "Done"
