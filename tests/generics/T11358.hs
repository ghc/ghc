{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import GHC.Generics

infixr 1 `T`
data T a = T a a deriving Generic
instance HasFixity (T a)

data I a = a `I` a deriving Generic
instance HasFixity (I a)

class HasFixity a where
  fixity :: a -> Fixity
  default fixity :: (Generic a, GHasFixity (Rep a)) => a -> Fixity
  fixity = gfixity . from

class GHasFixity f where
  gfixity :: f a -> Fixity

instance GHasFixity f => GHasFixity (D1 d f) where
  gfixity (M1 x) = gfixity x

instance Constructor c => GHasFixity (C1 c f) where
  gfixity c = conFixity c

main :: IO ()
main = do
  putStrLn $ show (fixity (T "a" "b")) ++ ", " ++ show (fixity ("a" `I` "b"))
