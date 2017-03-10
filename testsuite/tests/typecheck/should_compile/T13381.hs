{-# OPTIONS_GHC -O #-}   -- To switch on the RULE
{-# LANGUAGE RankNTypes #-}

module T13381 where

data Exp a = Exp

fromExp :: Exp a -> a
fromExp _ = error "Impossible"

newtype Iter a b = Iter { getIter :: forall r. (a -> r) -> (b -> r) -> r }

iterLoop :: (a -> Iter a b) -> a -> b
iterLoop f x = error "urk"


  -- This rewrite rule results in a GHC panic: "opt_univ fell into a hole" on GHC 8.0.1, 8.0.2, and 8.1.
{-# RULES "fromExp-into-iterLoop" [~]
    forall (f :: Int -> Iter (Exp Int) (Exp Char))
           (init :: Int).
    fromExp (iterLoop f init) = error "urk"
  #-}
