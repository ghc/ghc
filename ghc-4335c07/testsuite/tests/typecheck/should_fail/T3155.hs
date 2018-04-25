{-# LANGUAGE GADTs, TypeOperators #-}
module T3155 where

-- Test Trac #3155
-- Gave bad error message in GHC 6.10

data Any s where
  Any :: s ix -> ix -> Any s

data AnyR s r where
  AnyR :: s ix -> r ix -> AnyR s r

unR :: (forall ix. r ix -> ix) -> AnyR s r -> Any s
unR f (AnyR ix rix) = Any ix (f rix)
