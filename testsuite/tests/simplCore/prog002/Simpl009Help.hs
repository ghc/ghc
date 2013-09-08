{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- Helper for simpl009.hs (see comments there)

module Simpl009Help where
  
import Control.Applicative (Applicative(..), Alternative(empty, (<|>)))
import Control.Monad

newtype Parser s a
  = Parser (forall res . (a -> [String] -> P s res) -> [String] -> P s res)

data P s res
  = Symbol (s -> P s res)
  | Fail [String] [String]
  | Result res (P s res)

instance Functor (Parser s) where
    fmap = liftM

instance Applicative (Parser s) where
    pure = return
    (<*>) = ap

instance Monad (Parser s) where
  return a = Parser (\fut -> fut a)
  
  Parser f >>= k =
    Parser (\fut -> f (\a -> let Parser g = k a in g fut))

  fail s =
    Parser (\fut exp -> Fail exp [s])

instance Alternative (Parser s) where
    empty = mzero
    (<|>) = mplus

instance MonadPlus (Parser s) where
  mplus = error "urk"
  mzero = Parser (\fut exp -> Fail exp [])

lookAhead :: forall s. Parser s s
lookAhead =
  Parser (\fut exp -> Symbol (\c ->
    feed c (fut c [])
  ))
 where
  feed :: forall res. s -> P s res -> P s res
  feed c (Symbol sym)     = sym c
  feed c (Result res fut) = Result res (feed c fut)
  feed c p@(Fail _ _)     = p
