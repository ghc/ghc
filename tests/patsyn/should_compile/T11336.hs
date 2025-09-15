{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Bug where

data AST s a
  where
    Sym  :: s a -> AST s a
    (:$) :: AST s (a -> b) -> AST s a -> AST s b

data A sig where A :: A (a -> a)
data B sig where B :: B (a -> a)

data AB a
  where
    AA :: A a -> AB a
    BB :: B a -> AB a

class Prj s where prj :: AB a -> Maybe (s a)

instance Prj A
  where
    prj (AA s) = Just s
    prj _      = Nothing

instance Prj B
  where
    prj (BB s) = Just s
    prj _      = Nothing

pattern SymP s <- Sym (prj -> Just s)

fun :: AST AB a -> AST AB a
fun (SymP A :$ _) = undefined
fun (SymP B :$ _) = undefined
