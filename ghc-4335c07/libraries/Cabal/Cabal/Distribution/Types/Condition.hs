{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Condition (
    Condition(..),
    cNot,
    cAnd,
    cOr,
    simplifyCondition,
) where

import Prelude ()
import Distribution.Compat.Prelude

-- | A boolean expression parameterized over the variable type used.
data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving (Show, Eq, Typeable, Data, Generic)

-- | Boolean negation of a 'Condition' value.
cNot :: Condition a -> Condition a
cNot (Lit b)  = Lit (not b)
cNot (CNot c) = c
cNot c        = CNot c

-- | Boolean AND of two 'Condtion' values.
cAnd :: Condition a -> Condition a -> Condition a
cAnd (Lit False) _           = Lit False
cAnd _           (Lit False) = Lit False
cAnd (Lit True)  x           = x
cAnd x           (Lit True)  = x
cAnd x           y           = CAnd x y

-- | Boolean OR of two 'Condition' values.
cOr :: Eq v => Condition v -> Condition v -> Condition v
cOr  (Lit True)  _           = Lit True
cOr  _           (Lit True)  = Lit True
cOr  (Lit False) x           = x
cOr  x           (Lit False) = x
cOr  c           (CNot d)
  | c == d                   = Lit True
cOr  (CNot c)    d
  | c == d                   = Lit True
cOr  x           y           = COr x y

instance Functor Condition where
  f `fmap` Var c    = Var (f c)
  _ `fmap` Lit c    = Lit c
  f `fmap` CNot c   = CNot (fmap f c)
  f `fmap` COr c d  = COr  (fmap f c) (fmap f d)
  f `fmap` CAnd c d = CAnd (fmap f c) (fmap f d)

instance Foldable Condition where
  f `foldMap` Var c    = f c
  _ `foldMap` Lit _    = mempty
  f `foldMap` CNot c   = foldMap f c
  f `foldMap` COr c d  = foldMap f c `mappend` foldMap f d
  f `foldMap` CAnd c d = foldMap f c `mappend` foldMap f d

instance Traversable Condition where
  f `traverse` Var c    = Var `fmap` f c
  _ `traverse` Lit c    = pure $ Lit c
  f `traverse` CNot c   = CNot `fmap` traverse f c
  f `traverse` COr c d  = COr  `fmap` traverse f c <*> traverse f d
  f `traverse` CAnd c d = CAnd `fmap` traverse f c <*> traverse f d

instance Applicative Condition where
  pure  = Var
  (<*>) = ap

instance Monad Condition where
  return = pure
  -- Terminating cases
  (>>=) (Lit x) _ = Lit x
  (>>=) (Var x) f = f x
  -- Recursing cases
  (>>=) (CNot  x  ) f = CNot (x >>= f)
  (>>=) (COr   x y) f = COr  (x >>= f) (y >>= f)
  (>>=) (CAnd  x y) f = CAnd (x >>= f) (y >>= f)

instance Monoid (Condition a) where
  mempty = Lit False
  mappend = (<>)

instance Semigroup (Condition a) where
  (<>) = COr

instance Alternative Condition where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Condition where
  mzero = mempty
  mplus = mappend

instance Binary c => Binary (Condition c)

-- | Simplify the condition and return its free variables.
simplifyCondition :: Condition c
                  -> (c -> Either d Bool)   -- ^ (partial) variable assignment
                  -> (Condition d, [d])
simplifyCondition cond i = fv . walk $ cond
  where
    walk cnd = case cnd of
      Var v   -> either Var Lit (i v)
      Lit b   -> Lit b
      CNot c  -> case walk c of
                   Lit True -> Lit False
                   Lit False -> Lit True
                   c' -> CNot c'
      COr c d -> case (walk c, walk d) of
                   (Lit False, d') -> d'
                   (Lit True, _)   -> Lit True
                   (c', Lit False) -> c'
                   (_, Lit True)   -> Lit True
                   (c',d')         -> COr c' d'
      CAnd c d -> case (walk c, walk d) of
                    (Lit False, _) -> Lit False
                    (Lit True, d') -> d'
                    (_, Lit False) -> Lit False
                    (c', Lit True) -> c'
                    (c',d')        -> CAnd c' d'
    -- gather free vars
    fv c = (c, fv' c)
    fv' c = case c of
      Var v     -> [v]
      Lit _      -> []
      CNot c'    -> fv' c'
      COr c1 c2  -> fv' c1 ++ fv' c2
      CAnd c1 c2 -> fv' c1 ++ fv' c2
