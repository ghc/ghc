{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlTestMonad
  ( ControlTestMonad(..)
  , Event(..)
  )
where

-- Defines observable events that can occur during a computation.
-- Each event is either an action or an observation.  If two
-- computations produce the same events, they are equivalent.

import GHC.Utils.Outputable

class (MonadFail m) => ControlTestMonad stmt expr m where
  evalPredicate :: expr -> m Bool
  evalEnum      :: expr -> (Integer,Integer) -> m Integer
                   -- ^ range is half-open: includes low end but not high
  takeAction    :: stmt -> m ()

data Event stmt expr = Action stmt
                     | Predicate expr Bool
                     | Switch expr (Integer,Integer) Integer
  deriving (Eq)

instance (Outputable e, Outputable s) => Outputable (Event e s) where
  ppr (Action l) = ppr l
  ppr (Predicate l b) = ppr l <+> parens (if b then "T" else "F")
  ppr (Switch l (lo,hi) i) =
      ppr l <+> parens (hcat [ text $ show i
                             , " in ["
                             , text $ show lo
                             , ".."
                             , text $ show hi
                             , "]"
                             ])

instance (Show e, Show s) => Show (Event e s) where
  show (Action l) = show l
  show (Predicate l b) = show l ++ "(" ++ (if b then "T" else "F") ++ ")"
  show (Switch l (lo,hi) i) =
      show l ++ "(" ++ show i ++ " in [" ++ show lo ++ ".." ++ show hi ++ "])"
