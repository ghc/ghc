{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module AdvanceTypes where

data Pattern :: [*] -> * where
  Nil :: Pattern '[]
  Cons :: Maybe h -> Pattern t -> Pattern (h ': t)
