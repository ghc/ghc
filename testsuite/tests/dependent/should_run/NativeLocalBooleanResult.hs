{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeLocalBooleanResult
#ifdef ABSTRACT
    ( Index(..), Syntax, Step(..), substitute )
#else
    ( Index(..), Syntax(..), Step(..), substitute )
#endif
  where

data Index = Z | S Index
data Syntax = Var Index | Done | Nest Syntax

same :: Index -> Index -> Bool
same Z Z = True
same Z (S _) = False
same (S _) Z = False
same (S x) (S y) = same x y

#ifdef RULE
{-# RULES "skip name comparison" forall x y. same x y = False #-}
#endif

substitute :: Syntax -> Index -> Syntax -> Syntax
substitute (Var x) y replacement = case same x y of
    True -> replacement
    False -> Var x
#ifndef PARTIAL
substitute Done _ _ = Done
#endif
#ifdef NONDECREASING
substitute (Nest term) y replacement = Nest (substitute (Nest term) y replacement)
#else
substitute (Nest term) y replacement = Nest (substitute term y replacement)
#endif

data Step (name :: Index) (body :: Syntax) (argument :: Syntax) (result :: Syntax) where
    Reduce :: Step x t v (substitute t x v)
    Unfold :: forall x t. Step x t (Nest t) (substitute t x (Nest t))
