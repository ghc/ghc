{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -O #-}

module Gadt17_help (
      TernOp (..), applyTernOp
    ) where

data TypeWitness a   where
    TWInt    ::         TypeWitness Int
    TWBool   ::         TypeWitness Bool
    TWFloat  ::         TypeWitness Float
    TWDouble ::         TypeWitness Double

instance (Eq a) => Eq (TypeWitness a) where
  (==) TWInt     TWInt     = True
  (==) TWBool    TWBool    = True
  (==) TWFloat   TWFloat   = True
  (==) TWDouble  TWDouble  = True
  (==) _         _         = False

data TernOp a b c d where
  OpIf       ::                   TypeWitness a                    ->    TernOp Bool   a      a      a
  OpTernFunc ::  TypeWitness a -> TypeWitness b -> TypeWitness c
                            -> TypeWitness d -> (a -> b -> c -> d) ->    TernOp a      b      c      d

instance Show (TernOp a b c d) where
  show (OpIf       {})  = "OpIf"
  show (OpTernFunc {})  = "OpTernFunc <function>"


applyTernOp :: TernOp a b c d -> a -> b -> c -> d
applyTernOp (OpIf       {})         cond x y = if (cond) then x else y
applyTernOp (OpTernFunc _ _ _ _ f)  x    y z = f x y z

