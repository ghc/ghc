{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls, TypeOperators #-}

module Test where

import Data.Kind (Type)

data Zero
data Succ (a :: Type)

data FZ
data FS (fn :: Type)

data Fin n fn where
  FZ :: Fin (Succ n) FZ
  FS :: Fin n fn -> Fin (Succ n) (FS fn)

data Nil
data a ::: b

type family Lookup ts fn :: Type
type instance Lookup ((t :: Type) ::: ts) FZ = t
type instance Lookup (t ::: (ts :: Type)) (FS fn) = Lookup ts fn

data Tuple n ts where
  Nil   :: Tuple Zero Nil
  (:::) :: t -> Tuple n ts -> Tuple (Succ n) (t ::: ts)

proj :: Fin n fn -> Tuple n ts -> Lookup ts fn
proj FZ      (v ::: _)  = v
proj (FS fn) (_ ::: vs) = proj fn vs
