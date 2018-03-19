{-# LANGUAGE PolyKinds, TypeOperators, DataKinds, TypeFamilies, GADTs #-}

module T11984 where

data family Sing (a :: k)

data Schema = Sch [Bool]

data instance Sing (x :: Schema) where
  SSch :: Sing x -> Sing ('Sch x)

data instance Sing (x :: [k]) where
  SNil :: Sing '[]
  SCons :: Sing a -> Sing b -> Sing (a ': b)

data G a where
  GCons :: G ('Sch (a ': b))

eval :: G s -> Sing s -> ()
eval GCons s =
        case s of
          -- SSch SNil -> undefined
          SSch (SCons _ _) -> undefined
