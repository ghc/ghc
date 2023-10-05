
module T7453 where

newtype Id a = Id { runId :: a }

-- cast1 :: a -> b
cast1 v = runId z
    where z :: Id t
          z = aux
              where aux = Id v

-- cast2 :: a -> b
cast2 v = z ()
    where z :: () -> t
          z = aux
              where aux = const v

-- cast3 :: a -> b
cast3 v = z
    where z :: t
          z = v
              where aux = const v

cast1' :: a -> b
cast1' = cast1

cast2' :: a -> b
cast2' = cast2

cast3' :: a -> b
cast3' = cast3

