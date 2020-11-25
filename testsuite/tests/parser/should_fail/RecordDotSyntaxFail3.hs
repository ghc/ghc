{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, ScopedTypeVariables, PolyKinds, TypeApplications, DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordDotSyntax #-}

class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)

getField :: forall x r a . HasField x r a => r -> a
getField = snd . hasField @x -- Note: a.x = is getField @"x" a.

setField :: forall x r a . HasField x r a => r -> a -> r
setField = fst . hasField @x -- Note : a{x = b} is setField @"x" a b.

-- 'Corge' has a '&&&' field of type 'Int'
data Corge = Corge { (&&&) :: Int } deriving (Show, Eq)
instance HasField "&&&" Corge Int where
    hasField r = (\x -> case r of Corge { .. } -> Corge { (&&&) = x, .. }, (&&&) r)

main = do
  let b = Corge { (&&&) = 12 };
  print $ (b.(&&&))
   -- Syntax error: Dot notation is not available for fields with
   -- operator names
