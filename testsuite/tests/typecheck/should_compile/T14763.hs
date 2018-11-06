{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module T14763 where

import Data.Kind (Type)

data Value a = Value a

data SomeValue expr where
  SomeValue :: Esqueleto query expr backend => expr (Value a) -> SomeValue expr

class Esqueleto (query :: Type -> Type) (expr :: Type -> Type) backend
        | query -> expr backend, expr -> query backend

data SqlQuery a

data SqlBackend

data SqlExpr a where
  ECompositeKey :: SqlExpr (Value a)

instance Esqueleto SqlQuery SqlExpr SqlBackend

match' :: SomeValue SqlExpr -> a
match' (SomeValue ECompositeKey) = undefined

-- This is tricky becauuse we get a Given constraint
--    [G] Esqueleto query SqlExpr backend
-- where query and backend are existential.
-- Then fundeps with the top-level instance specify
--    [D] query   ~ SqlQuery
--    [D] backend ~ SqlBackend
-- And that is not an error!
-- (Nor can we exploit it, though.)
