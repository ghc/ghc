{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module T15385 where

import Data.Type.Equality

data T a where
  TInt  :: T Int
  TBool :: T Bool

f1, f2 :: a :~: Int -> T a -> ()
f1 eq t
  | Refl <- eq
  = case t of
      TInt -> ()
f2 eq t
  = if |  Refl <- eq
       -> case t of
            TInt -> ()
