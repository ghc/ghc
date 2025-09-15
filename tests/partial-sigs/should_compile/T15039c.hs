{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module T15039c where

import Data.Coerce
import Data.Kind
import Data.Type.Coercion
import Data.Type.Equality

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

ex1 :: Dict ((a :: Type) ~ (b :: Type)) -> ()
ex1 (Dict :: _) = ()

ex2 :: Dict ((a :: Type) ~~ (b :: Type)) -> ()
ex2 (Dict :: _) = ()

ex3 :: Dict ((a :: Type) ~~ (b :: k)) -> ()
ex3 (Dict :: _) = ()

-- Don't know how to make GHC print an unlifted, nominal equality in an error
-- message.
--
-- ex4, ex5 :: ???

ex6 :: Dict (Coercible (a :: Type) (b :: Type)) -> ()
ex6 (Dict :: _) = ()

ex7 :: _ => Coercion (a :: Type) (b :: Type)
ex7 = Coercion

-- Don't know how to make GHC print an unlifted, heterogeneous,
-- representational equality in an error message.
--
-- ex8 :: ???
