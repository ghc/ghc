{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T23171 where

import Data.Kind

type C1 :: Type -> Type -> Constraint
class C1 t m where

type C2 :: Type -> Constraint
class C2 a where

type C3 :: Type -> Constraint
class C2 a => C3 a where

type D :: Type -> Constraint
class D t where
instance (forall m. C3 m => C1 t m) => D t where

type T :: Type -> Type
type family T a where

try :: forall (e :: Type). D (T e) => e -> ()
try _ = ()

type C1T :: Type -> Type -> Constraint
class C1 (T e) m => C1T e m

tried :: forall (e :: Type). (forall m. C1T e m) => e -> ()
tried = try @e

-- From the call to "try", we get [W] D (T e).
-- After using the instance for D, we get the QC [G] C3 m ==> [W] C1 (T e) m.
--
-- The Given "[G] C3 m" thus arises from superclass expansion
-- from "D (T e)", which contains a type family application, T.
-- So the logic in 'mkStrictSuperClasses' better be able to handle that when
-- expanding the superclasses of C3 (in this case, C2); in particular
-- ltPatersonSize needs to handle a type family in its second argument.

