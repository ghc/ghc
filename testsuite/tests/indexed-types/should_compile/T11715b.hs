
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T11715b where

import Data.Kind ( Constraint )

type Id :: ( k -> Constraint ) -> l -> Constraint
type family   Id a
type instance Id @k @k f = f

type T = Id Eq (Eq Bool)
-- this should remain stuck, instead of
-- reducing to the ill-kinded `Eq (Eq Bool)`
-- The type family equation for Id should not apply,
-- as this application is of the form `Id @Type @Constraint`
-- whereas the type family axiom is of the form `Id @k @k`.
