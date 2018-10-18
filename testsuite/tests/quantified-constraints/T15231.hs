{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Bug where

import Data.Kind

data ECC :: Constraint -> Type -> Type

class Y a
class Z a

instance  c         => Y (ECC c a)
instance (c => Z a) => Z (ECC c a)
