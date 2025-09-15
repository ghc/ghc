{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Err where

import GHC.Exts (Constraint)

class Determines b | -> b

class (forall (b :: *) (c :: Constraint). (Determines b, Determines c) => c) => OpCode

instance OpCode
