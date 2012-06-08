{-# LANGUAGE UndecidableInstances #-}

module ShouldFail where

-- The constraint in the context of an instance declaration
-- is ambiguous, but we have UndecidableInstances on, so
-- it could conceivably be ok (we'd need OverlappingInstances
-- too in this case).  So we allow it.
-- See Note [The ambiguity check for type signatures] in TcMType

class Bar a
instance Bar a => Bar Bool

