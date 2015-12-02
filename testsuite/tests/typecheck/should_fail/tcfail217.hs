-- Test we can't hide cycles with type synonyms
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, UndecidableInstances #-}
module TcFail where

type Aish = A

-- Currently (Dec 15) we a type variable
-- in the head of a superclass constraint,
-- and do not worry about recursive uses in
-- the args of superclass constraints
class cls (Aish cls) => A cls c where
