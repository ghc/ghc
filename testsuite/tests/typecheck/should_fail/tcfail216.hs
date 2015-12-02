{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, UndecidableInstances #-}
module TcFail where

-- Currently (Dec 15) we a type variable
-- in the head of a superclass constraint,
-- and do not worry about recursive uses in
-- the args of superclass constraints
class cls (A cls) => A cls c where
