-- Test we do get a cycle for superclasses escaping via a free tyvar
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, UndecidableInstances #-}
module TcFail where

class cls (A cls) => A cls c where
