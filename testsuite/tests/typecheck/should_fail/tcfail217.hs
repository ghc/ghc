-- Test we can't hide cycles with type synonyms
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, UndecidableInstances #-}
module TcFail where

type Aish = A

class cls (Aish cls) => A cls c where
