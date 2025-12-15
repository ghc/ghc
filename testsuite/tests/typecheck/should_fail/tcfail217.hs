-- Test we can't hide cycles with type synonyms
{-# LANGUAGE UndecidableInstances #-}
module TcFail where

type Aish = A

class Aish a => A a where
