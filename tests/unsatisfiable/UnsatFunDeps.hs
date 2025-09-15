{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module UnsatFunDeps where

import GHC.TypeError

class C a b | a -> b
instance Unsatisfiable (Text "No") => C a b
