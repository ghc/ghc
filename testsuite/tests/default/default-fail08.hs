-- | Default for a partially applied Coercible constraint doesn't trigger panic

{-# LANGUAGE Haskell2010, ConstraintKinds, MultiParamTypeClasses, NamedDefaults #-}

import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (Identity))

type CoercibleFromInt = Coercible Int

default CoercibleFromInt (Identity Int)

main = print (coerce (4 :: Int))
