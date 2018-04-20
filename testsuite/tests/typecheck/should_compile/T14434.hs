{-# LANGUAGE MonoLocalBinds, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS -fsolve-constant-dicts #-}

module T14434 where

class ToString a where
  toString :: a -> String

-- | This instance is used in original code as hack
-- to simplify code generation
instance {-# OVERLAPPABLE #-} ToString a where
  toString _ = "Catchall attribute value"

toStringX :: (ToString a) => a -> String
toStringX = toString
  -- Here we do /not/ want to solve the ToString
  -- constraint with the local instance
