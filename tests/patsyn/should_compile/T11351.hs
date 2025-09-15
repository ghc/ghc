{-# LANGUAGE PatternSynonyms, TypeApplications, ScopedTypeVariables, ViewPatterns,
             AllowAmbiguousTypes #-}

module T11351 where

import GHC.TypeLits
import Data.Proxy

symbol :: forall s. KnownSymbol s => String
symbol = symbolVal @s Proxy

-- Not in scope: type variable ‘s’
-- Not in scope: type variable ‘s’
pattern Symbol :: forall s. KnownSymbol s => String
pattern Symbol <- ((== symbol @s) -> True) where
         Symbol = symbol @s

-- • Could not deduce (KnownSymbol n0)
--     arising from a use of ‘symbolVal’
--   from the context: KnownSymbol s
--     bound by the type signature for pattern synonym ‘Symbol’:
--                String
pattern Symbol2 :: forall s. KnownSymbol s => String
pattern Symbol2 <- ((== symbolVal (Proxy :: Proxy s)) -> True)
