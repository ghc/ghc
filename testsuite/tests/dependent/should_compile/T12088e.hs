{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Test that data declarations (not just instances) are retried.
module T12088e where

import Data.Kind

type family K a where K Type = Type   -- (K)

type family G             -- (G)
type instance G = F       -- (G:inst)

data T b = T (b :: K G)   -- (T)

type F = Type             -- (F)

data X = MkX (T X)        -- (X)

-- The declaration `data T` needs to be retried.
--
-- Dependency analysis produces this topologically sorted list of SCCs:
--   K, G, T, F, X, G:inst
-- But it does not account for the non-lexical dependency G:inst <- T.
--
-- The correct order of kind checking is this:
--   K, G, F, G:inst, T, X
-- We only discover it after the first attempt to check T fails.
