{-# LANGUAGE Haskell2010 #-}
module NamespacedIdentifiers where

-- | A link to:
--
--   * the type t'Bar'
--   * the constructor v'Bar'
--   * the unimported but qualified type t'A.A'
--   * the unimported but qualified value v'A.A'
--
data Foo = Bar

-- | A link to the value v'Foo' (which shouldn't exist).
data Bar
