{-# LANGUAGE GADTs #-}
{-# OPTIONS -haddock -ddump-parsed-ast #-}

-- Haddock comments in this test case should all be rejected, but they are not.
--
-- This is a known issue. Users should avoid writing comments in such
-- positions, as a future fix will disallow them.
--
-- See Note [Register keyword location] in GHC.Parser.PostProcess.Haddock

module
  -- | Bad comment for the module
  T17544_kw where

data Foo -- | Bad comment for MkFoo
  where MkFoo :: Foo

newtype Bar -- | Bad comment for MkBar
  where MkBar :: () -> Bar

class Cls a
    -- | Bad comment for clsmethod
  where
    clsmethod :: a
