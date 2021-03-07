{-# LANGUAGE Haskell2010 #-}
-- | This module tests the ‘@since …’ annotation.
--
-- @since 1.2.3
module Bug26 where

-- | Foo
--
-- @since 2.10.7
--
-- @since 2.10.8
f :: ()
f = ()

-- | Bar
g :: ()
g = ()

-- | Class
--
-- @since 1.0
class C a where
  -- | @since 1.2.3
  c_f :: a

-- | instance for ()
--
-- @since 0.7.8
instance C () where
  c_f = ()
