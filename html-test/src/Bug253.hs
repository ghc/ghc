{-# LANGUAGE Haskell2010 #-}
-- | This module tests that if we're trying to link to a /qualified/
-- identifier that's not in scope, we get an anchor as if it was a
-- variable. Previous behaviour was to treat it as a type constructor
-- so issue like #253 arose. Also see @rename@ function comments in
-- source.
module Bug253 where

-- | This link should generate @#v@ anchor: 'DoesNotExist.fakeFakeFake'
foo :: ()
foo = ()
