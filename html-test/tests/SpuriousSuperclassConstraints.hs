{-# LANGUAGE EmptyDataDecls, KindSignatures #-}
-- |
-- What is tested here:
--
-- Due to a change in GHC 7.6.1 we had a bug that superclass contraints were
-- included in the instances list.  Edward K. repported it here:
--
--   <http://www.haskell.org/pipermail/haskell-cafe/2012-September/103600.html>
--
-- And here is the corresponding theard on glasgow-haskell-users:
--
--   <http://www.haskell.org/pipermail/glasgow-haskell-users/2012-September/022914.html>
--
-- It has been fixed in:
--
-- > 6ccf78e15a525282fef61bc4f58a279aa9c21771
-- > Fix spurious superclass constraints bug.
--
module SpuriousSuperclassConstraints where

import Control.Applicative

data SomeType (f :: * -> *) a

instance Functor (SomeType f) where
  fmap = undefined

instance Applicative f => Applicative (SomeType f) where
  pure   = undefined
  (<*>)  = undefined
