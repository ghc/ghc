{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#include "bifunctors-common.h"

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.
----------------------------------------------------------------------------
module Data.Bifunctor.Joker
  ( Joker(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Classes

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
           , Typeable
#endif
           )

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 708
data JokerMetaData
data JokerMetaCons
data JokerMetaSel

instance Datatype JokerMetaData where
    datatypeName _ = "Joker"
    moduleName _ = "Data.Bifunctor.Joker"

instance Constructor JokerMetaCons where
    conName _ = "Joker"
    conIsRecord _ = True

instance Selector JokerMetaSel where
    selName _ = "runJoker"

instance Generic1 (Joker g a) where
    type Rep1 (Joker g a) = D1 JokerMetaData (C1 JokerMetaCons
        (S1 JokerMetaSel (Rec1 g)))
    from1 = M1 . M1 . M1 . Rec1 . runJoker
    to1 = Joker . unRec1 . unM1 . unM1 . unM1
#endif

#if LIFTED_FUNCTOR_CLASSES
instance Eq1 g => Eq1 (Joker g a) where
  liftEq g = eqJoker (liftEq g)
instance Eq1 g => Eq2 (Joker g) where
  liftEq2 _ g = eqJoker (liftEq g)

instance Ord1 g => Ord1 (Joker g a) where
  liftCompare g = compareJoker (liftCompare g)
instance Ord1 g => Ord2 (Joker g) where
  liftCompare2 _ g = compareJoker (liftCompare g)

instance Read1 g => Read1 (Joker g a) where
  liftReadsPrec rp rl = readsPrecJoker (liftReadsPrec rp rl)
instance Read1 g => Read2 (Joker g) where
  liftReadsPrec2 _ _ rp2 rl2 = readsPrecJoker (liftReadsPrec rp2 rl2)

instance Show1 g => Show1 (Joker g a) where
  liftShowsPrec sp sl = showsPrecJoker (liftShowsPrec sp sl)
instance Show1 g => Show2 (Joker g) where
  liftShowsPrec2 _ _ sp2 sl2 = showsPrecJoker (liftShowsPrec sp2 sl2)
#else
instance Eq1 g => Eq1 (Joker g a) where
  eq1 = eqJoker eq1

instance Ord1 g => Ord1 (Joker g a) where
  compare1 = compareJoker compare1

instance Read1 g => Read1 (Joker g a) where
  readsPrec1 = readsPrecJoker readsPrec1

instance Show1 g => Show1 (Joker g a) where
  showsPrec1 = showsPrecJoker showsPrec1
#endif

eqJoker :: (g b1 -> g b2 -> Bool)
        -> Joker g a1 b1 -> Joker g a2 b2 -> Bool
eqJoker eqB (Joker x) (Joker y) = eqB x y

compareJoker :: (g b1 -> g b2 -> Ordering)
             -> Joker g a1 b1 -> Joker g a2 b2 -> Ordering
compareJoker compareB (Joker x) (Joker y) = compareB x y

readsPrecJoker :: (Int -> ReadS (g b))
               -> Int -> ReadS (Joker g a b)
readsPrecJoker rpB p =
  readParen (p > 10) $ \s0 -> do
    ("Joker",    s1) <- lex s0
    ("{",        s2) <- lex s1
    ("runJoker", s3) <- lex s2
    (x,          s4) <- rpB 0 s3
    ("}",        s5) <- lex s4
    return (Joker x, s5)

showsPrecJoker :: (Int -> g b -> ShowS)
               -> Int -> Joker g a b -> ShowS
showsPrecJoker spB p (Joker x) =
  showParen (p > 10) $
      showString "Joker {runJoker = "
    . spB 0 x
    . showChar '}'

instance Functor g => Bifunctor (Joker g) where
  first _ = Joker . runJoker
  {-# INLINE first #-}
  second g = Joker . fmap g . runJoker
  {-# INLINE second #-}
  bimap _ g = Joker . fmap g . runJoker
  {-# INLINE bimap #-}

instance Functor g => Functor (Joker g a) where
  fmap g = Joker . fmap g . runJoker
  {-# INLINE fmap #-}

instance Applicative g => Biapplicative (Joker g) where
  bipure _ b = Joker (pure b)
  {-# INLINE bipure #-}

  Joker mf <<*>> Joker mx = Joker (mf <*> mx)
  {-# INLINE (<<*>>) #-}

instance Foldable g => Bifoldable (Joker g) where
  bifoldMap _ g = foldMap g . runJoker
  {-# INLINE bifoldMap #-}

instance Foldable g => Foldable (Joker g a) where
  foldMap g = foldMap g . runJoker
  {-# INLINE foldMap #-}

instance Traversable g => Bitraversable (Joker g) where
  bitraverse _ g = fmap Joker . traverse g . runJoker
  {-# INLINE bitraverse #-}

instance Traversable g => Traversable (Joker g a) where
  traverse g = fmap Joker . traverse g . runJoker
  {-# INLINE traverse #-}
