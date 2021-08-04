{-# LANGUAGE RankNTypes, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Aliases
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (local universal quantification)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>.
-- The present module provides a number of declarations for typical generic
-- function types, corresponding type case, and others.
--
-----------------------------------------------------------------------------

module Data.Generics.Aliases (

        -- * Combinators to \"make\" generic functions via cast
        mkT, mkQ, mkM, mkMp, mkR,
        ext0, extT, extQ, extM, extMp, extB, extR,

        -- * Type synonyms for generic function types
        GenericT,
        GenericQ,
        GenericM,
        GenericB,
        GenericR,
        Generic,
        Generic'(..),
        GenericT'(..),
        GenericQ'(..),
        GenericM'(..),

        -- * Ingredients of generic functions
        orElse,

        -- * Function combinators on generic functions
        recoverMp,
        recoverQ,
        choiceMp,
        choiceQ,

        -- * Type extension for unary type constructors
        ext1,
        ext1T,
        ext1M,
        ext1Q,
        ext1R,
        ext1B,

        -- * Type extension for binary type constructors
        ext2T,
        ext2M,
        ext2Q,
        ext2R,
        ext2B

  ) where

#ifdef __HADDOCK__
import Prelude
#endif
import Control.Monad
import Data.Data

------------------------------------------------------------------------------
--
--      Combinators to "make" generic functions
--      We use type-safe cast in a number of ways to make generic functions.
--
------------------------------------------------------------------------------

-- | Make a generic transformation;
--   start from a type-specific case;
--   preserve the term otherwise
--
mkT :: ( Typeable a
       , Typeable b
       )
    => (b -> b)
    -> a
    -> a
mkT = extT id


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r


-- | Make a generic monadic transformation;
--   start from a type-specific case;
--   resort to return otherwise
--
mkM :: ( Monad m
       , Typeable a
       , Typeable b
       )
    => (b -> m b)
    -> a
    -> m a
mkM = extM return


{-

For the remaining definitions, we stick to a more concise style, i.e.,
we fold maybes with "maybe" instead of case ... of ..., and we also
use a point-free style whenever possible.

-}


-- | Make a generic monadic transformation for MonadPlus;
--   use \"const mzero\" (i.e., failure) instead of return as default.
--
mkMp :: ( MonadPlus m
        , Typeable a
        , Typeable b
        )
     => (b -> m b)
     -> a
     -> m a
mkMp = extM (const mzero)


-- | Make a generic builder;
--   start from a type-specific ase;
--   resort to no build (i.e., mzero) otherwise
--
mkR :: ( MonadPlus m
       , Typeable a
       , Typeable b
       )
    => m b -> m a
mkR f = mzero `extR` f


-- | Flexible type extension
ext0 :: (Typeable a, Typeable b) => c a -> c b -> c a
ext0 def ext = maybe def id (gcast ext)


-- | Extend a generic transformation by a type-specific case
extT :: ( Typeable a
        , Typeable b
        )
     => (a -> a)
     -> (b -> b)
     -> a
     -> a
extT def ext = unT ((T def) `ext0` (T ext))


-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)


-- | Extend a generic monadic transformation by a type-specific case
extM :: ( Monad m
        , Typeable a
        , Typeable b
        )
     => (a -> m a) -> (b -> m b) -> a -> m a
extM def ext = unM ((M def) `ext0` (M ext))


-- | Extend a generic MonadPlus transformation by a type-specific case
extMp :: ( MonadPlus m
         , Typeable a
         , Typeable b
         )
      => (a -> m a) -> (b -> m b) -> a -> m a
extMp = extM


-- | Extend a generic builder
extB :: ( Typeable a
        , Typeable b
        )
     => a -> b -> a
extB a = maybe a id . cast


-- | Extend a generic reader
extR :: ( Monad m
        , Typeable a
        , Typeable b
        )
     => m a -> m b -> m a
extR def ext = unR ((R def) `ext0` (R ext))



------------------------------------------------------------------------------
--
--      Type synonyms for generic function types
--
------------------------------------------------------------------------------


-- | Generic transformations,
--   i.e., take an \"a\" and return an \"a\"
--
type GenericT = forall a. Data a => a -> a


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Generic monadic transformations,
--   i.e., take an \"a\" and compute an \"a\"
--
type GenericM m = forall a. Data a => a -> m a


-- | Generic builders
--   i.e., produce an \"a\".
--
type GenericB = forall a. Data a => a


-- | Generic readers, say monadic builders,
--   i.e., produce an \"a\" with the help of a monad \"m\".
--
type GenericR m = forall a. Data a => m a


-- | The general scheme underlying generic functions
--   assumed by gfoldl; there are isomorphisms such as
--   GenericT = Generic T.
--
type Generic c = forall a. Data a => a -> c a


-- | Wrapped generic functions;
--   recall: [Generic c] would be legal but [Generic' c] not.
--
data Generic' c = Generic' { unGeneric' :: Generic c }


-- | Other first-class polymorphic wrappers
newtype GenericT'   = GT { unGT :: forall a. Data a => a -> a }
newtype GenericQ' r = GQ { unGQ :: GenericQ r }
newtype GenericM' m = GM { unGM :: forall a. Data a => a -> m a }


-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y


{-

The following variations take "orElse" to the function
level. Furthermore, we generalise from "Maybe" to any
"MonadPlus". This makes sense for monadic transformations and
queries. We say that the resulting combinators modell choice. We also
provide a prime example of choice, that is, recovery from failure. In
the case of transformations, we recover via return whereas for
queries a given constant is returned.

-}

-- | Choice for monadic transformations
choiceMp :: MonadPlus m => GenericM m -> GenericM m -> GenericM m
choiceMp f g x = f x `mplus` g x


-- | Choice for monadic queries
choiceQ :: MonadPlus m => GenericQ (m r) -> GenericQ (m r) -> GenericQ (m r)
choiceQ f g x = f x `mplus` g x


-- | Recover from the failure of monadic transformation by identity
recoverMp :: MonadPlus m => GenericM m -> GenericM m
recoverMp f = f `choiceMp` return


-- | Recover from the failure of monadic query by a constant
recoverQ :: MonadPlus m => r -> GenericQ (m r) -> GenericQ (m r)
recoverQ r f = f `choiceQ` const (return r)



------------------------------------------------------------------------------
--      Type extension for unary type constructors
------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
#define Typeable2 Typeable
#endif

-- | Flexible type extension
ext1 :: (Data a, Typeable1 t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)


-- | Type extension of transformations for unary type constructors
ext1T :: (Data d, Typeable1 t)
      => (forall e. Data e => e -> e)
      -> (forall f. Data f => t f -> t f)
      -> d -> d
ext1T def ext = unT ((T def) `ext1` (T ext))


-- | Type extension of monadic transformations for type constructors
ext1M :: (Monad m, Data d, Typeable1 t)
      => (forall e. Data e => e -> m e)
      -> (forall f. Data f => t f -> m (t f))
      -> d -> m d
ext1M def ext = unM ((M def) `ext1` (M ext))


-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable1 t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of readers for type constructors
ext1R :: (Monad m, Data d, Typeable1 t)
      => m d
      -> (forall e. Data e => m (t e))
      -> m d
ext1R def ext = unR ((R def) `ext1` (R ext))


-- | Type extension of builders for type constructors
ext1B :: (Data a, Typeable1 t)
      => a
      -> (forall b. Data b => (t b))
      -> a
ext1B def ext = unB ((B def) `ext1` (B ext))

------------------------------------------------------------------------------
--      Type extension for binary type constructors
------------------------------------------------------------------------------

-- | Flexible type extension
ext2 :: (Data a, Typeable2 t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)


-- | Type extension of transformations for unary type constructors
ext2T :: (Data d, Typeable2 t)
      => (forall e. Data e => e -> e)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> t d1 d2)
      -> d -> d
ext2T def ext = unT ((T def) `ext2` (T ext))


-- | Type extension of monadic transformations for type constructors
ext2M :: (Monad m, Data d, Typeable2 t)
      => (forall e. Data e => e -> m e)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> m (t d1 d2))
      -> d -> m d
ext2M def ext = unM ((M def) `ext2` (M ext))


-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable2 t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))


-- | Type extension of readers for type constructors
ext2R :: (Monad m, Data d, Typeable2 t)
      => m d
      -> (forall d1 d2. (Data d1, Data d2) => m (t d1 d2))
      -> m d
ext2R def ext = unR ((R def) `ext2` (R ext))


-- | Type extension of builders for type constructors
ext2B :: (Data a, Typeable2 t)
      => a
      -> (forall d1 d2. (Data d1, Data d2) => (t d1 d2))
      -> a
ext2B def ext = unB ((B def) `ext2` (B ext))

------------------------------------------------------------------------------
--
--      Type constructors for type-level lambdas
--
------------------------------------------------------------------------------


-- | The type constructor for transformations
newtype T x = T { unT :: x -> x }

-- | The type constructor for transformations
newtype M m x = M { unM :: x -> m x }

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | The type constructor for readers
newtype R m x = R { unR :: m x }

-- | The type constructor for builders
newtype B x = B {unB :: x}
