-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Aliases
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module provides
-- a number of declarations for typical generic function types,
-- corresponding type case, and others.
--
-----------------------------------------------------------------------------

module Data.Generics.Aliases ( 

	-- * Combinators to \"make\" generic functions via cast
	mkT, mkQ, mkM, mkMp, mkR,
	extT, extQ, extM, extMp, extB, extR,

	-- * Type synonyms for generic function types
	GenericT, 
	GenericQ,
	GenericM,
	GenericB,
	GenericR,
        Generic,
        Generic'(..),

	-- * Inredients of generic functions
	orElse,

	-- * Function combinators on generic functions
	recoverMp,
	recoverQ,
	choiceMp,
	choiceQ,

        -- * Operators for (over-appreciated) unfolding
        gunfoldB,
        gunfoldR

  ) where


import Control.Monad
import Data.Generics.Basics



------------------------------------------------------------------------------
--
--	Combinators to "make" generic functions
--	We use type-safe cast in a number of ways to make generic functions.
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
mkT f = case cast f of
               Just g -> g
               Nothing -> id


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
mkM f = case castarr f of
              Just g  -> g
              Nothing -> return


{-

For the remaining definitions, we stick to a more concise style, i.e.,
we fold maybies with "maybe" instead of case ... of ..., and we also
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
mkMp = maybe (const mzero) id . castarr


-- | Make a generic builder;
--   start from a type-specific ase;
--   resort to no build (i.e., mzero) otherwise
--
mkR :: ( MonadPlus m
       , Typeable a
       , Typeable b
       )
    => m b -> m a
mkR = maybe mzero id . castss


-- | Extend a generic transformation by a type-specific case
extT :: ( Typeable a
        , Typeable b 
        )
     => (a -> a)
     -> (b -> b)
     -> a
     -> a
extT f = maybe f id . cast


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
extM f = maybe f id . castarr


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
extR f = maybe f id . castss


------------------------------------------------------------------------------
--
--	Type synonyms for generic function types
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
--   GenericT = Generic ID.
--
type Generic c = forall a. Data a => a -> c a


-- | Wrapped generic functions;
--   recall: [Generic c] would be legal but [Generic' c] not.
--
data Generic' c = Generic' { unGeneric' :: Generic c }



-- | Left-biased choice on maybies
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
--
--	Generic unfolding
--
------------------------------------------------------------------------------

-- | Construct an initial term with undefined immediate subterms
--   and then map over the skeleton to fill in proper terms.
gunfoldB :: Data a
         => Constr
         -> (forall a. Data a => a)
         -> a
gunfoldB c f = gmapT (const f) (fromConstr c)


-- | Monadic variation on \"gunfoldB\"
gunfoldR :: (Monad m, Data a)
         => Constr
         -> (forall a. Data a => m a)
         -> m a
gunfoldR c f = gmapM (const f) $ fromConstr c
