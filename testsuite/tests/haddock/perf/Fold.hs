{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Fold' s a@ is a generalization of something 'Foldable'. It allows
-- you to extract multiple results from a container. A 'Foldable' container
-- can be characterized by the behavior of
-- @'Data.Foldable.foldMap' :: ('Foldable' t, 'Monoid' m) => (a -> m) -> t a -> m@.
-- Since we want to be able to work with monomorphic containers, we could
-- generalize this signature to @forall m. 'Monoid' m => (a -> m) -> s -> m@,
-- and then decorate it with 'Const' to obtain
--
-- @type 'Fold' s a = forall m. 'Monoid' m => 'Getting' m s a@
--
-- Every 'Getter' is a valid 'Fold' that simply doesn't use the 'Monoid'
-- it is passed.
--
-- In practice the type we use is slightly more complicated to allow for
-- better error messages and for it to be transformed by certain
-- 'Applicative' transformers.
--
-- Everything you can do with a 'Foldable' container, you can with with a 'Fold' and there are
-- combinators that generalize the usual 'Foldable' operations here.
----------------------------------------------------------------------------
module Control.Lens.Fold
  (
  -- * Folds
    Fold
  , IndexedFold

  -- * Getting Started
  , (^..)
  , (^?)
  , (^?!)
  , pre, ipre
  , preview, previews, ipreview, ipreviews
  , preuse, preuses, ipreuse, ipreuses

  , has, hasn't

  -- ** Building Folds
  , folding, ifolding
  , foldring, ifoldring
  , folded
  , folded64
  , unfolded
  , iterated
  , filtered
  , filteredBy
  , backwards
  , repeated
  , replicated
  , cycled
  , takingWhile
  , droppingWhile
  , worded, lined

  -- ** Folding
  , foldMapOf, foldOf
  , foldrOf, foldlOf
  , toListOf, toNonEmptyOf
  , anyOf, allOf, noneOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_, forOf_, sequenceAOf_
  , traverse1Of_, for1Of_, sequence1Of_
  , mapMOf_, forMOf_, sequenceOf_
  , asumOf, msumOf
  , concatMapOf, concatOf
  , elemOf, notElemOf
  , lengthOf
  , nullOf, notNullOf
  , firstOf, first1Of, lastOf, last1Of
  , maximumOf, maximum1Of, minimumOf, minimum1Of
  , maximumByOf, minimumByOf
  , findOf
  , findMOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldr1Of', foldl1Of'
  , foldrMOf, foldlMOf
  , lookupOf

  -- * Indexed Folds
  , (^@..)
  , (^@?)
  , (^@?!)

  -- ** Indexed Folding
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf
  , ianyOf
  , iallOf
  , inoneOf
  , itraverseOf_
  , iforOf_
  , imapMOf_
  , iforMOf_
  , iconcatMapOf
  , ifindOf
  , ifindMOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf
  , itoListOf
  , elemIndexOf
  , elemIndicesOf
  , findIndexOf
  , findIndicesOf

  -- ** Building Indexed Folds
  , ifiltered
  , itakingWhile
  , idroppingWhile

  -- * Internal types
  , Leftmost
  , Rightmost
  , Traversed
  , Sequenced

  ) where

import Prelude
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad as Monad
import Control.Monad.Reader
import qualified Control.Monad.Reader as Reader
import Data.Functor
import Control.Monad.State
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Monoid (First (..), All (..), Any (..), Endo (..), Dual(..), Monoid(..))
import qualified Data.Monoid as Monoid
import Data.Ord (Down(..))
import Data.Functor.Compose
import Data.Functor.Contravariant
import Control.Applicative
import GHC.Stack
import Control.Applicative.Backwards
import Data.Kind
import Data.Functor.Identity
import Data.Bifunctor
import Control.Arrow (Arrow, ArrowApply(..), ArrowChoice(..), ArrowLoop(..), (&&&), (***))
import qualified Control.Arrow as Arrow
import qualified Control.Category as C
import Control.Monad.Writer
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Tree
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Control.Monad.State as State
import Control.Monad.Writer
import Data.Coerce
import qualified GHC.Generics as Generics
import GHC.Generics (K1(..), U1(..), Par1(..), (:.:)(..), Rec1, M1, (:*:)(..))
import Control.Monad.Trans.Cont
import qualified Data.Semigroup as Semi
import qualified Data.Semigroup as Semigroup
import Data.Complex
import Control.Monad.Trans.Identity
import qualified Data.Functor.Product as Functor
import Data.Proxy
import Data.Typeable
import Data.Ix
import Data.Foldable (traverse_)

infixr 9 #.
infixl 8 .#

{- |

There are two ways to define a comonad:

I. Provide definitions for 'extract' and 'extend'
satisfying these laws:

@
'extend' 'extract'      = 'id'
'extract' . 'extend' f  = f
'extend' f . 'extend' g = 'extend' (f . 'extend' g)
@

In this case, you may simply set 'fmap' = 'liftW'.

These laws are directly analogous to the laws for monads
and perhaps can be made clearer by viewing them as laws stating
that Cokleisli composition must be associative, and has extract for
a unit:

@
f '=>=' 'extract'   = f
'extract' '=>=' f   = f
(f '=>=' g) '=>=' h = f '=>=' (g '=>=' h)
@

II. Alternately, you may choose to provide definitions for 'fmap',
'extract', and 'duplicate' satisfying these laws:

@
'extract' . 'duplicate'      = 'id'
'fmap' 'extract' . 'duplicate' = 'id'
'duplicate' . 'duplicate'    = 'fmap' 'duplicate' . 'duplicate'
@

In this case you may not rely on the ability to define 'fmap' in
terms of 'liftW'.

You may of course, choose to define both 'duplicate' /and/ 'extend'.
In that case you must also satisfy these laws:

@
'extend' f  = 'fmap' f . 'duplicate'
'duplicate' = 'extend' id
'fmap' f    = 'extend' (f . 'extract')
@

These are the default definitions of 'extend' and 'duplicate' and
the definition of 'liftW' respectively.

-}

class Functor w => Comonad w where
  -- |
  -- @
  -- 'extract' . 'fmap' f = f . 'extract'
  -- @
  extract :: w a -> a

  -- |
  -- @
  -- 'duplicate' = 'extend' 'id'
  -- 'fmap' ('fmap' f) . 'duplicate' = 'duplicate' . 'fmap' f
  -- @
  duplicate :: w a -> w (w a)
  duplicate = extend id

  -- |
  -- @
  -- 'extend' f = 'fmap' f . 'duplicate'
  -- @
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- | A 'Profunctor' @p@ is a 'Sieve' __on__ @f@ if it is a subprofunctor of @'Star' f@.
--
-- That is to say it is a subset of @Hom(-,f=)@ closed under 'lmap' and 'rmap'.
--
-- Alternately, you can view it as a sieve __in__ the comma category @Hask/f@.
class (Profunctor p, Functor f) => Sieve p f | p -> f where
  sieve :: p a b -> a -> f b

instance Sieve (->) Identity where
  sieve f = Identity . f
  {-# INLINE sieve #-}

instance (Monad m, Functor m) => Sieve (Arrow.Kleisli m) m where
  sieve = Arrow.runKleisli
  {-# INLINE sieve #-}

-- | A 'Profunctor' @p@ is a 'Cosieve' __on__ @f@ if it is a subprofunctor of @'Costar' f@.
--
-- That is to say it is a subset of @Hom(f-,=)@ closed under 'lmap' and 'rmap'.
--
-- Alternately, you can view it as a cosieve __in__ the comma category @f/Hask@.
class (Profunctor p, Functor f) => Cosieve p f | p -> f where
  cosieve :: p a b -> f a -> b

instance Cosieve (->) Identity where
  cosieve f (Identity d) = f d
  {-# INLINE cosieve #-}

instance Cosieve Tagged Proxy where
  cosieve (Tagged a) _ = a
  {-# INLINE cosieve #-}

-- * Representable Profunctors

-- | A 'Profunctor' @p@ is 'Representable' if there exists a 'Functor' @f@ such that
-- @p d c@ is isomorphic to @d -> f c@.
class (Sieve p (Rep p), Strong p) => Representable p where
  type Rep p :: * -> *
  -- | Laws:
  --
  -- @
  -- 'tabulate' '.' 'sieve' ≡ 'id'
  -- 'sieve' '.' 'tabulate' ≡ 'id'
  -- @
  tabulate :: (d -> Rep p c) -> p d c

-- | Default definition for 'first'' given that p is 'Representable'.
firstRep :: Representable p => p a b -> p (a, c) (b, c)
firstRep p = tabulate $ \(a,c) -> (\b -> (b, c)) <$> sieve p a

-- | Default definition for 'second'' given that p is 'Representable'.
secondRep :: Representable p => p a b -> p (c, a) (c, b)
secondRep p = tabulate $ \(c,a) -> (,) c <$> sieve p a

instance Representable (->) where
  type Rep (->) = Identity
  tabulate f = runIdentity . f
  {-# INLINE tabulate #-}

instance (Monad m, Functor m) => Representable (Arrow.Kleisli m) where
  type Rep (Arrow.Kleisli m) = m
  tabulate = Arrow.Kleisli
  {-# INLINE tabulate #-}

{- TODO: coproducts and products
instance (Representable p, Representable q) => Representable (Bifunctor.Product p q)
  type Rep (Bifunctor.Product p q) = Functor.Product p q

instance (Corepresentable p, Corepresentable q) => Corepresentable (Bifunctor.Product p q) where
  type Rep (Bifunctor.Product p q) = Functor.Sum p q
-}

----------------------------------------------------------------------------
-- * Pastro
----------------------------------------------------------------------------

-- | Pastro -| Tambara
--
-- @
-- Pastro p ~ exists z. Costar ((,)z) `Procompose` p `Procompose` Star ((,)z)
-- @
--
-- 'Pastro' freely makes any 'Profunctor' 'Strong'.
data Pastro p a b where
  Pastro :: ((y, z) -> b) -> p x y -> (a -> (x, z)) -> Pastro p a b

instance Functor (Pastro p a) where
  fmap f (Pastro l m r) = Pastro (f . l) m r

instance Profunctor (Pastro p) where
  dimap f g (Pastro l m r) = Pastro (g . l) m (r . f)
  lmap f (Pastro l m r) = Pastro l m (r . f)
  rmap g (Pastro l m r) = Pastro (g . l) m r
  w #. Pastro l m r = Pastro (w #. l) m r
  Pastro l m r .# w = Pastro l m (r .# w)

--------------------------------------------------------------------------------
-- * Costrength for (,)
--------------------------------------------------------------------------------

-- | Analogous to 'ArrowLoop', 'loop' = 'unfirst'
class Profunctor p => Costrong p where
  -- | Laws:
  --
  -- @
  -- 'unfirst' ≡ 'unsecond' '.' 'dimap' 'swap' 'swap'
  -- 'lmap' (,()) ≡ 'unfirst' '.' 'rmap' (,())
  -- 'unfirst' '.' 'lmap' ('second' f) ≡ 'unfirst' '.' 'rmap' ('second' f)
  -- 'unfirst' '.' 'unfirst' = 'unfirst' '.' 'dimap' assoc unassoc where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  unfirst  :: p (a, d) (b, d) -> p a b
  unfirst = unsecond . dimap swap swap

  -- | Laws:
  --
  -- @
  -- 'unsecond' ≡ 'unfirst' '.' 'dimap' 'swap' 'swap'
  -- 'lmap' ((),) ≡ 'unsecond' '.' 'rmap' ((),)
  -- 'unsecond' '.' 'lmap' ('first' f) ≡ 'unsecond' '.' 'rmap' ('first' f)
  -- 'unsecond' '.' 'unsecond' = 'unsecond' '.' 'dimap' unassoc assoc where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  unsecond :: p (d, a) (d, b) -> p a b
  unsecond = unfirst . dimap swap swap

  {-# MINIMAL unfirst | unsecond #-}

instance Costrong (->) where
  unfirst f a = b where (b, d) = f (a, d)
  unsecond f a = b where (d, b) = f (d, a)

instance Costrong Tagged where
  unfirst (Tagged bd) = Tagged (fst bd)
  unsecond (Tagged db) = Tagged (snd db)

instance MonadFix m => Costrong (Arrow.Kleisli m) where
  unfirst (Arrow.Kleisli f) = Arrow.Kleisli (liftM fst . mfix . f')
    where f' x y = f (x, snd y)

-- | 'tabulate' and 'sieve' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'tabulated' :: 'Representable' p => 'Iso'' (d -> 'Rep' p c) (p d c)@
tabulated :: (Representable p, Representable q) => Iso (d -> Rep p c) (d' -> Rep q c') (p d c) (q d' c')
tabulated = dimap tabulate (fmap sieve)
{-# INLINE tabulated #-}

-- * Corepresentable Profunctors

-- | A 'Profunctor' @p@ is 'Corepresentable' if there exists a 'Functor' @f@ such that
-- @p d c@ is isomorphic to @f d -> c@.
class (Cosieve p (Corep p), Costrong p) => Corepresentable p where
  type Corep p :: * -> *
  -- | Laws:
  --
  -- @
  -- 'cotabulate' '.' 'cosieve' ≡ 'id'
  -- 'cosieve' '.' 'cotabulate' ≡ 'id'
  -- @
  cotabulate :: (Corep p d -> c) -> p d c

-- | Default definition for 'unfirst' given that @p@ is 'Corepresentable'.
unfirstCorep :: Corepresentable p => p (a, d) (b, d) -> p a b
unfirstCorep p = cotabulate f
  where f fa = b where (b, d) = cosieve p ((\a -> (a, d)) <$> fa)

-- | Default definition for 'unsecond' given that @p@ is 'Corepresentable'.
unsecondCorep :: Corepresentable p => p (d, a) (d, b) -> p a b
unsecondCorep p = cotabulate f
  where f fa = b where (d, b) = cosieve p ((,) d <$> fa)

-- | Default definition for 'closed' given that @p@ is 'Corepresentable'
closedCorep :: Corepresentable p => p a b -> p (x -> a) (x -> b)
closedCorep p = cotabulate $ \fs x -> cosieve p (fmap ($ x) fs)

instance Corepresentable (->) where
  type Corep (->) = Identity
  cotabulate f = f . Identity
  {-# INLINE cotabulate #-}

instance Corepresentable Tagged where
  type Corep Tagged = Proxy
  cotabulate f = Tagged (f Proxy)
  {-# INLINE cotabulate #-}

-- | 'cotabulate' and 'cosieve' form two halves of an isomorphism.
--
-- This can be used with the combinators from the @lens@ package.
--
-- @'cotabulated' :: 'Corep' f p => 'Iso'' (f d -> c) (p d c)@
cotabulated :: (Corepresentable p, Corepresentable q) => Iso (Corep p d -> c) (Corep q d' -> c') (p d c) (q d' c')
cotabulated = dimap cotabulate (fmap cosieve)
{-# INLINE cotabulated #-}

--------------------------------------------------------------------------------
-- * Prep
--------------------------------------------------------------------------------

-- | @'Prep' -| 'Star' :: [Hask, Hask] -> Prof@
--
-- This gives rise to a monad in @Prof@, @('Star'.'Prep')@, and
-- a comonad in @[Hask,Hask]@ @('Prep'.'Star')@
--
-- 'Prep' has a polymorphic kind since @5.6@.

-- Prep :: (Type -> k -> Type) -> (k -> Type)
data Prep p a where
  Prep :: x -> p x a -> Prep p a

instance Profunctor p => Functor (Prep p) where
  fmap f (Prep x p) = Prep x (rmap f p)

instance (Applicative (Rep p), Representable p) => Applicative (Prep p) where
  pure a = Prep () $ tabulate $ const $ pure a
  Prep xf pf <*> Prep xa pa = Prep (xf,xa) (tabulate go) where
    go (xf',xa') = sieve pf xf' <*> sieve pa xa'

instance (Monad (Rep p), Representable p) => Monad (Prep p) where
  return a = Prep () $ tabulate $ const $ return a
  Prep xa pa >>= f = Prep xa $ tabulate $ sieve pa >=> \a -> case f a of
    Prep xb pb -> sieve pb xb

--------------------------------------------------------------------------------
-- * Coprep
--------------------------------------------------------------------------------

-- | 'Prep' has a polymorphic kind since @5.6@.

-- Coprep :: (k -> Type -> Type) -> (k -> Type)
newtype Coprep p a = Coprep { runCoprep :: forall r. p a r -> r }

instance Profunctor p => Functor (Coprep p) where
  fmap f (Coprep g) = Coprep (g . lmap f)


------------------------------------------------------------------------------
-- Strong
------------------------------------------------------------------------------

-- | Generalizing 'Star' of a strong 'Functor'
--
-- /Note:/ Every 'Functor' in Haskell is strong with respect to @(,)@.
--
-- This describes profunctor strength with respect to the product structure
-- of Hask.
--
-- <http://www.riec.tohoku.ac.jp/~asada/papers/arrStrMnd.pdf>
--
class Profunctor p => Strong p where
  -- | Laws:
  --
  -- @
  -- 'first'' ≡ 'dimap' 'swap' 'swap' '.' 'second''
  -- 'lmap' 'fst' ≡ 'rmap' 'fst' '.' 'first''
  -- 'lmap' ('second'' f) '.' 'first'' ≡ 'rmap' ('second'' f) '.' 'first''
  -- 'first'' '.' 'first'' ≡ 'dimap' assoc unassoc '.' 'first'' where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  first' :: p a b  -> p (a, c) (b, c)
  first' = dimap swap swap . second'

  -- | Laws:
  --
  -- @
  -- 'second'' ≡ 'dimap' 'swap' 'swap' '.' 'first''
  -- 'lmap' 'snd' ≡ 'rmap' 'snd' '.' 'second''
  -- 'lmap' ('first'' f) '.' 'second'' ≡ 'rmap' ('first'' f) '.' 'second''
  -- 'second'' '.' 'second'' ≡ 'dimap' unassoc assoc '.' 'second'' where
  --   assoc ((a,b),c) = (a,(b,c))
  --   unassoc (a,(b,c)) = ((a,b),c)
  -- @
  second' :: p a b -> p (c, a) (c, b)
  second' = dimap swap swap . first'

  {-# MINIMAL first' | second' #-}

uncurry' :: Strong p => p a (b -> c) -> p (a, b) c
uncurry' = rmap (\(f,x) -> f x) . first'
{-# INLINE uncurry' #-}

strong :: Strong p => (a -> b -> c) -> p a b -> p a c
strong f x = dimap (\a -> (a, a)) (\(b, a) -> f a b) (first' x)

instance Strong (->) where
  first' ab ~(a, c) = (ab a, c)
  {-# INLINE first' #-}
  second' ab ~(c, a) = (c, ab a)
  {-# INLINE second' #-}

instance Monad m => Strong (Arrow.Kleisli m) where
  first' (Arrow.Kleisli f) = Arrow.Kleisli $ \ ~(a, c) -> do
     b <- f a
     return (b, c)
  {-# INLINE first' #-}
  second' (Arrow.Kleisli f) = Arrow.Kleisli $ \ ~(c, a) -> do
     b <- f a
     return (c, b)
  {-# INLINE second' #-}

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@,
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is \"free\"
--
-- 'Tagged' has kind @k -> * -> *@ if the compiler supports @PolyKinds@, therefore
-- there is an extra @k@ showing in the instance haddocks that may cause confusion.
newtype Tagged s b = Tagged { unTagged :: b } deriving
  ( Eq, Ord, Ix, Bounded
  , Generics.Generic
  , Generics.Generic1
  , Typeable
  )

-----------------------------------------------------------------------------
-- Settable
-----------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class (Applicative f, Distributive f, Traversable f) => Settable f where
  untainted :: f a -> a

  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot g = g `seq` rmap untainted g
  {-# INLINE untaintedDot #-}

  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot g = g `seq` rmap pure g
  {-# INLINE taintedDot #-}

-- | So you can pass our 'Control.Lens.Setter.Setter' into combinators from other lens libraries.
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
  {-# INLINE taintedDot #-}

-- | 'Control.Lens.Fold.backwards'
instance Settable f => Settable (Backwards f) where
  untainted = untaintedDot forwards
  {-# INLINE untainted #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untaintedDot (untaintedDot getCompose)
  {-# INLINE untainted #-}


-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Lens.Extras (is)
-- >>> import Data.Function
-- >>> import Data.List.Lens
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Data.Monoid (Sum (..))
-- >>> import System.Timeout (timeout)
-- >>> import qualified Data.Map as Map
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!

infixl 8 ^., ^@.

infixl 4 <.>, <., .>

class Distributive f

-- | The generalization of 'Costar' of 'Functor' that is strong with respect
-- to 'Either'.
--
-- Note: This is also a notion of strength, except with regards to another monoidal
-- structure that we can choose to equip Hask with: the cocartesian coproduct.
class Profunctor p => Choice p where
  -- | Laws:
  --
  -- @
  -- 'left'' ≡ 'dimap' swapE swapE '.' 'right'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Left' ≡ 'lmap' 'Left' '.' 'left''
  -- 'lmap' ('right' f) '.' 'left'' ≡ 'rmap' ('right' f) '.' 'left''
  -- 'left'' '.' 'left'' ≡ 'dimap' assocE unassocE '.' 'left'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b)) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c
  -- @
  left'  :: p a b -> p (Either a c) (Either b c)
  left' =  dimap (either Right Left) (either Right Left) . right'

  -- | Laws:
  --
  -- @
  -- 'right'' ≡ 'dimap' swapE swapE '.' 'left'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Right' ≡ 'lmap' 'Right' '.' 'right''
  -- 'lmap' ('left' f) '.' 'right'' ≡ 'rmap' ('left' f) '.' 'right''
  -- 'right'' '.' 'right'' ≡ 'dimap' unassocE assocE '.' 'right'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b)) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c
  -- @
  right' :: p a b -> p (Either c a) (Either c b)
  right' =  dimap (either Right Left) (either Right Left) . left'

  {-# MINIMAL left' | right' #-}

instance Choice (->) where
  left' ab (Left a) = Left (ab a)
  left' _ (Right c) = Right c
  {-# INLINE left' #-}
  right' = fmap
  {-# INLINE right' #-}

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
  (.#) pbc _ = coerce pbc
  {-# INLINE (#.) #-}
  {-# INLINE (.#) #-}

instance Comonad Identity
instance Comonad ((,) i)
instance Applicative (Tagged a)
instance Functor (Tagged a)
instance Profunctor Tagged
instance Profunctor (Arrow.Kleisli m)
instance Distributive (Compose f g)
instance Distributive (Backwards f)
instance Distributive Identity

instance Monad m => Choice (Arrow.Kleisli m) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance Choice Tagged where
  left' (Tagged b) = Tagged (Left b)
  {-# INLINE left' #-}
  right' (Tagged b) = Tagged (Right b)
  {-# INLINE right' #-}

-- | A strong lax semi-monoidal endofunctor.
-- This is equivalent to an 'Applicative' without 'pure'.
--
-- Laws:
--
-- @
-- ('.') '<$>' u '<.>' v '<.>' w = u '<.>' (v '<.>' w)
-- x '<.>' (f '<$>' y) = ('.' f) '<$>' x '<.>' y
-- f '<$>' (x '<.>' y) = (f '.') '<$>' x '<.>' y
-- @
--
-- The laws imply that `.>` and `<.` really ignore their
-- left and right results, respectively, and really
-- return their right and left results, respectively.
-- Specifically,
--
-- @
-- (mf '<$>' m) '.>' (nf '<$>' n) = nf '<$>' (m '.>' n)
-- (mf '<$>' m) '<.' (nf '<$>' n) = mf '<$>' (m '<.' n)
-- @
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b
  (<.>) = liftF2 id

  -- | @ a '.>' b = 'const' 'id' '<$>' a '<.>' b @
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | @ a '<.' b = 'const' '<$>' a '<.>' b @
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

  -- | Lift a binary function into a comonad with zipping
  liftF2 :: (a -> b -> c) -> f a -> f b -> f c
  liftF2 f a b = f <$> a <.> b
  {-# INLINE liftF2 #-}

instance Apply (Tagged a) where
  (<.>) = (<*>)
  (<.) = (<*)
  (.>) = (*>)

instance Apply Proxy where
  (<.>) = (<*>)
  (<.) = (<*)
  (.>) = (*>)

instance Apply f => Apply (Backwards f) where
  Backwards f <.> Backwards a = Backwards (flip id <$> a <.> f)

instance (Apply f, Apply g) => Apply (Compose f g) where
  Compose f <.> Compose x = Compose ((<.>) <$> f <.> x)

instance (Apply f, Apply g) => Apply (Functor.Product f g) where
  Functor.Pair f g <.> Functor.Pair x y = Functor.Pair (f <.> x) (g <.> y)

-- | A @'(,)' m@ is not 'Applicative' unless its @m@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup m => Apply ((,)m) where
  (m, f) <.> (n, a) = (m <> n, f a)
  (m, a) <.  (n, _) = (m <> n, a)
  (m, _)  .> (n, b) = (m <> n, b)

instance Apply NonEmpty where
  (<.>) = ap

instance Apply (Either a) where
  Left a  <.> _       = Left a
  Right _ <.> Left a  = Left a
  Right f <.> Right b = Right (f b)

  Left a  <.  _       = Left a
  Right _ <.  Left a  = Left a
  Right a <.  Right _ = Right a

  Left a   .> _       = Left a
  Right _  .> Left a  = Left a
  Right _  .> Right b = Right b

-- | A @'Const' m@ is not 'Applicative' unless its @m@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup m => Apply (Const m) where
  Const m <.> Const n = Const (m <> n)
  Const m <.  Const n = Const (m <> n)
  Const m  .> Const n = Const (m <> n)

instance Apply ((->)m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply ZipList where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply [] where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply IO where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply Maybe where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply Identity where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply w => Apply (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

instance Monad m => Apply (WrappedMonad m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Arrow a => Apply (WrappedArrow a b) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance Apply Complex where
  (a :+ b) <.> (c :+ d) = a c :+ b d

-- | A 'Map k' is not 'Applicative', but it is an instance of 'Apply'
instance Ord k => Apply (Map k) where
  (<.>) = Map.intersectionWith id
  (<. ) = Map.intersectionWith const
  ( .>) = Map.intersectionWith (const id)

-- | An 'IntMap' is not 'Applicative', but it is an instance of 'Apply'
instance Apply IntMap.IntMap where
  (<.>) = IntMap.intersectionWith id
  (<. ) = IntMap.intersectionWith const
  ( .>) = IntMap.intersectionWith (const id)

instance Apply Tree where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

-- MaybeT is _not_ the same as Compose f Maybe
instance (Functor m, Monad m) => Apply (MaybeT m) where
  (<.>) = apDefault

instance (Functor m, Monad m) => Apply (ExceptT e m) where
  (<.>) = apDefault

instance Apply m => Apply (ReaderT e m) where
  ReaderT f <.> ReaderT a = ReaderT $ \e -> f e <.> a e

-- unfortunately, WriterT has its wrapped product in the wrong order to just use (<.>) instead of flap
-- | A @'Strict.WriterT' w m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Apply m, Semigroup w) => Apply (Strict.WriterT w m) where
  Strict.WriterT f <.> Strict.WriterT a = Strict.WriterT $ flap <$> f <.> a where
    flap (x,m) (y,n) = (x y, m <> n)

-- | A @'Lazy.WriterT' w m@ is not 'Applicative' unless its @w@ is a 'Monoid', but it is an instance of 'Apply'
instance (Apply m, Semigroup w) => Apply (Lazy.WriterT w m) where
  Lazy.WriterT f <.> Lazy.WriterT a = Lazy.WriterT $ flap <$> f <.> a where
    flap ~(x,m) ~(y,n) = (x y, m <> n)

instance Apply (ContT r m) where
  ContT f <.> ContT v = ContT $ \k -> f $ \g -> v (k . g)

-- | Wrap an 'Applicative' to be used as a member of 'Apply'
newtype WrappedApplicative f a = WrapApplicative { unwrapApplicative :: f a }

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrapApplicative a) = WrapApplicative (f <$> a)

instance Applicative f => Apply (WrappedApplicative f) where
  WrapApplicative f <.> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <.  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  .> WrapApplicative b = WrapApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrapApplicative . pure
  WrapApplicative f <*> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <*  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  *> WrapApplicative b = WrapApplicative (a  *> b)

instance Alternative f => Alternative (WrappedApplicative f) where
  empty = WrapApplicative empty
  WrapApplicative a <|> WrapApplicative b = WrapApplicative (a <|> b)

-- | Transform an Apply into an Applicative by adding a unit.
newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either (f a) a }

-- | Apply a non-empty container of functions to a possibly-empty-with-unit container of values.
(<.*>) :: (Apply f) => f (a -> b) -> MaybeApply f a -> f b
ff <.*> MaybeApply (Left fa) = ff <.> fa
ff <.*> MaybeApply (Right a) = ($ a) <$> ff
infixl 4 <.*>

-- | Apply a possibly-empty-with-unit container of functions to a non-empty container of values.
(<*.>) :: (Apply f) => MaybeApply f (a -> b) -> f a -> f b
MaybeApply (Left ff) <*.> fa = ff <.> fa
MaybeApply (Right f) <*.> fa = f <$> fa
infixl 4 <*.>

-- | Traverse a 'Traversable' using 'Apply', getting the results back in a 'MaybeApply'.
traverse1Maybe :: (Traversable t, Apply f) => (a -> f b) -> t a -> MaybeApply f (t b)
traverse1Maybe f = traverse (MaybeApply . Left . f)

instance Functor f => Functor (MaybeApply f) where
  fmap f (MaybeApply (Right a)) = MaybeApply (Right (f     a ))
  fmap f (MaybeApply (Left fa)) = MaybeApply (Left  (f <$> fa))

instance Apply f => Apply (MaybeApply f) where
  MaybeApply (Right f) <.> MaybeApply (Right a) = MaybeApply (Right (f         a ))
  MaybeApply (Right f) <.> MaybeApply (Left fa) = MaybeApply (Left  (f     <$> fa))
  MaybeApply (Left ff) <.> MaybeApply (Right a) = MaybeApply (Left  (($ a) <$> ff))
  MaybeApply (Left ff) <.> MaybeApply (Left fa) = MaybeApply (Left  (ff    <.> fa))

  MaybeApply a         <. MaybeApply (Right _) = MaybeApply a
  MaybeApply (Right a) <. MaybeApply (Left fb) = MaybeApply (Left (a  <$ fb))
  MaybeApply (Left fa) <. MaybeApply (Left fb) = MaybeApply (Left (fa <. fb))

  MaybeApply (Right _) .> MaybeApply b = MaybeApply b
  MaybeApply (Left fa) .> MaybeApply (Right b) = MaybeApply (Left (fa $> b ))
  MaybeApply (Left fa) .> MaybeApply (Left fb) = MaybeApply (Left (fa .> fb))

instance Apply f => Applicative (MaybeApply f) where
  pure a = MaybeApply (Right a)
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance Apply Down where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

instance Apply Monoid.Sum where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Product where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Dual where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.First where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Monoid.Last where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
deriving instance Apply f => Apply (Monoid.Alt f)
-- in GHC 8.6 we'll have to deal with Apply f => Apply (Ap f) the same way
instance Apply Semigroup.First where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Last where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Min where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)
instance Apply Semigroup.Max where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

instance (Apply f, Apply g) => Apply (f :*: g) where
  (a :*: b) <.> (c :*: d) = (a <.> c) :*: (b <.> d)

deriving instance Apply f => Apply (M1 i t f)
deriving instance Apply f => Apply (Rec1 f)

instance (Apply f, Apply g) => Apply (f :.: g) where
  Comp1 m <.> Comp1 n = Comp1 $ (<.>) <$> m <.> n

instance Apply U1 where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

-- | A @'K1' i c@ is not 'Applicative' unless its @c@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup c => Apply (K1 i c) where
  K1 a <.> K1 b = K1 (a <> b)
  K1 a <.  K1 b = K1 (a <> b)
  K1 a  .> K1 b = K1 (a <> b)
instance Apply Par1 where (<.>)=(<*>);(.>)=(*>);(<.)=(<*)

-- | A 'V1' is not 'Applicative', but it is an instance of 'Apply'
instance Apply Generics.V1 where
  e <.> _ = case e of {}
------------------------------------------------------------------------------
-- Magma
------------------------------------------------------------------------------

-- | This provides a way to peek at the internal structure of a
-- 'Control.Lens.Traversal.Traversal' or 'Control.Lens.Traversal.IndexedTraversal'
data Magma i t b a where
  MagmaAp   :: Magma i (x -> y) b a -> Magma i x b a -> Magma i y b a
  MagmaPure :: x -> Magma i x b a
  MagmaFmap :: (x -> y) -> Magma i x b a -> Magma i y b a
  Magma :: i -> a -> Magma i b b a

-- note the 3rd argument infers as phantom, but that would be unsound
type role Magma representational nominal nominal nominal

instance Functor (Magma i t b) where
  fmap f (MagmaAp x y)    = MagmaAp (fmap f x) (fmap f y)
  fmap _ (MagmaPure x)    = MagmaPure x
  fmap f (MagmaFmap xy x) = MagmaFmap xy (fmap f x)
  fmap f (Magma i a)  = Magma i (f a)

instance Foldable (Magma i t b) where
  foldMap f (MagmaAp x y)   = foldMap f x `mappend` foldMap f y
  foldMap _ MagmaPure{}     = mempty
  foldMap f (MagmaFmap _ x) = foldMap f x
  foldMap f (Magma _ a) = f a

instance Traversable (Magma i t b) where
  traverse f (MagmaAp x y)    = MagmaAp <$> traverse f x <*> traverse f y
  traverse _ (MagmaPure x)    = pure (MagmaPure x)
  traverse f (MagmaFmap xy x) = MagmaFmap xy <$> traverse f x
  traverse f (Magma i a)  = Magma i <$> f a

instance (Show i, Show a) => Show (Magma i t b a) where
  showsPrec d (MagmaAp x y) = showParen (d > 4) $
    showsPrec 4 x . showString " <*> " . showsPrec 5 y
  showsPrec d (MagmaPure _) = showParen (d > 10) $
    showString "pure .."
  showsPrec d (MagmaFmap _ x) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 x
  showsPrec d (Magma i a) = showParen (d > 10) $
    showString "Magma " . showsPrec 11 i . showChar ' ' . showsPrec 11 a

-- | Run a 'Magma' where all the individual leaves have been converted to the
-- expected type
runMagma :: Magma i t a a -> t
runMagma (MagmaAp l r)   = runMagma l (runMagma r)
runMagma (MagmaFmap f r) = f (runMagma r)
runMagma (MagmaPure x)   = x
runMagma (Magma _ a) = a

------------------------------------------------------------------------------
-- Molten
------------------------------------------------------------------------------

-- | This is a a non-reassociating initially encoded version of 'Bazaar'.
newtype Molten i a b t = Molten { runMolten :: Magma i t b a }

instance Functor (Molten i a b) where
  fmap f (Molten xs) = Molten (MagmaFmap f xs)
  {-# INLINE fmap #-}

instance Apply (Molten i a b) where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

instance Applicative (Molten i a b) where
  pure  = Molten #. MagmaPure
  {-# INLINE pure #-}
  Molten xs <*> Molten ys = Molten (MagmaAp xs ys)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- Mafic
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations in sums where possible.
data Mafic a b t = Mafic Int (Int -> Magma Int t b a)

-- | Generate a 'Magma' using from a prefix sum.
runMafic :: Mafic a b t -> Magma Int t b a
runMafic (Mafic _ k) = k 0

instance Functor (Mafic a b) where
  fmap f (Mafic w k) = Mafic w (MagmaFmap f . k)
  {-# INLINE fmap #-}

instance Apply (Mafic a b) where
  Mafic wf mf <.> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<.>) #-}

instance Applicative (Mafic a b) where
  pure a = Mafic 0 $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  Mafic wf mf <*> ~(Mafic wa ma) = Mafic (wf + wa) $ \o -> MagmaAp (mf o) (ma (o + wf))
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- TakingWhile
------------------------------------------------------------------------------

-- | This is used to generate an indexed magma from an unindexed source
--
-- By constructing it this way we avoid infinite reassociations where possible.
--
-- In @'TakingWhile' p g a b t@, @g@ has a @nominal@ role to avoid exposing an illegal _|_ via 'Contravariant',
-- while the remaining arguments are degraded to a @nominal@ role by the invariants of 'Magma'
data TakingWhile p (g :: Type -> Type) a b t = TakingWhile Bool t (Bool -> Magma () t b (Corep p a))
type role TakingWhile nominal nominal nominal nominal nominal

-- | Generate a 'Magma' with leaves only while the predicate holds from left to right.
runTakingWhile :: TakingWhile p f a b t -> Magma () t b (Corep p a)
runTakingWhile (TakingWhile _ _ k) = k True

instance Functor (TakingWhile p f a b) where
  fmap f (TakingWhile w t k) = let ft = f t in TakingWhile w ft $ \b -> if b then MagmaFmap f (k b) else MagmaPure ft
  {-# INLINE fmap #-}

instance Apply (TakingWhile p f a b) where
  TakingWhile wf tf mf <.> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<.>) #-}

instance Applicative (TakingWhile p f a b) where
  pure a = TakingWhile True a $ \_ -> MagmaPure a
  {-# INLINE pure #-}
  TakingWhile wf tf mf <*> ~(TakingWhile wa ta ma) = TakingWhile (wf && wa) (tf ta) $ \o ->
    if o then MagmaAp (mf True) (ma wf) else MagmaPure (tf ta)
  {-# INLINE (<*>) #-}



-- This constraint is unused intentionally, it protects TakingWhile
instance Contravariant f => Contravariant (TakingWhile p f a b) where
  contramap _ = (<$) (error "contramap: TakingWhile")
  {-# INLINE contramap #-}

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | A 'Monoid' for a 'Contravariant' 'Applicative'.
newtype Folding f a = Folding { getFolding :: f a }

instance (Contravariant f, Applicative f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr *> fs)
  {-# INLINE (<>) #-}

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- Traversed
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.traverseOf_' and the like.
--
-- The argument 'a' of the result should not be used!
newtype Traversed a f = Traversed { getTraversed :: f a }

-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
instance Applicative f => Semigroup (Traversed a f) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed a f) where
  mempty = Traversed (pure (error "Traversed: value used"))
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- TraversedF
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Fold.traverse1Of_' and the like.
--
-- @since 4.16
newtype TraversedF a f = TraversedF { getTraversedF :: f a }

instance Apply f => Semigroup (TraversedF a f) where
  TraversedF ma <> TraversedF mb = TraversedF (ma .> mb)
  {-# INLINE (<>) #-}

instance (Apply f, Applicative f) => Monoid (TraversedF a f) where
  mempty = TraversedF (pure (error "TraversedF: value used"))
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- Sequenced
------------------------------------------------------------------------------

-- | Used internally by 'Control.Lens.Traversal.mapM_' and the like.
--
-- The argument 'a' of the result should not be used!
--
-- See 4.16 Changelog entry for the explanation of "why not Apply f =>"?
newtype Sequenced a m = Sequenced { getSequenced :: m a }

instance Monad m => Semigroup (Sequenced a m) where
  Sequenced ma <> Sequenced mb = Sequenced (ma >> mb)
  {-# INLINE (<>) #-}

instance Monad m => Monoid (Sequenced a m) where
  mempty = Sequenced (return (error "Sequenced: value used"))
  {-# INLINE mempty #-}

------------------------------------------------------------------------------
-- NonEmptyDList
------------------------------------------------------------------------------

newtype NonEmptyDList a
  = NonEmptyDList { getNonEmptyDList :: [a] -> NonEmpty.NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  NonEmptyDList f <> NonEmptyDList g = NonEmptyDList (f . NonEmpty.toList . g)

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Control.Lens.Fold.firstOf'.
data Leftmost a = LPure | LLeaf a | LStep (Leftmost a)

instance Semigroup (Leftmost a) where
  x <> y = LStep $ case x of
    LPure    -> y
    LLeaf _  -> x
    LStep x' -> case y of
      -- The last two cases make firstOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that firstOf (backwards folded) [1..] is Just _|_.
      LPure    -> x'
      LLeaf a  -> LLeaf $ fromMaybe a (getLeftmost x')
      LStep y' -> mappend x' y'

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LStep x) = getLeftmost x

-- | Used for 'Control.Lens.Fold.lastOf'.
data Rightmost a = RPure | RLeaf a | RStep (Rightmost a)

instance Semigroup (Rightmost a) where
  x <> y = RStep $ case y of
    RPure    -> x
    RLeaf _  -> y
    RStep y' -> case x of
      -- The last two cases make lastOf produce a Just as soon as any element
      -- is encountered, and possibly serve as a micro-optimisation; this
      -- behaviour can be disabled by replacing them with _ -> x <> y'.
      -- Note that this means that lastOf folded [1..] is Just _|_.
      RPure    -> y'
      RLeaf a  -> RLeaf $ fromMaybe a (getRightmost y')
      RStep x' -> mappend x' y'

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it can return 'Just'
-- the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RStep x) = getRightmost x

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | Build an (index-preserving) 'Getter' from an arbitrary Haskell function.
--
-- @
-- 'to' f '.' 'to' g ≡ 'to' (g '.' f)
-- @
--
-- @
-- a '^.' 'to' f ≡ f a
-- @
--
-- >>> a ^.to f
-- f a
--
-- >>> ("hello","world")^.to snd
-- "world"
--
-- >>> 5^.to succ
-- 6
--
-- >>> (0, -5)^._2.to abs
-- 5
--
-- @
-- 'to' :: (s -> a) -> 'IndexPreservingGetter' s a
-- @
to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
to k = dimap k (contramap k)
{-# INLINE to #-}

-- |
-- @
-- 'ito' :: (s -> (i, a)) -> 'IndexedGetter' i s a
-- @
ito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
ito k = dimap k (contramap (snd . k)) . uncurry . indexed
{-# INLINE ito #-}


-- | Build an constant-valued (index-preserving) 'Getter' from an arbitrary Haskell value.
--
-- @
-- 'like' a '.' 'like' b ≡ 'like' b
-- a '^.' 'like' b ≡ b
-- a '^.' 'like' b ≡ a '^.' 'to' ('const' b)
-- @
--
-- This can be useful as a second case 'failing' a 'Fold'
-- e.g. @foo `failing` 'like' 0@
--
-- @
-- 'like' :: a -> 'IndexPreservingGetter' s a
-- @
like :: (Profunctor p, Contravariant f, Functor f) => a -> Optic' p f s a
like a = to (const a)
{-# INLINE like #-}

-- |
-- @
-- 'ilike' :: i -> a -> 'IndexedGetter' i s a
-- @
ilike :: (Indexable i p, Contravariant f, Functor f) => i -> a -> Over' p f s a
ilike i a = ito (const (i, a))
{-# INLINE ilike #-}

-- | When you see this in a type signature it indicates that you can
-- pass the function a 'Lens', 'Getter',
-- 'Control.Lens.Traversal.Traversal', 'Control.Lens.Fold.Fold',
-- 'Control.Lens.Prism.Prism', 'Control.Lens.Iso.Iso', or one of
-- the indexed variants, and it will just \"do the right thing\".
--
-- Most 'Getter' combinators are able to be used with both a 'Getter' or a
-- 'Control.Lens.Fold.Fold' in limited situations, to do so, they need to be
-- monomorphic in what we are going to extract with 'Control.Applicative.Const'. To be compatible
-- with 'Lens', 'Control.Lens.Traversal.Traversal' and
-- 'Control.Lens.Iso.Iso' we also restricted choices of the irrelevant @t@ and
-- @b@ parameters.
--
-- If a function accepts a @'Getting' r s a@, then when @r@ is a 'Data.Monoid.Monoid', then
-- you can pass a 'Control.Lens.Fold.Fold' (or
-- 'Control.Lens.Traversal.Traversal'), otherwise you can only pass this a
-- 'Getter' or 'Lens'.
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Used to consume an 'Control.Lens.Fold.IndexedFold'.
type IndexedGetting i m s a = Indexed i a (Const m a) -> s -> Const m s

-- | This is a convenient alias used when consuming (indexed) getters and (indexed) folds
-- in a highly general fashion.
type Accessing p m s a = p a (Const m a) -> s -> Const m s

-------------------------------------------------------------------------------
-- Getting Values
-------------------------------------------------------------------------------

-- | View the value pointed to by a 'Getter', 'Control.Lens.Iso.Iso' or
-- 'Lens' or the result of folding over all the results of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- at a monoidal value.
--
-- @
-- 'view' '.' 'to' ≡ 'id'
-- @
--
-- >>> view (to f) a
-- f a
--
-- >>> view _2 (1,"hello")
-- "hello"
--
-- >>> view (to succ) 5
-- 6
--
-- >>> view (_2._1) ("hello",("world","!!!"))
-- "world"
--
--
-- As 'view' is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restricted signatures:
--
-- @
-- 'view' ::             'Getter' s a     -> s -> a
-- 'view' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Fold.Fold' s m       -> s -> m
-- 'view' ::             'Control.Lens.Iso.Iso'' s a       -> s -> a
-- 'view' ::             'Lens'' s a      -> s -> a
-- 'view' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Traversal.Traversal'' s m -> s -> m
-- @
--
-- In a more general setting, such as when working with a 'Monad' transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'Getter' s a     -> m a
-- 'view' :: ('MonadReader' s m, 'Data.Monoid.Monoid' a) => 'Control.Lens.Fold.Fold' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Lens'' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Data.Monoid.Monoid' a) => 'Control.Lens.Traversal.Traversal'' s a -> m a
-- @
view :: MonadReader s m => Getting a s a -> m a
view l = Reader.asks (getConst #. l Const)
{-# INLINE view #-}

-- | View a function of the value pointed to by a 'Getter' or 'Lens' or the result of
-- folding over the result of mapping the targets of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal'.
--
-- @
-- 'views' l f ≡ 'view' (l '.' 'to' f)
-- @
--
-- >>> views (to f) g a
-- g (f a)
--
-- >>> views _2 length (1,"hello")
-- 5
--
-- As 'views' is commonly used to access the target of a 'Getter' or obtain a monoidal summary of the targets of a 'Fold',
-- It may be useful to think of it as having one of these more restricted signatures:
--
-- @
-- 'views' ::             'Getter' s a     -> (a -> r) -> s -> r
-- 'views' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Fold.Fold' s a       -> (a -> m) -> s -> m
-- 'views' ::             'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> s -> r
-- 'views' ::             'Lens'' s a      -> (a -> r) -> s -> r
-- 'views' :: 'Data.Monoid.Monoid' m => 'Control.Lens.Traversal.Traversal'' s a -> (a -> m) -> s -> m
-- @
--
-- In a more general setting, such as when working with a 'Monad' transformer stack you can use:
--
-- @
-- 'views' :: 'MonadReader' s m             => 'Getter' s a     -> (a -> r) -> m r
-- 'views' :: ('MonadReader' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Fold.Fold' s a       -> (a -> r) -> m r
-- 'views' :: 'MonadReader' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> m r
-- 'views' :: 'MonadReader' s m             => 'Lens'' s a      -> (a -> r) -> m r
-- 'views' :: ('MonadReader' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @
-- 'views' :: 'MonadReader' s m => 'Getting' r s a -> (a -> r) -> m r
-- @
views :: MonadReader s m => LensLike' (Const r) s a -> (a -> r) -> m r
views l f = Reader.asks (coerce l f)
{-# INLINE views #-}

-- | View the value pointed to by a 'Getter' or 'Lens' or the
-- result of folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..').
--
-- >>> (a,b)^._2
-- b
--
-- >>> ("hello","world")^._2
-- "world"
--
-- >>> import Data.Complex
-- >>> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- 2.23606797749979
--
-- @
-- ('^.') ::             s -> 'Getter' s a     -> a
-- ('^.') :: 'Data.Monoid.Monoid' m => s -> 'Control.Lens.Fold.Fold' s m       -> m
-- ('^.') ::             s -> 'Control.Lens.Iso.Iso'' s a       -> a
-- ('^.') ::             s -> 'Lens'' s a      -> a
-- ('^.') :: 'Data.Monoid.Monoid' m => s -> 'Control.Lens.Traversal.Traversal'' s m -> m
-- @
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

-------------------------------------------------------------------------------
-- MonadState
-------------------------------------------------------------------------------

-- | Use the target of a 'Lens', 'Control.Lens.Iso.Iso', or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that points
-- to a monoidal value.
--
-- >>> evalState (use _1) (a,b)
-- a
--
-- >>> evalState (use _1) ("hello","world")
-- "hello"
--
-- @
-- 'use' :: 'MonadState' s m             => 'Getter' s a     -> m a
-- 'use' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Fold.Fold' s r       -> m r
-- 'use' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> m a
-- 'use' :: 'MonadState' s m             => 'Lens'' s a      -> m a
-- 'use' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Traversal.Traversal'' s r -> m r
-- @
use :: MonadState s m => Getting a s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

-- | Use the target of a 'Lens', 'Control.Lens.Iso.Iso' or
-- 'Getter' in the current state, or use a summary of a
-- 'Control.Lens.Fold.Fold' or 'Control.Lens.Traversal.Traversal' that
-- points to a monoidal value.
--
-- >>> evalState (uses _1 length) ("hello","world")
-- 5
--
-- @
-- 'uses' :: 'MonadState' s m             => 'Getter' s a     -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Fold.Fold' s a       -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Lens'' s a      -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Control.Lens.Iso.Iso'' s a       -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.Lens.Traversal.Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @
-- 'uses' :: 'MonadState' s m => 'Getting' r s t a b -> (a -> r) -> m r
-- @
uses :: MonadState s m => LensLike' (Const r) s a -> (a -> r) -> m r
uses l f = State.gets (views l f)
{-# INLINE uses #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'listening' :: 'MonadWriter' w m             => 'Getter' w u     -> m a -> m (a, u)
-- 'listening' :: 'MonadWriter' w m             => 'Lens'' w u      -> m a -> m (a, u)
-- 'listening' :: 'MonadWriter' w m             => 'Iso'' w u       -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'Fold' w u       -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'Traversal'' w u -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'Prism'' w u     -> m a -> m (a, u)
-- @
listening :: MonadWriter w m => Getting u w u -> m a -> m (a, u)
listening l m = do
  (a, w) <- listen m
  return (a, view l w)
{-# INLINE listening #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'ilistening' :: 'MonadWriter' w m             => 'IndexedGetter' i w u     -> m a -> m (a, (i, u))
-- 'ilistening' :: 'MonadWriter' w m             => 'IndexedLens'' i w u      -> m a -> m (a, (i, u))
-- 'ilistening' :: ('MonadWriter' w m, 'Monoid' u) => 'IndexedFold' i w u       -> m a -> m (a, (i, u))
-- 'ilistening' :: ('MonadWriter' w m, 'Monoid' u) => 'IndexedTraversal'' i w u -> m a -> m (a, (i, u))
-- @
ilistening :: MonadWriter w m => IndexedGetting i (i, u) w u -> m a -> m (a, (i, u))
ilistening l m = do
  (a, w) <- listen m
  return (a, iview l w)
{-# INLINE ilistening #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'listenings' :: 'MonadWriter' w m             => 'Getter' w u     -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: 'MonadWriter' w m             => 'Lens'' w u      -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: 'MonadWriter' w m             => 'Iso'' w u       -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'Fold' w u       -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'Traversal'' w u -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'Prism'' w u     -> (u -> v) -> m a -> m (a, v)
-- @
listenings :: MonadWriter w m => Getting v w u -> (u -> v) -> m a -> m (a, v)
listenings l uv m = do
  (a, w) <- listen m
  return (a, views l uv w)
{-# INLINE listenings #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'ilistenings' :: 'MonadWriter' w m             => 'IndexedGetter' w u     -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: 'MonadWriter' w m             => 'IndexedLens'' w u      -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: ('MonadWriter' w m, 'Monoid' v) => 'IndexedFold' w u       -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: ('MonadWriter' w m, 'Monoid' v) => 'IndexedTraversal'' w u -> (i -> u -> v) -> m a -> m (a, v)
-- @
ilistenings :: MonadWriter w m => IndexedGetting i v w u -> (i -> u -> v) -> m a -> m (a, v)
ilistenings l iuv m = do
  (a, w) <- listen m
  return (a, iviews l iuv w)
{-# INLINE ilistenings #-}

------------------------------------------------------------------------------
-- Indexed Getters
------------------------------------------------------------------------------

-- | View the index and value of an 'IndexedGetter' into the current environment as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iview :: MonadReader s m => IndexedGetting i (i,a) s a -> m (i,a)
iview l = asks (getConst #. l (Indexed $ \i -> Const #. (,) i))
{-# INLINE iview #-}

-- | View a function of the index and value of an 'IndexedGetter' into the current environment.
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
--
-- @
-- 'iviews' ≡ 'Control.Lens.Fold.ifoldMapOf'
-- @
iviews :: MonadReader s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iviews l f = asks (coerce l f)
{-# INLINE iviews #-}

-- | Use the index and value of an 'IndexedGetter' into the current state as a pair.
--
-- When applied to an 'IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iuse :: MonadState s m => IndexedGetting i (i,a) s a -> m (i,a)
iuse l = gets (getConst #. l (Indexed $ \i -> Const #. (,) i))
{-# INLINE iuse #-}

-- | Use a function of the index and value of an 'IndexedGetter' into the current state.
--
-- When applied to an 'IndexedFold' the result will be a monoidal summary instead of a single answer.
iuses :: MonadState s m => IndexedGetting i r s a -> (i -> a -> r) -> m r
iuses l f = gets (coerce l f)
{-# INLINE iuses #-}

-- | View the index and value of an 'IndexedGetter' or 'IndexedLens'.
--
-- This is the same operation as 'iview' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..').
--
-- @
-- ('^@.') :: s -> 'IndexedGetter' i s a -> (i, a)
-- ('^@.') :: s -> 'IndexedLens'' i s a  -> (i, a)
-- @
--
-- The result probably doesn't have much meaning when applied to an 'IndexedFold'.
(^@.) :: s -> IndexedGetting i (i, a) s a -> (i, a)
s ^@. l = getConst $ l (Indexed $ \i -> Const #. (,) i) s
{-# INLINE (^@.) #-}

-- | Coerce a 'Getter'-compatible 'Optical' to an 'Optical''. This
-- is useful when using a 'Traversal' that is not simple as a 'Getter' or a
-- 'Fold'.
--
-- @
-- 'getting' :: 'Traversal' s t a b          -> 'Fold' s a
-- 'getting' :: 'Lens' s t a b               -> 'Getter' s a
-- 'getting' :: 'IndexedTraversal' i s t a b -> 'IndexedFold' i s a
-- 'getting' :: 'IndexedLens' i s t a b      -> 'IndexedGetter' i s a
-- @
getting :: (Profunctor p, Profunctor q, Functor f, Contravariant f)
        => Optical p q f s t a b -> Optical' p q f s a
getting l f = rmap phantom . l $ rmap phantom f

----------------------------------------------------------------------------
-- Profunctors
----------------------------------------------------------------------------

-- | Formally, the class 'Profunctor' represents a profunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where the first argument is contravariant
-- and the second argument is covariant.
--
-- You can define a 'Profunctor' by either defining 'dimap' or by defining both
-- 'lmap' and 'rmap'.
--
-- If you supply 'dimap', you should ensure that:
--
-- @'dimap' 'id' 'id' ≡ 'id'@
--
-- If you supply 'lmap' and 'rmap', ensure:
--
-- @
-- 'lmap' 'id' ≡ 'id'
-- 'rmap' 'id' ≡ 'id'
-- @
--
-- If you supply both, you should also ensure:
--
-- @'dimap' f g ≡ 'lmap' f '.' 'rmap' g@
--
-- These ensure by parametricity:
--
-- @
-- 'dimap' (f '.' g) (h '.' i) ≡ 'dimap' g h '.' 'dimap' f i
-- 'lmap' (f '.' g) ≡ 'lmap' g '.' 'lmap' f
-- 'rmap' (f '.' g) ≡ 'rmap' f '.' 'rmap' g
-- @
class Profunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'dimap' f g ≡ 'lmap' f '.' 'rmap' g@
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  -- | Map the first argument contravariantly.
  --
  -- @'lmap' f ≡ 'dimap' f 'id'@
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}

  -- | Map the second argument covariantly.
  --
  -- @'rmap' ≡ 'dimap' 'id'@
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}

  -- | Strictly map the second argument argument
  -- covariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a first argument that is
  -- operationally identity.
  --
  -- The semantics of this function with respect to bottoms
  -- should match the default definition:
  --
  -- @('Profuctor.Unsafe.#.') ≡ \\_ -> \\p -> p \`seq\` 'rmap' 'coerce' p@
  (#.) :: forall a b c q. Coercible c b => q b c -> p a b -> p a c
  (#.) = \_ -> \p -> p `seq` rmap (coerce (id :: c -> c) :: b -> c) p
  {-# INLINE (#.) #-}

  -- | Strictly map the first argument argument
  -- contravariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a second argument that is
  -- operationally identity.
  --
  -- @('.#') ≡ \\p -> p \`seq\` \\f -> 'lmap' 'coerce' p@
  (.#) :: forall a b c q. Coercible b a => p b c -> q a b -> p a c
  (.#) = \p -> p `seq` \_ -> lmap (coerce (id :: b -> b) :: a -> b) p
  {-# INLINE (.#) #-}

  {-# MINIMAL dimap | (lmap, rmap) #-}

------------------------------------------------------------------------------
-- Conjoined
------------------------------------------------------------------------------

-- | This is a 'Profunctor' that is both 'Corepresentable' by @f@ and 'Representable' by @g@ such
-- that @f@ is left adjoint to @g@. From this you can derive a lot of structure due
-- to the preservation of limits and colimits.
class
  ( Choice p, Corepresentable p, Comonad (Corep p), Traversable (Corep p)
  , Strong p, Representable p, Monad (Rep p), MonadFix (Rep p), Costrong p, ArrowLoop p, ArrowApply p, ArrowChoice p
  ) => Conjoined p where

  -- | 'Conjoined' is strong enough to let us distribute every 'Conjoined'
  -- 'Profunctor' over every Haskell 'Functor'. This is effectively a
  -- generalization of 'fmap'.
  distrib :: Functor f => p a b -> p (f a) (f b)
  distrib = tabulate . collect . sieve
  {-# INLINE distrib #-}

  -- | This permits us to make a decision at an outermost point about whether or not we use an index.
  --
  -- Ideally any use of this function should be done in such a way so that you compute the same answer,
  -- but this cannot be enforced at the type level.
  conjoined :: ((p ~ (->)) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
  conjoined _ r = r
  {-# INLINE conjoined #-}

instance Conjoined (->) where
  distrib = fmap
  {-# INLINE distrib #-}
  conjoined l _ = l
  {-# INLINE conjoined #-}

----------------------------------------------------------------------------
-- Indexable
----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Conjoined p => Indexable i p where
  -- | Build a function from an 'indexed' function.
  indexed :: p a b -> i -> a -> b

instance Indexable i (->) where
  indexed = const
  {-# INLINE indexed #-}

-----------------------------------------------------------------------------
-- Indexed Internals
-----------------------------------------------------------------------------

-- | A function with access to a index. This constructor may be useful when you need to store
-- an 'Indexable' in a container to avoid @ImpredicativeTypes@.
--
-- @index :: Indexed i a b -> i -> a -> b@
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Functor (Indexed i a) where
  fmap g (Indexed f) = Indexed $ \i a -> g (f i a)
  {-# INLINE fmap #-}

instance Apply (Indexed i a) where
  Indexed f <.> Indexed g = Indexed $ \i a -> f i a (g i a)
  {-# INLINE (<.>) #-}

instance Applicative (Indexed i a) where
  pure b = Indexed $ \_ _ -> b
  {-# INLINE pure #-}
  Indexed f <*> Indexed g = Indexed $ \i a -> f i a (g i a)
  {-# INLINE (<*>) #-}

instance Monad (Indexed i a) where
  return = pure
  {-# INLINE return #-}
  Indexed f >>= k = Indexed $ \i a -> runIndexed (k (f i a)) i a
  {-# INLINE (>>=) #-}

instance MonadFix (Indexed i a) where
  mfix f = Indexed $ \ i a -> let o = runIndexed (f o) i a in o
  {-# INLINE mfix #-}

instance Profunctor (Indexed i) where
  dimap ab cd ibc = Indexed $ \i -> cd . runIndexed ibc i . ab
  {-# INLINE dimap #-}
  lmap ab ibc = Indexed $ \i -> runIndexed ibc i . ab
  {-# INLINE lmap #-}
  rmap bc iab = Indexed $ \i -> bc . runIndexed iab i
  {-# INLINE rmap #-}
  (.#) ibc _ = coerce ibc
  {-# INLINE (.#) #-}
  (#.) _ = coerce
  {-# INLINE (#.) #-}

instance Costrong (Indexed i) where
  unfirst (Indexed iadbd) = Indexed $ \i a -> let
      (b, d) = iadbd i (a, d)
    in b

instance Sieve (Indexed i) ((->) i) where
  sieve = flip . runIndexed
  {-# INLINE sieve #-}

instance Representable (Indexed i) where
  type Rep (Indexed i) = (->) i
  tabulate = Indexed . flip
  {-# INLINE tabulate #-}

instance Cosieve (Indexed i) ((,) i) where
  cosieve = uncurry . runIndexed
  {-# INLINE cosieve #-}

instance Corepresentable (Indexed i) where
  type Corep (Indexed i) = (,) i
  cotabulate = Indexed . curry
  {-# INLINE cotabulate #-}

instance Choice (Indexed i) where
  right' = right
  {-# INLINE right' #-}

instance Strong (Indexed i) where
  second' = Arrow.second
  {-# INLINE second' #-}

instance C.Category (Indexed i) where
  id = Indexed (const id)
  {-# INLINE id #-}
  Indexed f . Indexed g = Indexed $ \i -> f i . g i
  {-# INLINE (.) #-}

instance Arrow (Indexed i) where
  arr f = Indexed (\_ -> f)
  {-# INLINE arr #-}
  first f = Indexed (Arrow.first . runIndexed f)
  {-# INLINE first #-}
  second f = Indexed (Arrow.second . runIndexed f)
  {-# INLINE second #-}
  Indexed f *** Indexed g = Indexed $ \i -> f i *** g i
  {-# INLINE (***) #-}
  Indexed f &&& Indexed g = Indexed $ \i -> f i &&& g i
  {-# INLINE (&&&) #-}

instance ArrowChoice (Indexed i) where
  left f = Indexed (left . runIndexed f)
  {-# INLINE left #-}
  right f = Indexed (right . runIndexed f)
  {-# INLINE right #-}
  Indexed f +++ Indexed g = Indexed $ \i -> f i +++ g i
  {-# INLINE (+++)  #-}
  Indexed f ||| Indexed g = Indexed $ \i -> f i ||| g i
  {-# INLINE (|||) #-}

instance ArrowApply (Indexed i) where
  app = Indexed $ \ i (f, b) -> runIndexed f i b
  {-# INLINE app #-}

instance ArrowLoop (Indexed i) where
  loop (Indexed f) = Indexed $ \i b -> let (c,d) = f i (b, d) in c
  {-# INLINE loop #-}

instance Conjoined (Indexed i) where
  distrib (Indexed iab) = Indexed $ \i fa -> iab i <$> fa
  {-# INLINE distrib #-}

instance i ~ j => Indexable i (Indexed j) where
  indexed = runIndexed
  {-# INLINE indexed #-}

------------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed'.
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing f) where
  Indexing mf <.> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing f) where
  contramap f (Indexing m) = Indexing $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

instance Semigroup (f a) => Semigroup (Indexing f a) where
    Indexing mx <> Indexing my = Indexing $ \i -> case mx i of
      (j, x) -> case my j of
         ~(k, y) -> (k, x <> y)
    {-# INLINE (<>) #-}

-- |
--
-- >>> "cat" ^@.. (folded <> folded)
-- [(0,'c'),(1,'a'),(2,'t'),(0,'c'),(1,'a'),(2,'t')]
--
-- >>> "cat" ^@.. indexing (folded <> folded)
-- [(0,'c'),(1,'a'),(2,'t'),(3,'c'),(4,'a'),(5,'t')]
instance Monoid (f a) => Monoid (Indexing f a) where
    mempty = Indexing $ \i -> (i, mempty)
    {-# INLINE mempty #-}

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- @
-- 'indexing' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedTraversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedLens' 'Int'  s t a b
-- 'indexing' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedLens' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.IndexedFold' 'Int' s a
-- 'indexing' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.IndexedGetter' 'Int' s a
-- @
--
-- @'indexing' :: 'Indexable' 'Int' p => 'Control.Lens.Type.LensLike' ('Indexing' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
indexing l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing #-}

------------------------------------------------------------------------------
-- Indexing64
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int64'@ with a 'Functor', used
-- by 'Control.Lens.Indexed.indexed64'.
newtype Indexing64 f a = Indexing64 { runIndexing64 :: Int64 -> (Int64, f a) }

instance Functor f => Functor (Indexing64 f) where
  fmap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing64 f) where
  Indexing64 mf <.> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing64 f) where
  pure x = Indexing64 $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing64 mf <*> Indexing64 ma = Indexing64 $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing64 f) where
  contramap f (Indexing64 m) = Indexing64 $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.IndexedTraversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.IndexedFold', etc.
--
-- This combinator is like 'indexing' except that it handles large traversals and folds gracefully.
--
-- @
-- 'indexing64' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.IndexedTraversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.IndexedLens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.IndexedFold' 'Int64' s a
-- 'indexing64' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.IndexedGetter' 'Int64' s a
-- @
--
-- @'indexing64' :: 'Indexable' 'Int64' p => 'Control.Lens.Type.LensLike' ('Indexing64' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing64 :: Indexable Int64 p => ((a -> Indexing64 f b) -> s -> Indexing64 f t) -> p a (f b) -> s -> f t
indexing64 l iafb s = snd $ runIndexing64 (l (\a -> Indexing64 (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing64 #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Fold a container with indices returning both the indices and the values.
--
-- The result is only valid to compose in a 'Traversal', if you don't edit the
-- index as edits to the index have no effect.
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex
-- [(0,10),(1,20),(2,30)]
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex . alongside negated (re _Show)
-- [(0,"10"),(-1,"20"),(-2,"30")]
--
withIndex :: (Indexable i p, Functor f) => p (i, s) (f (j, t)) -> Indexed i s (f t)
withIndex f = Indexed $ \i a -> snd <$> indexed f i (i, a)
{-# INLINE withIndex #-}

-- | When composed with an 'IndexedFold' or 'IndexedTraversal' this yields an
-- ('Indexed') 'Fold' of the indices.
asIndex :: (Indexable i p, Contravariant f, Functor f) => p i (f i) -> Indexed i s (f s)
asIndex f = Indexed $ \i _ -> phantom (indexed f i i)
{-# INLINE asIndex #-}

-- | A 'Lens' is actually a lens family as described in
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the
-- three common sense 'Lens' laws:
--
-- 1) You get back what you put in:
--
-- @
-- 'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l v s)  ≡ v
-- @
--
-- 2) Putting back what you got doesn't change anything:
--
-- @
-- 'Control.Lens.Setter.set' l ('Control.Lens.Getter.view' l s) s  ≡ s
-- @
--
-- 3) Setting twice is the same as setting once:
--
-- @
-- 'Control.Lens.Setter.set' l v' ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v' s
-- @
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- There are some emergent properties of these laws:
--
-- 1) @'Control.Lens.Setter.set' l s@ must be injective for every @s@ This is a consequence of law #1
--
-- 2) @'Control.Lens.Setter.set' l@ must be surjective, because of law #2, which indicates that it is possible to obtain any 'v' from some 's' such that @'Control.Lens.Setter.set' s v = s@
--
-- 3) Given just the first two laws you can prove a weaker form of law #3 where the values @v@ that you are setting match:
--
-- @
-- 'Control.Lens.Setter.set' l v ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v s
-- @
--
-- Every 'Lens' can be used directly as a 'Control.Lens.Setter.Setter' or 'Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Fold' or 'Getter'.
--
-- Since every 'Lens' is a valid 'Traversal', the
-- 'Traversal' laws are required of any 'Lens' you create:
--
-- @
-- l 'pure' ≡ 'pure'
-- 'fmap' (l f) '.' l g ≡ 'Data.Functor.Compose.getCompose' '.' l ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- @
-- type 'Lens' s t a b = forall f. 'Functor' f => 'LensLike' f s t a b
-- @
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | @
-- type 'Lens'' = 'Simple' 'Lens'
-- @
type Lens' s a = Lens s s a a

-- | Every 'IndexedLens' is a valid 'Lens' and a valid 'Control.Lens.Traversal.IndexedTraversal'.
type IndexedLens i s t a b = forall f p. (Indexable i p, Functor f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedLens'' i = 'Simple' ('IndexedLens' i)
-- @
type IndexedLens' i s a = IndexedLens i s s a a

-- | An 'IndexPreservingLens' leaves any index it is composed with alone.
type IndexPreservingLens s t a b = forall p f. (Conjoined p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexPreservingLens'' = 'Simple' 'IndexPreservingLens'
-- @
type IndexPreservingLens' s a = IndexPreservingLens s s a a

------------------------------------------------------------------------------
-- Traversals
------------------------------------------------------------------------------

-- | A 'Traversal' can be used directly as a 'Control.Lens.Setter.Setter' or a 'Fold' (but not as a 'Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'Traversal' laws.
--
-- These have also been known as multilenses, but they have the signature and spirit of
--
-- @
-- 'Data.Traversable.traverse' :: 'Data.Traversable.Traversable' f => 'Traversal' (f a) (f b) a b
-- @
--
-- and the more evocative name suggests their application.
--
-- Most of the time the 'Traversal' you will want to use is just 'Data.Traversable.traverse', but you can also pass any
-- 'Lens' or 'Iso' as a 'Traversal', and composition of a 'Traversal' (or 'Lens' or 'Iso') with a 'Traversal' (or 'Lens' or 'Iso')
-- using ('.') forms a valid 'Traversal'.
--
-- The laws for a 'Traversal' @t@ follow from the laws for 'Data.Traversable.Traversable' as stated in \"The Essence of the Iterator Pattern\".
--
-- @
-- t 'pure' ≡ 'pure'
-- 'fmap' (t f) '.' t g ≡ 'Data.Functor.Compose.getCompose' '.' t ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- One consequence of this requirement is that a 'Traversal' needs to leave the same number of elements as a
-- candidate for subsequent 'Traversal' that it started with. Another testament to the strength of these laws
-- is that the caveat expressed in section 5.5 of the \"Essence of the Iterator Pattern\" about exotic
-- 'Data.Traversable.Traversable' instances that 'Data.Traversable.traverse' the same entry multiple times was actually already ruled out by the
-- second law in that same paper!
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | @
-- type 'Traversal'' = 'Simple' 'Traversal'
-- @
type Traversal' s a = Traversal s s a a

-- | A 'Traversal' which targets at least one element.
--
-- Note that since 'Apply' is not a superclass of 'Applicative', a 'Traversal1'
-- cannot always be used in place of a 'Traversal'. In such circumstances
-- 'Control.Lens.Traversal.cloneTraversal' will convert a 'Traversal1' into a 'Traversal'.
type Traversal1 s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type Traversal1' s a = Traversal1 s s a a

-- | Every 'IndexedTraversal' is a valid 'Control.Lens.Traversal.Traversal' or
-- 'Control.Lens.Fold.IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'IndexedTraversal' to be used
-- directly as a 'Control.Lens.Traversal.Traversal'.
--
-- The 'Control.Lens.Traversal.Traversal' laws are still required to hold.
--
-- In addition, the index @i@ should satisfy the requirement that it stays
-- unchanged even when modifying the value @a@, otherwise traversals like
-- 'indices' break the 'Traversal' laws.
type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedTraversal'' i = 'Simple' ('IndexedTraversal' i)
-- @
type IndexedTraversal' i s a = IndexedTraversal i s s a a

type IndexedTraversal1 i s t a b = forall p f. (Indexable i p, Apply f) => p a (f b) -> s -> f t
type IndexedTraversal1' i s a = IndexedTraversal1 i s s a a

-- | An 'IndexPreservingLens' leaves any index it is composed with alone.
type IndexPreservingTraversal s t a b = forall p f. (Conjoined p, Applicative f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexPreservingTraversal'' = 'Simple' 'IndexPreservingTraversal'
-- @
type IndexPreservingTraversal' s a = IndexPreservingTraversal s s a a

type IndexPreservingTraversal1 s t a b = forall p f. (Conjoined p, Apply f) => p a (f b) -> p s (f t)
type IndexPreservingTraversal1' s a = IndexPreservingTraversal1 s s a a

------------------------------------------------------------------------------
-- Setters
------------------------------------------------------------------------------

-- | The only 'LensLike' law that can apply to a 'Setter' @l@ is that
--
-- @
-- 'Control.Lens.Setter.set' l y ('Control.Lens.Setter.set' l x a) ≡ 'Control.Lens.Setter.set' l y a
-- @
--
-- You can't 'Control.Lens.Getter.view' a 'Setter' in general, so the other two laws are irrelevant.
--
-- However, two 'Functor' laws apply to a 'Setter':
--
-- @
-- 'Control.Lens.Setter.over' l 'id' ≡ 'id'
-- 'Control.Lens.Setter.over' l f '.' 'Control.Lens.Setter.over' l g ≡ 'Control.Lens.Setter.over' l (f '.' g)
-- @
--
-- These can be stated more directly:
--
-- @
-- l 'pure' ≡ 'pure'
-- l f '.' 'untainted' '.' l g ≡ l (f '.' 'untainted' '.' g)
-- @
--
-- You can compose a 'Setter' with a 'Lens' or a 'Traversal' using ('.') from the @Prelude@
-- and the result is always only a 'Setter' and nothing more.
--
-- >>> over traverse f [a,b,c,d]
-- [f a,f b,f c,f d]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over (traverse._1) f [(a,b),(c,d)]
-- [(f a,b),(f c,d)]
--
-- >>> over both f (a,b)
-- (f a,f b)
--
-- >>> over (traverse.both) f [(a,b),(c,d)]
-- [(f a,f b),(f c,f d)]
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- | A 'Setter'' is just a 'Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @
-- 'sets' Data.Text.map :: 'Setter'' 'Data.Text.Internal.Text' 'Char'
-- @
--
-- @
-- type 'Setter'' = 'Simple' 'Setter'
-- @
type Setter' s a = Setter s s a a

-- | Every 'IndexedSetter' is a valid 'Setter'.
--
-- The 'Setter' laws are still required to hold.
type IndexedSetter i s t a b = forall f p.
  (Indexable i p, Settable f) => p a (f b) -> s -> f t

-- | @
-- type 'IndexedSetter'' i = 'Simple' ('IndexedSetter' i)
-- @
type IndexedSetter' i s a = IndexedSetter i s s a a

-- | An 'IndexPreservingSetter' can be composed with a 'IndexedSetter', 'IndexedTraversal' or 'IndexedLens'
-- and leaves the index intact, yielding an 'IndexedSetter'.
type IndexPreservingSetter s t a b = forall p f. (Conjoined p, Settable f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexedPreservingSetter'' i = 'Simple' 'IndexedPreservingSetter'
-- @
type IndexPreservingSetter' s a = IndexPreservingSetter s s a a

-----------------------------------------------------------------------------
-- Isomorphisms
-----------------------------------------------------------------------------

-- | Isomorphism families can be composed with another 'Lens' using ('.') and 'id'.
--
-- Since every 'Iso' is both a valid 'Lens' and a valid 'Prism', the laws for those types
-- imply the following laws for an 'Iso' 'f':
--
-- @
-- f '.' 'Control.Lens.Iso.from' f ≡ 'id'
-- 'Control.Lens.Iso.from' f '.' f ≡ 'id'
-- @
--
-- Note: Composition with an 'Iso' is index- and measure- preserving.
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'Iso'' = 'Control.Lens.Type.Simple' 'Iso'
-- @
type Iso' s a = Iso s s a a

------------------------------------------------------------------------------
-- Review Internals
------------------------------------------------------------------------------

-- | This is a limited form of a 'Prism' that can only be used for 're' operations.
--
-- Like with a 'Getter', there are no laws to state for a 'Review'.
--
-- You can generate a 'Review' by using 'unto'. You can also use any 'Prism' or 'Iso'
-- directly as a 'Review'.
type Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => Optic' p f t b

-- | If you see this in a signature for a function, the function is expecting a 'Review'
-- (in practice, this usually means a 'Prism').
type AReview t b = Optic' Tagged Identity t b

------------------------------------------------------------------------------
-- Prism Internals
------------------------------------------------------------------------------

-- | A 'Prism' @l@ is a 'Traversal' that can also be turned
-- around with 'Control.Lens.Review.re' to obtain a 'Getter' in the
-- opposite direction.
--
-- There are three laws that a 'Prism' should satisfy:
--
-- First, if I 'Control.Lens.Review.re' or 'Control.Lens.Review.review' a value with a 'Prism' and then 'Control.Lens.Fold.preview' or use ('Control.Lens.Fold.^?'), I will get it back:
--
-- @
-- 'Control.Lens.Fold.preview' l ('Control.Lens.Review.review' l b) ≡ 'Just' b
-- @
--
-- Second, if you can extract a value @a@ using a 'Prism' @l@ from a value @s@, then the value @s@ is completely described by @l@ and @a@:
--
-- @
-- 'Control.Lens.Fold.preview' l s ≡ 'Just' a ⟹ 'Control.Lens.Review.review' l a ≡ s
-- @
--
-- Third, if you get non-match @t@, you can convert it result back to @s@:
--
-- @
-- 'Control.Lens.Combinators.matching' l s ≡ 'Left' t ⟹ 'Control.Lens.Combinators.matching' l t ≡ 'Left' s
-- @
--
-- The first two laws imply that the 'Traversal' laws hold for every 'Prism' and that we 'Data.Traversable.traverse' at most 1 element:
--
-- @
-- 'Control.Lens.Fold.lengthOf' l x '<=' 1
-- @
--
-- It may help to think of this as an 'Iso' that can be partial in one direction.
--
-- Every 'Prism' is a valid 'Traversal'.
--
-- Every 'Iso' is a valid 'Prism'.
--
-- For example, you might have a @'Prism'' 'Integer' 'Numeric.Natural.Natural'@ allows you to always
-- go from a 'Numeric.Natural.Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Numeric.Natural.Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'Prism'' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'Control.Lens.Prism.prism' 'toInteger' '$' \\ i ->
--    if i '<' 0
--    then 'Left' i
--    else 'Right' ('fromInteger' i)
-- @
--
-- Now we can ask if an 'Integer' is a 'Numeric.Natural.Natural'.
--
-- >>> 5^?nat
-- Just 5
--
-- >>> (-5)^?nat
-- Nothing
--
-- We can update the ones that are:
--
-- >>> (-3,4) & both.nat *~ 2
-- (-3,8)
--
-- And we can then convert from a 'Numeric.Natural.Natural' to an 'Integer'.
--
-- >>> 5 ^. re nat -- :: Natural
-- 5
--
-- Similarly we can use a 'Prism' to 'Data.Traversable.traverse' the 'Left' half of an 'Either':
--
-- >>> Left "hello" & _Left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.re _Left
-- Left 5
--
-- such that if you query it with the 'Prism', you will get your original input back.
--
-- >>> 5^.re _Left ^? _Left
-- Just 5
--
-- Another interesting way to think of a 'Prism' is as the categorical dual of a 'Lens'
-- -- a co-'Lens', so to speak. This is what permits the construction of 'Control.Lens.Prism.outside'.
--
-- Note: Composition with a 'Prism' is index-preserving.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A 'Simple' 'Prism'.
type Prism' s a = Prism s s a a

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | A witness that @(a ~ s, b ~ t)@.
--
-- Note: Composition with an 'Equality' is index-preserving.
type Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall k3 (p :: k1 -> k3 -> Type) (f :: k2 -> k3) .
    p a (f b) -> p s (f t)

-- | A 'Simple' 'Equality'.
type Equality' s a = Equality s s a a

-- | Composable `asTypeOf`. Useful for constraining excess
-- polymorphism, @foo . (id :: As Int) . bar@.
type As a = Equality' a a

-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------

-- | A 'Getter' describes how to retrieve a single value in a way that can be
-- composed with other 'LensLike' constructions.
--
-- Unlike a 'Lens' a 'Getter' is read-only. Since a 'Getter'
-- cannot be used to write back there are no 'Lens' laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(s -> a)@.
--
-- Moreover, a 'Getter' can be used directly as a 'Control.Lens.Fold.Fold',
-- since it just ignores the 'Applicative'.
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | Every 'IndexedGetter' is a valid 'Control.Lens.Fold.IndexedFold' and can be used for 'Control.Lens.Getter.Getting' like a 'Getter'.
type IndexedGetter i s a = forall p f. (Indexable i p, Contravariant f, Functor f) => p a (f a) -> s -> f s

-- | An 'IndexPreservingGetter' can be used as a 'Getter', but when composed with an 'IndexedTraversal',
-- 'IndexedFold', or 'IndexedLens' yields an 'IndexedFold', 'IndexedFold' or 'IndexedGetter' respectively.
type IndexPreservingGetter s a = forall p f. (Conjoined p, Contravariant f, Functor f) => p a (f a) -> p s (f s)

--------------------------
-- Folds
--------------------------

-- | A 'Fold' describes how to retrieve multiple values in a way that can be composed
-- with other 'LensLike' constructions.
--
-- A @'Fold' s a@ provides a structure with operations very similar to those of the 'Data.Foldable.Foldable'
-- typeclass, see 'Control.Lens.Fold.foldMapOf' and the other 'Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Data.Foldable.Foldable' (f a)@, then there should be a
-- @fooOf@ method that takes a @'Fold' s a@ and a value of type @s@.
--
-- A 'Getter' is a legal 'Fold' that just ignores the supplied 'Data.Monoid.Monoid'.
--
-- Unlike a 'Control.Lens.Traversal.Traversal' a 'Fold' is read-only. Since a 'Fold' cannot be used to write back
-- there are no 'Lens' laws that apply.
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- | Every 'IndexedFold' is a valid 'Control.Lens.Fold.Fold' and can be used for 'Control.Lens.Getter.Getting'.
type IndexedFold i s a = forall p f.  (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

-- | An 'IndexPreservingFold' can be used as a 'Fold', but when composed with an 'IndexedTraversal',
-- 'IndexedFold', or 'IndexedLens' yields an 'IndexedFold' respectively.
type IndexPreservingFold s a = forall p f. (Conjoined p, Contravariant f, Applicative f) => p a (f a) -> p s (f s)

-- | A relevant Fold (aka 'Fold1') has one or more targets.
type Fold1 s a = forall f. (Contravariant f, Apply f) => (a -> f a) -> s -> f s
type IndexedFold1 i s a = forall p f.  (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s
type IndexPreservingFold1 s a = forall p f. (Conjoined p, Contravariant f, Apply f) => p a (f a) -> p s (f s)

-------------------------------------------------------------------------------
-- Simple Overloading
-------------------------------------------------------------------------------

-- | A 'Simple' 'Lens', 'Simple' 'Traversal', ... can
-- be used instead of a 'Lens','Traversal', ...
-- whenever the type variables don't change upon setting a value.
--
-- @
-- 'Data.Complex.Lens._imagPart' :: 'Simple' 'Lens' ('Data.Complex.Complex' a) a
-- 'Control.Lens.Traversal.traversed' :: 'Simple' ('IndexedTraversal' 'Int') [a] a
-- @
--
-- Note: To use this alias in your own code with @'LensLike' f@ or
-- 'Setter', you may have to turn on @LiberalTypeSynonyms@.
--
-- This is commonly abbreviated as a \"prime\" marker, /e.g./ 'Lens'' = 'Simple' 'Lens'.
type Simple f s a = f s s a a

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------

-- | A valid 'Optic' @l@ should satisfy the laws:
--
-- @
-- l 'pure' ≡ 'pure'
-- l ('Procompose' f g) = 'Procompose' (l f) (l g)
-- @
--
-- This gives rise to the laws for 'Equality', 'Iso', 'Prism', 'Lens',
-- 'Traversal', 'Traversal1', 'Setter', 'Fold', 'Fold1', and 'Getter' as well
-- along with their index-preserving variants.
--
-- @
-- type 'LensLike' f s t a b = 'Optic' (->) f s t a b
-- @
type Optic p f s t a b = p a (f b) -> p s (f t)

-- | @
-- type 'Optic'' p f s a = 'Simple' ('Optic' p f) s a
-- @
type Optic' p f s a = Optic p f s s a a

-- | @
-- type 'LensLike' f s t a b = 'Optical' (->) (->) f s t a b
-- @
--
-- @
-- type 'Over' p f s t a b = 'Optical' p (->) f s t a b
-- @
--
-- @
-- type 'Optic' p f s t a b = 'Optical' p p f s t a b
-- @
type Optical p q f s t a b = p a (f b) -> q s (f t)

-- | @
-- type 'Optical'' p q f s a = 'Simple' ('Optical' p q f) s a
-- @
type Optical' p q f s a = Optical p q f s s a a


-- | Many combinators that accept a 'Lens' can also accept a
-- 'Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the
-- caller.
--
-- If a function accepts a @'LensLike' f s t a b@ for some 'Functor' @f@,
-- then they may be passed a 'Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'Traversal'.
type LensLike f s t a b = (a -> f b) -> s -> f t

-- | @
-- type 'LensLike'' f = 'Simple' ('LensLike' f)
-- @
type LensLike' f s a = LensLike f s s a a

-- | Convenient alias for constructing indexed lenses and their ilk.
type IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t

-- | Convenient alias for constructing simple indexed lenses and their ilk.
type IndexedLensLike' i f s a = IndexedLensLike i f s s a a

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
type Over p f s t a b = p a (f b) -> s -> f t

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
--
-- @
-- type 'Over'' p f = 'Simple' ('Over' p f)
-- @
type Over' p f s a = Over p f s s a a


--------------------------
-- Folds
--------------------------

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
--
-- >>> [1,2,3,4]^..folding tail
-- [2,3,4]
folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa agb = phantom . traverse_ agb . sfa
{-# INLINE folding #-}

ifolding :: (Foldable f, Indexable i p, Contravariant g, Applicative g) => (s -> f (i, a)) -> Over p g s t a b
ifolding sfa f = phantom . traverse_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding #-}

-- | Obtain a 'Fold' by lifting 'foldr' like function.
--
-- >>> [1,2,3,4]^..foldring foldr
-- [1,2,3,4]
foldring :: (Contravariant f, Applicative f) => ((a -> f a -> f a) -> f a -> s -> f a) -> LensLike f s t a b
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect
{-# INLINE foldring #-}

-- | Obtain 'FoldWithIndex' by lifting 'ifoldr' like function.
ifoldring :: (Indexable i p, Contravariant f, Applicative f) => ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
ifoldring ifr f = phantom . ifr (\i a fa -> indexed f i a *> fa) noEffect
{-# INLINE ifoldring #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
--
-- >>> Just 3^..folded
-- [3]
--
-- >>> Nothing^..folded
-- []
--
-- >>> [(1,2),(3,4)]^..folded.both
-- [1,2,3,4]
folded :: Foldable f => IndexedFold Int (f a) a
folded = conjoined (foldring foldr) (ifoldring ifoldr)
{-# INLINE folded #-}

ifoldr :: Foldable f => (Int -> a -> b -> b) -> b -> f a -> b
ifoldr f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
folded64 :: Foldable f => IndexedFold Int64 (f a) a
folded64 = conjoined (foldring foldr) (ifoldring ifoldr64)
{-# INLINE folded64 #-}

ifoldr64 :: Foldable f => (Int64 -> a -> b -> b) -> b -> f a -> b
ifoldr64 f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr64 #-}

-- | Form a 'Fold1' by repeating the input forever.
--
-- @
-- 'repeat' ≡ 'toListOf' 'repeated'
-- @
--
-- >>> timingOut $ 5^..taking 20 repeated
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
--
-- @
-- 'repeated' :: 'Fold1' a a
-- @
repeated :: Apply f => LensLike' f a a
repeated f a = as where as = f a .> as
{-# INLINE repeated #-}

-- | A 'Fold' that replicates its input @n@ times.
--
-- @
-- 'replicate' n ≡ 'toListOf' ('replicated' n)
-- @
--
-- >>> 5^..replicated 20
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)
{-# INLINE replicated #-}

-- | Transform a non-empty 'Fold' into a 'Fold1' that loops over its elements over and over.
--
-- >>> timingOut $ [1,2,3]^..taking 7 (cycled traverse)
-- [1,2,3,1,2,3,1]
--
-- @
-- 'cycled' :: 'Fold1' s a -> 'Fold1' s a
-- @
cycled :: Apply f => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a .> as
{-# INLINE cycled #-}

-- | Build a 'Fold' that unfolds its values from a seed.
--
-- @
-- 'Prelude.unfoldr' ≡ 'toListOf' '.' 'unfolded'
-- @
--
-- >>> 10^..unfolded (\b -> if b == 0 then Nothing else Just (b, b-1))
-- [10,9,8,7,6,5,4,3,2,1]
unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g = go where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> noEffect
{-# INLINE unfolded #-}

-- | @x '^.' 'iterated' f@ returns an infinite 'Fold1' of repeated applications of @f@ to @x@.
--
-- @
-- 'toListOf' ('iterated' f) a ≡ 'iterate' f a
-- @
--
-- @
-- 'iterated' :: (a -> a) -> 'Fold1' a a
-- @
iterated :: Apply f => (a -> a) -> LensLike' f a a
iterated f g = go where
  go a = g a .> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- Note: This is /not/ a legal 'Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- Note: This is also /not/ a legal 'Prism', unless you are very careful not to inject a value that fails the predicate.
--
-- As a counter example, consider that given @evens = 'filtered' 'even'@ the second 'Traversal' law is violated:
--
-- @
-- 'Control.Lens.Setter.over' evens 'succ' '.' 'Control.Lens.Setter.over' evens 'succ' '/=' 'Control.Lens.Setter.over' evens ('succ' '.' 'succ')
-- @
--
-- So, in order for this to qualify as a legal 'Traversal' you can only use it for actions that preserve the result of the predicate!
--
-- >>> [1..10]^..folded.filtered even
-- [2,4,6,8,10]
--
-- This will preserve an index if it is present.
filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'
{-# INLINE filtered #-}

-- | Obtain a potentially empty 'IndexedTraversal' by taking the first element from another,
-- potentially empty `Fold` and using it as an index.
--
-- The resulting optic can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- >>> [(Just 2, 3), (Nothing, 4)] & mapped . filteredBy (_1 . _Just) <. _2 %@~ (*) :: [(Maybe Int, Int)]
-- [(Just 2,6),(Nothing,4)]
--
-- @
-- 'filteredBy' :: 'Fold' a i -> 'IndexedTraversal'' i a a
-- @
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a
filteredBy p f val = case val ^? p of
  Nothing -> pure val
  Just witness -> indexed f witness val

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')
-- @
--
-- >>> timingOut $ toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
--
-- @
-- 'takingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- @
--
-- /Note:/ When applied to a 'Traversal', 'takingWhile' yields something that can be used as if it were a 'Traversal', but
-- which is not a 'Traversal' per the laws, unless you are careful to ensure that you do not invalidate the predicate when
-- writing back through it.
takingWhile :: (Conjoined p, Applicative f) => (a -> Bool) -> Over p (TakingWhile p f a a) s t a a -> Over p f s t a a
takingWhile p l pafb = fmap runMagma . traverse (cosieve pafb) . runTakingWhile . l flag where
  flag = cotabulate $ \wa -> let a = extract wa; r = p a in TakingWhile r a $ \pr ->
    if pr && r then Magma () wa else MagmaPure a
{-# INLINE takingWhile #-}

-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')
-- @
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
--
-- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
-- [6,1]
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a                -- see notes
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingTraversal'' s a    -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingLens'' s a         -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingGetter' s a        -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingFold' s a          -> 'IndexPreservingFold' s a
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- @
--
-- Note: Many uses of this combinator will yield something that meets the types, but not the laws of a valid
-- 'Traversal' or 'IndexedTraversal'. The 'Traversal' and 'IndexedTraversal' laws are only satisfied if the
-- new values you assign to the first target also does not pass the predicate! Otherwise subsequent traversals
-- will visit fewer elements and 'Traversal' fusion is not sound.
--
-- So for any traversal @t@ and predicate @p@, @`droppingWhile` p t@ may not be lawful, but
-- @(`Control.Lens.Traversal.dropping` 1 . `droppingWhile` p) t@ is. For example:
--
-- >>> let l  :: Traversal' [Int] Int; l  = droppingWhile (<= 1) traverse
-- >>> let l' :: Traversal' [Int] Int; l' = dropping 1 l
--
-- @l@ is not a lawful setter because @`Control.Lens.Setter.over` l f .
-- `Control.Lens.Setter.over` l g ≢ `Control.Lens.Setter.over` l (f . g)@:
--
-- >>> [1,2,3] & l .~ 0 & l .~ 4
-- [1,0,0]
-- >>> [1,2,3] & l .~ 4
-- [1,4,4]
--
-- @l'@ on the other hand behaves lawfully:
--
-- >>> [1,2,3] & l' .~ 0 & l' .~ 4
-- [1,2,4]
-- >>> [1,2,3] & l' .~ 4
-- [1,2,4]
droppingWhile :: (Conjoined p, Profunctor q, Applicative f)
              => (a -> Bool)
              -> Optical p q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = cotabulate $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else cosieve f wa, b')
{-# INLINE droppingWhile #-}

-- | A 'Fold' over the individual 'words' of a 'String'.
--
-- @
-- 'worded' :: 'Fold' 'String' 'String'
-- 'worded' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'worded' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'worded' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any whitespace characters while traversing, and if your original 'String' contains only
-- isolated space characters (and no other characters that count as space, such as non-breaking spaces).
worded :: Applicative f => IndexedLensLike' Int f String String
worded f = fmap unwords . conjoined traverse (indexing traverse) f . words
{-# INLINE worded #-}

-- | A 'Fold' over the individual 'lines' of a 'String'.
--
-- @
-- 'lined' :: 'Fold' 'String' 'String'
-- 'lined' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'lined' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'lined' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any newline characters while traversing, and if your original 'String' contains only
-- isolated newline characters.
lined :: Applicative f => IndexedLensLike' Int f String String
lined f = fmap (intercalate "\n") . conjoined traverse (indexing traverse) f . lines
{-# INLINE lined #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- | Map each part of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' to a monoid and combine the results.
--
-- >>> foldMapOf (folded . both . _Just) Sum [(Just 21, Just 21)]
-- Sum {getSum = 42}
--
-- @
-- 'Data.Foldable.foldMap' = 'foldMapOf' 'folded'
-- @
--
-- @
-- 'foldMapOf' ≡ 'views'
-- 'ifoldMapOf' l = 'foldMapOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldMapOf' ::                'Getter' s a      -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Fold' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Fold1' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Lens'' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Iso'' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Traversal'' s a  -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Traversal1'' s a -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Prism'' s a      -> (a -> r) -> s -> r
-- @
--
-- @
-- 'foldMapOf' :: 'Getting' r s a -> (a -> r) -> s -> r
-- @
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = coerce
{-# INLINE foldMapOf #-}

-- | Combine the elements of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' using a monoid.
--
-- >>> foldOf (folded.folded) [[Sum 1,Sum 4],[Sum 8, Sum 8],[Sum 21]]
-- Sum {getSum = 42}
--
-- @
-- 'Data.Foldable.fold' = 'foldOf' 'folded'
-- @
--
-- @
-- 'foldOf' ≡ 'view'
-- @
--
-- @
-- 'foldOf' ::             'Getter' s m     -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' s m       -> s -> m
-- 'foldOf' ::             'Lens'' s m      -> s -> m
-- 'foldOf' ::             'Iso'' s m       -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Traversal'' s m -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Prism'' s m     -> s -> m
-- @
foldOf :: Getting a s a -> s -> a
foldOf l = getConst #. l Const
{-# INLINE foldOf #-}

-- | Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'
-- @
--
-- @
-- 'foldrOf' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Prism'' s a     -> (a -> r -> r) -> r -> s -> r
-- @
--
-- @
-- 'ifoldrOf' l ≡ 'foldrOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldrOf' :: 'Getting' ('Endo' r) s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'
-- @
--
-- @
-- 'foldlOf' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Prism'' s a     -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf l f z = (flip appEndo z .# getDual) `rmap` foldMapOf l (Dual #. Endo #. flip f)
{-# INLINE foldlOf #-}

-- | Extract a list of the targets of a 'Fold'. See also ('^..').
--
-- @
-- 'Data.Foldable.toList' ≡ 'toListOf' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @

-- >>> toListOf both ("hello","world")
-- ["hello","world"]
--
-- @
-- 'toListOf' :: 'Getter' s a     -> s -> [a]
-- 'toListOf' :: 'Fold' s a       -> s -> [a]
-- 'toListOf' :: 'Lens'' s a      -> s -> [a]
-- 'toListOf' :: 'Iso'' s a       -> s -> [a]
-- 'toListOf' :: 'Traversal'' s a -> s -> [a]
-- 'toListOf' :: 'Prism'' s a     -> s -> [a]
-- @
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

-- | Extract a 'NonEmpty' of the targets of 'Fold1'.
--
-- >>> toNonEmptyOf both1 ("hello", "world")
-- "hello" :| ["world"]
--
-- @
-- 'toNonEmptyOf' :: 'Getter' s a      -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Fold1' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Lens'' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Iso'' s a        -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Traversal1'' s a -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Prism'' s a      -> s -> NonEmpty a
-- @
toNonEmptyOf :: Getting (NonEmptyDList a) s a -> s -> NonEmpty a
toNonEmptyOf l = flip getNonEmptyDList [] . foldMapOf l (NonEmptyDList #. (:|))

-- | A convenient infix (flipped) version of 'toListOf'.
--
-- >>> [[1,2],[3]]^..id
-- [[[1,2],[3]]]
-- >>> [[1,2],[3]]^..traverse
-- [[1,2],[3]]
-- >>> [[1,2],[3]]^..traverse.traverse
-- [1,2,3]
--
-- >>> (1,2)^..both
-- [1,2]
--
-- @
-- 'Data.Foldable.toList' xs ≡ xs '^..' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @
--
-- @
-- ('^..') :: s -> 'Getter' s a     -> [a]
-- ('^..') :: s -> 'Fold' s a       -> [a]
-- ('^..') :: s -> 'Lens'' s a      -> [a]
-- ('^..') :: s -> 'Iso'' s a       -> [a]
-- ('^..') :: s -> 'Traversal'' s a -> [a]
-- ('^..') :: s -> 'Prism'' s a     -> [a]
-- @
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

-- | Returns 'True' if every target of a 'Fold' is 'True'.
--
-- >>> andOf both (True,False)
-- False
-- >>> andOf both (True,True)
-- True
--
-- @
-- 'Data.Foldable.and' ≡ 'andOf' 'folded'
-- @
--
-- @
-- 'andOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'andOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'andOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'andOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
andOf :: Getting All s Bool -> s -> Bool
andOf l = getAll #. foldMapOf l All
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold' is 'True'.
--
-- >>> orOf both (True,False)
-- True
-- >>> orOf both (False,False)
-- False
--
-- @
-- 'Data.Foldable.or' ≡ 'orOf' 'folded'
-- @
--
-- @
-- 'orOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'orOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'orOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'orOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
orOf :: Getting Any s Bool -> s -> Bool
orOf l = getAny #. foldMapOf l Any
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf both (=='x') ('x','y')
-- True
-- >>> import Data.Data.Lens
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11::Int))
-- True
--
-- @
-- 'Data.Foldable.any' ≡ 'anyOf' 'folded'
-- @
--
-- @
-- 'ianyOf' l ≡ 'anyOf' l '.' 'Indexed'
-- @
--
-- @
-- 'anyOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf both (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Foldable.all' ≡ 'allOf' 'folded'
-- @
--
-- @
-- 'iallOf' l = 'allOf' l '.' 'Indexed'
-- @
--
-- @
-- 'allOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)
{-# INLINE allOf #-}

-- | Returns 'True' only if no targets of a 'Fold' satisfy a predicate.
--
-- >>> noneOf each (is _Nothing) (Just 3, Just 4, Just 5)
-- True
-- >>> noneOf (folded.folded) (<10) [[13,99,20],[3,71,42]]
-- False
--
-- @
-- 'inoneOf' l = 'noneOf' l '.' 'Indexed'
-- @
--
-- @
-- 'noneOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
noneOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
noneOf l f = not . anyOf l f
{-# INLINE noneOf #-}

-- | Calculate the 'Product' of every number targeted by a 'Fold'.
--
-- >>> productOf both (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @
-- 'Data.Foldable.product' ≡ 'productOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Product' '.' 'foldMapOf'@
--
-- @
-- 'productOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'productOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'productOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'productOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
productOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
productOf l = foldlOf' l (*) 1
{-# INLINE productOf #-}

-- | Calculate the 'Sum' of every number targeted by a 'Fold'.
--
-- >>> sumOf both (5,6)
-- 11
-- >>> sumOf folded [1,2,3,4]
-- 10
-- >>> sumOf (folded.both) [(1,2),(3,4)]
-- 10
-- >>> import Data.Data.Lens
-- >>> sumOf biplate [(1::Int,[]),(2,[(3::Int,4::Int)])] :: Int
-- 10
--
-- @
-- 'Data.Foldable.sum' ≡ 'sumOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Sum' '.' 'foldMapOf'@
--
-- @
-- 'sumOf' '_1' :: 'Num' a => (a, b) -> a
-- 'sumOf' ('folded' '.' 'Control.Lens.Tuple._1') :: ('Foldable' f, 'Num' a) => f (a, b) -> a
-- @
--
-- @
-- 'sumOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'sumOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'sumOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'sumOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
sumOf l = foldlOf' l (+) 0
{-# INLINE sumOf #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
-- but unlike 'Control.Lens.Traversal.traverseOf' do not construct a new structure. 'traverseOf_' generalizes
-- 'Data.Foldable.traverse_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'traverseOf_' can work over any 'Functor', but when passed a 'Fold', 'traverseOf_' requires
-- an 'Applicative'.
--
-- >>> traverseOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.traverse_' ≡ 'traverseOf_' 'folded'
-- @
--
-- @
-- 'traverseOf_' '_2' :: 'Functor' f => (c -> f r) -> (d, c) -> f ()
-- 'traverseOf_' 'Control.Lens.Prism._Left' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ()
-- @
--
-- @
-- 'itraverseOf_' l ≡ 'traverseOf_' l '.' 'Indexed'
-- @
--
-- The rather specific signature of 'traverseOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'traverseOf_' :: 'Functor' f     => 'Getter' s a     -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Fold' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Lens'' s a      -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Iso'' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Traversal'' s a -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Prism'' s a     -> (a -> f r) -> s -> f ()
-- @
traverseOf_ :: Functor f => Getting (Traversed r f) s a -> (a -> f r) -> s -> f ()
traverseOf_ l f = void . getTraversed #. foldMapOf l (Traversed #. f)
{-# INLINE traverseOf_ #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
-- but unlike 'Control.Lens.Traversal.forOf' do not construct a new structure. 'forOf_' generalizes
-- 'Data.Foldable.for_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'forOf_' can work over any 'Functor', but when passed a 'Fold', 'forOf_' requires
-- an 'Applicative'.
--
-- @
-- 'for_' ≡ 'forOf_' 'folded'
-- @
--
-- >>> forOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- The rather specific signature of 'forOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'iforOf_' l s ≡ 'forOf_' l s '.' 'Indexed'
-- @
--
-- @
-- 'forOf_' :: 'Functor' f     => 'Getter' s a     -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Lens'' s a      -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Iso'' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Traversal'' s a -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Prism'' s a     -> s -> (a -> f r) -> f ()
-- @
forOf_ :: Functor f => Getting (Traversed r f) s a -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @
-- 'sequenceA_' ≡ 'sequenceAOf_' 'folded'
-- @
--
-- >>> sequenceAOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' s (f a)     -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Lens'' s (f a)      -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Iso'' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Traversal'' s (f a) -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Prism'' s (f a)     -> s -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed a f) s (f a) -> s -> f ()
sequenceAOf_ l = void . getTraversed #. foldMapOf l Traversed
{-# INLINE sequenceAOf_ #-}

-- | Traverse over all of the targets of a 'Fold1', computing an 'Apply' based answer.
--
-- As long as you have 'Applicative' or 'Functor' effect you are better using 'traverseOf_'.
-- The 'traverse1Of_' is useful only when you have genuine 'Apply' effect.
--
-- >>> traverse1Of_ both1 (\ks -> Map.fromList [ (k, ()) | k <- ks ]) ("abc", "bcd")
-- fromList [('b',()),('c',())]
--
-- @
-- 'traverse1Of_' :: 'Apply' f => 'Fold1' s a -> (a -> f r) -> s -> f ()
-- @
--
-- @since 4.16
traverse1Of_ :: Functor f => Getting (TraversedF r f) s a -> (a -> f r) -> s -> f ()
traverse1Of_ l f = void . getTraversedF #. foldMapOf l (TraversedF #. f)
{-# INLINE traverse1Of_ #-}

-- | See 'forOf_' and 'traverse1Of_'.
--
-- >>> for1Of_ both1 ("abc", "bcd") (\ks -> Map.fromList [ (k, ()) | k <- ks ])
-- fromList [('b',()),('c',())]
--
-- @
-- 'for1Of_' :: 'Apply' f => 'Fold1' s a -> s -> (a -> f r) -> f ()
-- @
--
-- @since 4.16
for1Of_ :: Functor f => Getting (TraversedF r f) s a -> s -> (a -> f r) -> f ()
for1Of_ = flip . traverse1Of_
{-# INLINE for1Of_ #-}

-- | See 'sequenceAOf_' and 'traverse1Of_'.
--
-- @
-- 'sequence1Of_' :: 'Apply' f => 'Fold1' s (f a) -> s -> f ()
-- @
--
-- @since 4.16
sequence1Of_ :: Functor f => Getting (TraversedF a f) s (f a) -> s -> f ()
sequence1Of_ l = void . getTraversedF #. foldMapOf l TraversedF
{-# INLINE sequence1Of_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- >>> mapMOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'
-- @
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' s a     -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Lens'' s a      -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Iso'' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Traversal'' s a -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Prism'' s a     -> (a -> m r) -> s -> m ()
-- @
mapMOf_ :: Monad m => Getting (Sequenced r m) s a -> (a -> m r) -> s -> m ()
mapMOf_ l f = liftM skip . getSequenced #. foldMapOf l (Sequenced #. f)
{-# INLINE mapMOf_ #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- >>> forMOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- @
-- 'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'
-- @
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' s a     -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Lens'' s a      -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Iso'' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Traversal'' s a -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Prism'' s a     -> s -> (a -> m r) -> m ()
-- @
forMOf_ :: Monad m => Getting (Sequenced r m) s a -> s -> (a -> m r) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- >>> sequenceOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'
-- @
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' s (m a)     -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Lens'' s (m a)      -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Iso'' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Traversal'' s (m a) -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Prism'' s (m a)     -> s -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced a m) s (m a) -> s -> m ()
sequenceOf_ l = liftM skip . getSequenced #. foldMapOf l Sequenced
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> asumOf both ("hello","world")
-- "helloworld"
--
-- >>> asumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'asum' ≡ 'asumOf' 'folded'
-- @
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' s (f a)     -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Fold' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Lens'' s (f a)      -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Iso'' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Traversal'' s (f a) -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Prism'' s (f a)     -> s -> f a
-- @
asumOf :: Alternative f => Getting (Endo (f a)) s (f a) -> s -> f a
asumOf l = foldrOf l (<|>) empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> msumOf both ("hello","world")
-- "helloworld"
--
-- >>> msumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'msum' ≡ 'msumOf' 'folded'
-- @
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' s (m a)     -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Fold' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Lens'' s (m a)      -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Iso'' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Traversal'' s (m a) -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Prism'' s (m a)     -> s -> m a
-- @
msumOf :: MonadPlus m => Getting (Endo (m a)) s (m a) -> s -> m a
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold' of the structure?
--
-- >>> elemOf both "hello" ("hello","world")
-- True
--
-- @
-- 'elem' ≡ 'elemOf' 'folded'
-- @
--
-- @
-- 'elemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- >>> notElemOf each 'd' ('a','b','c')
-- True
--
-- >>> notElemOf each 'a' ('a','b','c')
-- False
--
-- @
-- 'notElem' ≡ 'notElemOf' 'folded'
-- @
--
-- @
-- 'notElemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
notElemOf :: Eq a => Getting All s a -> a -> s -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- >>> concatMapOf both (\x -> [x, x + 1]) (1,3)
-- [1,2,3,4]
--
-- @
-- 'concatMap' ≡ 'concatMapOf' 'folded'
-- @
--
-- @
-- 'concatMapOf' :: 'Getter' s a     -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Fold' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Lens'' s a      -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Iso'' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Traversal'' s a -> (a -> [r]) -> s -> [r]
-- @
concatMapOf :: Getting [r] s a -> (a -> [r]) -> s -> [r]
concatMapOf = coerce
{-# INLINE concatMapOf #-}

-- | Concatenate all of the lists targeted by a 'Fold' into a longer list.
--
-- >>> concatOf both ("pan","ama")
-- "panama"
--
-- @
-- 'concat' ≡ 'concatOf' 'folded'
-- 'concatOf' ≡ 'view'
-- @
--
-- @
-- 'concatOf' :: 'Getter' s [r]     -> s -> [r]
-- 'concatOf' :: 'Fold' s [r]       -> s -> [r]
-- 'concatOf' :: 'Iso'' s [r]       -> s -> [r]
-- 'concatOf' :: 'Lens'' s [r]      -> s -> [r]
-- 'concatOf' :: 'Traversal'' s [r] -> s -> [r]
-- @
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf l = getConst #. l Const
{-# INLINE concatOf #-}


-- | Calculate the number of targets there are for a 'Fold' in a given container.
--
-- /Note:/ This can be rather inefficient for large containers and just like 'length',
-- this will not terminate for infinite folds.
--
-- @
-- 'length' ≡ 'lengthOf' 'folded'
-- @
--
-- >>> lengthOf _1 ("hello",())
-- 1
--
-- >>> lengthOf traverse [1..10]
-- 10
--
-- >>> lengthOf (traverse.traverse) [[1,2],[3,4],[5,6]]
-- 6
--
-- @
-- 'lengthOf' ('folded' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a) -> 'Int'
-- @
--
-- @
-- 'lengthOf' :: 'Getter' s a     -> s -> 'Int'
-- 'lengthOf' :: 'Fold' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Lens'' s a      -> s -> 'Int'
-- 'lengthOf' :: 'Iso'' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Traversal'' s a -> s -> 'Int'
-- @
lengthOf :: Getting (Endo (Endo Int)) s a -> s -> Int
lengthOf l = foldlOf' l (\a _ -> a + 1) 0
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- When using a 'Traversal' as a partial 'Lens', or a 'Fold' as a partial 'Getter' this can be a convenient
-- way to extract the optional value.
--
-- Note: if you get stack overflows due to this, you may want to use 'firstOf' instead, which can deal
-- more gracefully with heavily left-biased trees. This is because '^?' works by using the
-- 'Data.Monoid.First' monoid, which can occasionally cause space leaks.
--
-- >>> Left 4 ^?_Left
-- Just 4
--
-- >>> Right 4 ^?_Left
-- Nothing
--
-- >>> "world" ^? ix 3
-- Just 'l'
--
-- >>> "world" ^? ix 20
-- Nothing
--
-- This operator works as an infix version of 'preview'.
--
-- @
-- ('^?') ≡ 'flip' 'preview'
-- @
--
-- It may be helpful to think of '^?' as having one of the following
-- more specialized types:
--
-- @
-- ('^?') :: s -> 'Getter' s a     -> 'Maybe' a
-- ('^?') :: s -> 'Fold' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Lens'' s a      -> 'Maybe' a
-- ('^?') :: s -> 'Iso'' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Traversal'' s a -> 'Maybe' a
-- @
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)
{-# INLINE (^?) #-}

-- | Perform an *UNSAFE* 'head' of a 'Fold' or 'Traversal' assuming that it is there.
--
-- >>> Left 4 ^?! _Left
-- 4
--
-- >>> "world" ^?! ix 3
-- 'l'
--
-- @
-- ('^?!') :: s -> 'Getter' s a     -> a
-- ('^?!') :: s -> 'Fold' s a       -> a
-- ('^?!') :: s -> 'Lens'' s a      -> a
-- ('^?!') :: s -> 'Iso'' s a       -> a
-- ('^?!') :: s -> 'Traversal'' s a -> a
-- @
(^?!) :: HasCallStack => s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

-- | Retrieve the 'First' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'preview'@ or @^?'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but does so
-- in a way that builds an intermediate structure, and thus may have worse
-- constant factors. This also means that it can not be used in any 'Control.Monad.Reader.MonadReader',
-- but must instead have 's' passed as its last argument, unlike 'preview'.
--
-- Note: this could been named `headOf`.
--
-- >>> firstOf traverse [1..10]
-- Just 1
--
-- >>> firstOf both (1,2)
-- Just 1
--
-- >>> firstOf ignored ()
-- Nothing
--
-- @
-- 'firstOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'firstOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'firstOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
firstOf :: Getting (Leftmost a) s a -> s -> Maybe a
firstOf l = getLeftmost . foldMapOf l LLeaf
{-# INLINE firstOf #-}

-- | Retrieve the 'Data.Semigroup.First' entry of a 'Fold1' or 'Traversal1' or the result from a 'Getter' or 'Lens'.
--
-- >>> first1Of traverse1 (1 :| [2..10])
-- 1
--
-- >>> first1Of both1 (1,2)
-- 1
--
-- /Note:/ this is different from '^.'.
--
-- >>> first1Of traverse1 ([1,2] :| [[3,4],[5,6]])
-- [1,2]
--
-- >>> ([1,2] :| [[3,4],[5,6]]) ^. traverse1
-- [1,2,3,4,5,6]
--
-- @
-- 'first1Of' :: 'Getter' s a      -> s -> a
-- 'first1Of' :: 'Fold1' s a       -> s -> a
-- 'first1Of' :: 'Lens'' s a       -> s -> a
-- 'first1Of' :: 'Iso'' s a        -> s -> a
-- 'first1Of' :: 'Traversal1'' s a -> s -> a
-- @
first1Of :: Getting (Semi.First a) s a -> s -> a
first1Of l = Semi.getFirst . foldMapOf l Semi.First

-- | Retrieve the 'Last' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'ala' 'Last' '.' 'foldMapOf'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but may have worse
-- constant factors.
--
-- >>> lastOf traverse [1..10]
-- Just 10
--
-- >>> lastOf both (1,2)
-- Just 2
--
-- >>> lastOf ignored ()
-- Nothing
--
-- @
-- 'lastOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'lastOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'lastOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
lastOf :: Getting (Rightmost a) s a -> s -> Maybe a
lastOf l = getRightmost . foldMapOf l RLeaf
{-# INLINE lastOf #-}

-- | Retrieve the 'Data.Semigroup.Last' entry of a 'Fold1' or 'Traversal1' or retrieve the result
-- from a 'Getter' or 'Lens'.o
--
-- >>> last1Of traverse1 (1 :| [2..10])
-- 10
--
-- >>> last1Of both1 (1,2)
-- 2
--
-- @
-- 'last1Of' :: 'Getter' s a      -> s -> 'Maybe' a
-- 'last1Of' :: 'Fold1' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Lens'' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Iso'' s a        -> s -> 'Maybe' a
-- 'last1Of' :: 'Traversal1'' s a -> s -> 'Maybe' a
-- @
last1Of :: Getting (Semi.Last a) s a -> s -> a
last1Of l = Semi.getLast . foldMapOf l Semi.Last

-- | Returns 'True' if this 'Fold' or 'Traversal' has no targets in the given container.
--
-- Note: 'nullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'False'.
--
-- @
-- 'null' ≡ 'nullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the 'null' check of many containers.
--
-- >>> nullOf _1 (1,2)
-- False
--
-- >>> nullOf ignored ()
-- True
--
-- >>> nullOf traverse []
-- True
--
-- >>> nullOf (element 20) [1..10]
-- True
--
-- @
-- 'nullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'nullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'nullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'nullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
nullOf :: Getting All s a -> s -> Bool
nullOf = hasn't
{-# INLINE nullOf #-}

-- | Returns 'True' if this 'Fold' or 'Traversal' has any targets in the given container.
--
-- A more \"conversational\" alias for this combinator is 'has'.
--
-- Note: 'notNullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'True'.
--
-- @
-- 'not' '.' 'null' ≡ 'notNullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the @'not' '.' 'null'@ check of many containers.
--
-- >>> notNullOf _1 (1,2)
-- True
--
-- >>> notNullOf traverse [1..10]
-- True
--
-- >>> notNullOf folded []
-- False
--
-- >>> notNullOf (element 20) [1..10]
-- False
--
-- @
-- 'notNullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'notNullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'notNullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'notNullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
notNullOf :: Getting Any s a -> s -> Bool
notNullOf = has
{-# INLINE notNullOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'maximumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> maximumOf traverse [1..10]
-- Just 10
--
-- >>> maximumOf traverse []
-- Nothing
--
-- >>> maximumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 6
--
-- @
-- 'maximum' ≡ 'fromMaybe' ('error' \"empty\") '.' 'maximumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMax' ('foldMapOf' l 'Max')@ has lazier semantics but could leak memory.
--
-- @
-- 'maximumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
maximumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
maximumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! max x y
{-# INLINE maximumOf #-}

-- | Obtain the maximum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> maximum1Of traverse1 (1 :| [2..10])
-- 10
--
-- @
-- 'maximum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
maximum1Of :: Ord a => Getting (Semi.Max a) s a -> s -> a
maximum1Of l = Semi.getMax . foldMapOf l Semi.Max
{-# INLINE maximum1Of #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'minimumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> minimumOf traverse [1..10]
-- Just 1
--
-- >>> minimumOf traverse []
-- Nothing
--
-- >>> minimumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 2
--
-- @
-- 'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMin' ('foldMapOf' l 'Min')@ has lazier semantics but could leak memory.
--
--
-- @
-- 'minimumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
minimumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
minimumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y
{-# INLINE minimumOf #-}

-- | Obtain the minimum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> minimum1Of traverse1 (1 :| [2..10])
-- 1
--
-- @
-- 'minimum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
minimum1Of :: Ord a => Getting (Semi.Min a) s a -> s -> a
minimum1Of l = Semi.getMin . foldMapOf l Semi.Min
{-# INLINE minimum1Of #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso',
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- >>> maximumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "mustard"
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- @
-- 'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp
-- @
--
-- @
-- 'maximumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
maximumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then x else y
{-# INLINE maximumByOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso'
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- >>> minimumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "ham"
--
-- @
-- 'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumByOf' 'folded' cmp
-- @
--
-- @
-- 'minimumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
minimumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then y else x
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>> findOf each even (1,3,4,6)
-- Just 4
--
-- >>> findOf folded even [1,3,5,7]
-- Nothing
--
-- @
-- 'findOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- @
--
-- @
-- 'Data.Foldable.find' ≡ 'findOf' 'folded'
-- 'ifindOf' l ≡ 'findOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findOf' :: 'Getting' ('Endo' ('Maybe' a)) s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' l p = 'foldrOf' l (\a y -> if p a then 'Just' a else y) 'Nothing'
-- @
findOf :: Getting (Endo (Maybe a)) s a -> (a -> Bool) -> s -> Maybe a
findOf l f = foldrOf l (\a y -> if f a then Just a else y) Nothing
{-# INLINE findOf #-}

-- | The 'findMOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a monadic predicate and a structure and returns in the monad the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,4,6)
-- "Checking 1"
-- "Checking 3"
-- "Checking 4"
-- Just 4
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,5,7)
-- "Checking 1"
-- "Checking 3"
-- "Checking 5"
-- "Checking 7"
-- Nothing
--
-- @
-- 'findMOf' :: ('Monad' m, 'Getter' s a)     -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Fold' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Iso'' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Lens'' s a)      -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Traversal'' s a) -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
--
-- @
-- 'findMOf' 'folded' :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m (Maybe a)
-- 'ifindMOf' l ≡ 'findMOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findMOf' :: Monad m => 'Getting' ('Endo' (m ('Maybe' a))) s a -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' l p = 'foldrOf' l (\a y -> p a >>= \x -> if x then return ('Just' a) else y) $ return 'Nothing'
-- @
findMOf :: Monad m => Getting (Endo (m (Maybe a))) s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf l f = foldrOf l (\a y -> f a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE findMOf #-}

-- | The 'lookupOf' function takes a 'Fold' (or 'Getter', 'Traversal',
-- 'Lens', 'Iso', etc.), a key, and a structure containing key/value pairs.
-- It returns the first value corresponding to the given key. This function
-- generalizes 'lookup' to work on an arbitrary 'Fold' instead of lists.
--
-- >>> lookupOf folded 4 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'b'
--
-- >>> lookupOf each 2 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'a'
--
-- @
-- 'lookupOf' :: 'Eq' k => 'Fold' s (k,v) -> k -> s -> 'Maybe' v
-- @
lookupOf :: Eq k => Getting (Endo (Maybe v)) s (k,v) -> k -> s -> Maybe v
lookupOf l k = foldrOf l (\(k',v) next -> if k == k' then Just v else next) Nothing
{-# INLINE lookupOf #-}

-- | A variant of 'foldrOf' that has no base case and thus may only be applied
-- to lenses and structures such that the 'Lens' views at least one element of
-- the structure.
--
-- >>> foldr1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- 'Data.Foldable.foldr1' ≡ 'foldr1Of' 'folded'
-- @
--
-- @
-- 'foldr1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of :: HasCallStack => Getting (Endo (Maybe a)) s a -> (a -> a -> a) -> s -> a
foldr1Of l f xs = fromMaybe (error "foldr1Of: empty structure")
                            (foldrOf l mf Nothing xs) where
  mf x my = Just $ case my of
    Nothing -> x
    Just y -> f x y
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and structures such
-- that the 'Lens' views at least one element of the structure.
--
-- >>> foldl1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldl1Of' l f ≡ 'Prelude.foldl1' f '.' 'toListOf' l
-- 'Data.Foldable.foldl1' ≡ 'foldl1Of' 'folded'
-- @
--
-- @
-- 'foldl1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of :: HasCallStack => Getting (Dual (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of l f xs = fromMaybe (error "foldl1Of: empty structure") (foldlOf l mf Nothing xs) where
  mf mx y = Just $ case mx of
    Nothing -> y
    Just x  -> f x y
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- @
-- 'Data.Foldable.foldr'' ≡ 'foldrOf'' 'folded'
-- @
--
-- @
-- 'foldrOf'' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf' :: Getting (Dual (Endo (Endo r))) s a -> (a -> r -> r) -> r -> s -> r
foldrOf' l f z0 xs = foldlOf l f' (Endo id) xs `appEndo` z0
  where f' (Endo k) x = Endo $ \ z -> k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- @
-- 'Data.Foldable.foldl'' ≡ 'foldlOf'' 'folded'
-- @
--
-- @
-- 'foldlOf'' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf' l f z0 xs = foldrOf l f' (Endo id) xs `appEndo` z0
  where f' x (Endo k) = Endo $ \z -> k $! f z x
{-# INLINE foldlOf' #-}

-- | A variant of 'foldrOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of the
-- structure.
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldr1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of' :: HasCallStack => Getting (Dual (Endo (Endo (Maybe a)))) s a -> (a -> a -> a) -> s -> a
foldr1Of' l f xs = fromMaybe (error "foldr1Of': empty structure") (foldrOf' l mf Nothing xs) where
  mf x Nothing = Just $! x
  mf x (Just y) = Just $! f x y
{-# INLINE foldr1Of' #-}

-- | A variant of 'foldlOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of
-- the structure.
--
-- @
-- 'foldl1Of'' l f ≡ 'Data.List.foldl1'' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldl1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of' :: HasCallStack => Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of' l f xs = fromMaybe (error "foldl1Of': empty structure") (foldlOf' l mf Nothing xs) where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! f x y
{-# INLINE foldl1Of' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- @
-- 'Data.Foldable.foldrM' ≡ 'foldrMOf' 'folded'
-- @
--
-- @
-- 'foldrMOf' :: 'Monad' m => 'Getter' s a     -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Fold' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Iso'' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Lens'' s a      -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Traversal'' s a -> (a -> r -> m r) -> r -> s -> m r
-- @
foldrMOf :: Monad m
         => Getting (Dual (Endo (r -> m r))) s a
         -> (a -> r -> m r) -> r -> s -> m r
foldrMOf l f z0 xs = foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- @
-- 'Data.Foldable.foldlM' ≡ 'foldlMOf' 'folded'
-- @
--
-- @
-- 'foldlMOf' :: 'Monad' m => 'Getter' s a     -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Fold' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Iso'' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Lens'' s a      -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Traversal'' s a -> (r -> a -> m r) -> r -> s -> m r
-- @
foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s a
         -> (r -> a -> m r) -> r -> s -> m r
foldlMOf l f z0 xs = foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- | Check to see if this 'Fold' or 'Traversal' matches 1 or more entries.
--
-- >>> has (element 0) []
-- False
--
-- >>> has _Left (Left 12)
-- True
--
-- >>> has _Right (Left 12)
-- False
--
-- This will always return 'True' for a 'Lens' or 'Getter'.
--
-- >>> has _1 ("hello","world")
-- True
--
-- @
-- 'has' :: 'Getter' s a     -> s -> 'Bool'
-- 'has' :: 'Fold' s a       -> s -> 'Bool'
-- 'has' :: 'Iso'' s a       -> s -> 'Bool'
-- 'has' :: 'Lens'' s a      -> s -> 'Bool'
-- 'has' :: 'Traversal'' s a -> s -> 'Bool'
-- @
has :: Getting Any s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)
{-# INLINE has #-}



-- | Check to see if this 'Fold' or 'Traversal' has no matches.
--
-- >>> hasn't _Left (Right 12)
-- True
--
-- >>> hasn't _Left (Left 12)
-- False
hasn't :: Getting All s a -> s -> Bool
hasn't l = getAll #. foldMapOf l (\_ -> All False)
{-# INLINE hasn't #-}

------------------------------------------------------------------------------
-- Pre
------------------------------------------------------------------------------

-- | This converts a 'Fold' to a 'IndexPreservingGetter' that returns the first element, if it
-- exists, as a 'Maybe'.
--
-- @
-- 'pre' :: 'Getter' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Fold' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Traversal'' s a -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Lens'' s a      -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Iso'' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Prism'' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- @
pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
pre l = dimap (getFirst . getConst #. l (Const #. First #. Just)) phantom
{-# INLINE pre #-}

-- | This converts an 'IndexedFold' to an 'IndexPreservingGetter' that returns the first index
-- and element, if they exist, as a 'Maybe'.
--
-- @
-- 'ipre' :: 'IndexedGetter' i s a     -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedFold' i s a       -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedTraversal'' i s a -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedLens'' i s a      -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- @
ipre :: IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a))
ipre l = dimap (getFirst . getConst #. l (Indexed $ \i a -> Const (First (Just (i, a))))) phantom
{-# INLINE ipre #-}

------------------------------------------------------------------------------
-- Preview
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also 'firstOf' and '^?', which are similar with
-- some subtle differences (explained below).
--
-- @
-- 'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'preview' 'folded'
-- @
--
-- @
-- 'preview' = 'view' '.' 'pre'
-- @
--
--
-- Unlike '^?', this function uses a
-- 'Control.Monad.Reader.MonadReader' to read the value to be focused in on.
-- This allows one to pass the value as the last argument by using the
-- 'Control.Monad.Reader.MonadReader' instance for @(->) s@
-- However, it may also be used as part of some deeply nested transformer stack.
--
-- 'preview' uses a monoidal value to obtain the result.
-- This means that it generally has good performance, but can occasionally cause space leaks
-- or even stack overflows on some data types.
-- There is another function, 'firstOf', which avoids these issues at the cost of
-- a slight constant performance cost and a little less flexibility.
--
-- It may be helpful to think of 'preview' as having one of the following
-- more specialized types:
--
-- @
-- 'preview' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'preview' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'preview' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
--
--
-- @
-- 'preview' :: 'MonadReader' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Traversal'' s a -> m ('Maybe' a)
--
-- @
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}

-- | Retrieve the first index and value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also ('^@?').
--
-- @
-- 'ipreview' = 'view' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreview' :: 'IndexedGetter' i s a     -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedFold' i s a       -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedLens'' i s a      -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedTraversal'' i s a -> s -> 'Maybe' (i, a)
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreview' :: 'MonadReader' s m => 'IndexedGetter' s a     -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedFold' s a       -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedLens'' s a      -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedTraversal'' s a -> m ('Maybe' (i, a))
-- @
ipreview :: MonadReader s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))
{-# INLINE ipreview #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens').
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.

-- @
-- 'previews' = 'views' '.' 'pre'
-- @
--
-- @
-- 'previews' :: 'Getter' s a     -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Fold' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Lens'' s a      -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Iso'' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Traversal'' s a -> (a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Monad' transformer stack:
--
-- @
-- 'previews' :: 'MonadReader' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
previews :: MonadReader s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))
{-# INLINE previews #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or 'Just' the result from an 'IndexedGetter' or 'IndexedLens').
-- See also ('^@?').
--
-- @
-- 'ipreviews' = 'views' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreviews' :: 'IndexedGetter' i s a     -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedFold' i s a       -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedLens'' i s a      -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedTraversal'' i s a -> (i -> a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreviews :: MonadReader s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = asks (getFirst . ifoldMapOf l (\i -> First #. Just . f i))
{-# INLINE ipreviews #-}

------------------------------------------------------------------------------
-- Preuse
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuse' = 'use' '.' 'pre'
-- @
--
-- @
-- 'preuse' :: 'MonadState' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = gets (preview l)
{-# INLINE preuse #-}

-- | Retrieve the first index and value targeted by an 'IndexedFold' or 'IndexedTraversal' (or 'Just' the index
-- and result from an 'IndexedGetter' or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuse' = 'use' '.' 'ipre'
-- @
--
-- @
-- 'ipreuse' :: 'MonadState' s m => 'IndexedGetter' i s a     -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedFold' i s a       -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedLens'' i s a      -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> m ('Maybe' (i, a))
-- @
ipreuse :: MonadState s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreuse l = gets (ipreview l)
{-# INLINE ipreuse #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuses' = 'uses' '.' 'pre'
-- @
--
-- @
-- 'preuses' :: 'MonadState' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
preuses :: MonadState s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
preuses l f = gets (previews l f)
{-# INLINE preuses #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or a function of 'Just' the index and result from an 'IndexedGetter'
-- or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuses' = 'uses' '.' 'ipre'
-- @
--
-- @
-- 'ipreuses' :: 'MonadState' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreuses :: MonadState s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreuses l f = gets (ipreviews l f)
{-# INLINE ipreuses #-}

------------------------------------------------------------------------------
-- Profunctors
------------------------------------------------------------------------------


-- | This allows you to 'Control.Traversable.traverse' the elements of a pretty much any 'LensLike' construction in the opposite order.
--
-- This will preserve indexes on 'Indexed' types and will give you the elements of a (finite) 'Fold' or 'Traversal' in the opposite order.
--
-- This has no practical impact on a 'Getter', 'Setter', 'Lens' or 'Iso'.
--
-- /NB:/ To write back through an 'Iso', you want to use 'Control.Lens.Isomorphic.from'.
-- Similarly, to write back through an 'Prism', you want to use 'Control.Lens.Review.re'.
backwards :: (Profunctor p, Profunctor q) => Optical p q (Backwards f) s t a b -> Optical p q f s t a b
backwards l f = forwards #. l (Backwards #. f)
{-# INLINE backwards #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Fold an 'IndexedFold' or 'IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the @i@.
--
-- When you don't need access to the index then 'foldMapOf' is more flexible in what it accepts.
--
-- @
-- 'foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i s a     -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i s a       -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' ::             'IndexedLens'' i s a      -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedTraversal'' i s a -> (i -> a -> m) -> s -> m
-- @
--
ifoldMapOf :: IndexedGetting i m s a -> (i -> a -> m) -> s -> m
ifoldMapOf = coerce
{-# INLINE ifoldMapOf #-}

-- | Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldrOf' is more flexible in what it accepts.
--
-- @
-- 'foldrOf' l ≡ 'ifoldrOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l f z = flip appEndo z . getConst #. l (Const #. Endo #. Indexed f)
{-# INLINE ifoldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldlOf' is more flexible in what it accepts.
--
-- @
-- 'foldlOf' l ≡ 'ifoldlOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a     -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedLens'' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedTraversal'' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z = (flip appEndo z .# getDual) `rmap` ifoldMapOf l (\i -> Dual #. Endo #. flip (f i))
{-# INLINE ifoldlOf #-}

-- | Return whether or not any element viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what it accepts.
--
-- @
-- 'anyOf' l ≡ 'ianyOf' l '.' 'const'
-- @
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf = coerce
{-# INLINE ianyOf #-}

-- | Return whether or not all elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what it accepts.
--
-- @
-- 'allOf' l ≡ 'iallOf' l '.' 'const'
-- @
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s a -> (i -> a -> Bool) -> s -> Bool
iallOf = coerce
{-# INLINE iallOf #-}

-- | Return whether or not none of the elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'noneOf' is more flexible in what it accepts.
--
-- @
-- 'noneOf' l ≡ 'inoneOf' l '.' 'const'
-- @
--
-- @
-- 'inoneOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
inoneOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
inoneOf l f = not . ianyOf l f
{-# INLINE inoneOf #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the @i@, discarding the results.
--
-- When you don't need access to the index then 'traverseOf_' is more flexible in what it accepts.
--
-- @
-- 'traverseOf_' l ≡ 'Control.Lens.Traversal.itraverseOf' l '.' 'const'
-- @
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l f = void . getTraversed #. getConst #. l (Const #. Traversed #. Indexed f)
{-# INLINE itraverseOf_ #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @
-- 'iforOf_' ≡ 'flip' '.' 'itraverseOf_'
-- @
--
-- When you don't need access to the index then 'forOf_' is more flexible in what it accepts.
--
-- @
-- 'forOf_' l a ≡ 'iforOf_' l a '.' 'const'
-- @
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> s -> (i -> a -> f r) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'mapMOf_' is more flexible in what it accepts.
--
-- @
-- 'mapMOf_' l ≡ 'Control.Lens.Setter.imapMOf' l '.' 'const'
-- @
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> (i -> a -> m r) -> s -> m ()
imapMOf_ l f = liftM skip . getSequenced #. getConst #. l (Const #. Sequenced #. Indexed f)
{-# INLINE imapMOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @
-- 'iforMOf_' ≡ 'flip' '.' 'imapMOf_'
-- @
--
-- When you don't need access to the index then 'forMOf_' is more flexible in what it accepts.
--
-- @
-- 'forMOf_' l a ≡ 'Control.Lens.Traversal.iforMOf' l a '.' 'const'
-- @
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> s -> (i -> a -> m r) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- | Concatenate the results of a function of the elements of an 'IndexedFold' or 'IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'concatMapOf'  is more flexible in what it accepts.
--
-- @
-- 'concatMapOf' l ≡ 'iconcatMapOf' l '.' 'const'
-- 'iconcatMapOf' ≡ 'ifoldMapOf'
-- @
--
-- @
-- 'iconcatMapOf' :: 'IndexedGetter' i s a     -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedFold' i s a       -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedLens'' i s a      -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedTraversal'' i s a -> (i -> a -> [r]) -> s -> [r]
-- @
iconcatMapOf :: IndexedGetting i [r] s a -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'ifindOf' function takes an 'IndexedFold' or 'IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findOf' is more flexible in what it accepts.
--
-- @
-- 'findOf' l ≡ 'ifindOf' l '.' 'const'
-- @
--
-- @
-- 'ifindOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- @
ifindOf :: IndexedGetting i (Endo (Maybe a)) s a -> (i -> a -> Bool) -> s -> Maybe a
ifindOf l f = ifoldrOf l (\i a y -> if f i a then Just a else y) Nothing
{-# INLINE ifindOf #-}

-- | The 'ifindMOf' function takes an 'IndexedFold' or 'IndexedTraversal', a monadic predicate that is also
-- supplied the index, a structure and returns in the monad the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findMOf' is more flexible in what it accepts.
--
-- @
-- 'findMOf' l ≡ 'ifindMOf' l '.' 'const'
-- @
--
-- @
-- 'ifindMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
ifindMOf :: Monad m => IndexedGetting i (Endo (m (Maybe a))) s a -> (i -> a -> m Bool) -> s -> m (Maybe a)
ifindMOf l f = ifoldrOf l (\i a y -> f i a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE ifindMOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrOf'' is more flexible in what it accepts.
--
-- @
-- 'foldrOf'' l ≡ 'ifoldrOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'foldlOf'' is more flexible in what it accepts.
--
-- @
-- 'foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedFold' i s a         -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedLens'' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedTraversal'' i s a   -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrMOf' is more flexible in what it accepts.
--
-- @
-- 'foldrMOf' l ≡ 'ifoldrMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> r -> m r) -> r -> s -> m r
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s a -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlMOf' is more flexible in what it accepts.
--
-- @
-- 'foldlMOf' l ≡ 'ifoldlMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> r -> a -> m r) -> r -> s -> m r
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s a -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toListOf' is more flexible in what it accepts.
--
-- @
-- 'toListOf' l ≡ 'map' 'snd' '.' 'itoListOf' l
-- @
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i s a     -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedFold' i s a       -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedLens'' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedTraversal'' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i (Endo [(i,a)]) s a -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []
{-# INLINE itoListOf #-}

-- | An infix version of 'itoListOf'.

-- @
-- ('^@..') :: s -> 'IndexedGetter' i s a     -> [(i,a)]
-- ('^@..') :: s -> 'IndexedFold' i s a       -> [(i,a)]
-- ('^@..') :: s -> 'IndexedLens'' i s a      -> [(i,a)]
-- ('^@..') :: s -> 'IndexedTraversal'' i s a -> [(i,a)]
-- @
(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s a -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s
{-# INLINE (^@..) #-}

-- | Perform a safe 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' or retrieve 'Just' the index and result
-- from an 'IndexedGetter' or 'IndexedLens'.
--
-- When using a 'IndexedTraversal' as a partial 'IndexedLens', or an 'IndexedFold' as a partial 'IndexedGetter' this can be a convenient
-- way to extract the optional value.
--
-- @
-- ('^@?') :: s -> 'IndexedGetter' i s a     -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedFold' i s a       -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedLens'' i s a      -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedTraversal'' i s a -> 'Maybe' (i, a)
-- @
(^@?) :: s -> IndexedGetting i (Endo (Maybe (i, a))) s a -> Maybe (i, a)
s ^@? l = ifoldrOf l (\i x _ -> Just (i,x)) Nothing s
{-# INLINE (^@?) #-}

-- | Perform an *UNSAFE* 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' assuming that it is there.
--
-- @
-- ('^@?!') :: s -> 'IndexedGetter' i s a     -> (i, a)
-- ('^@?!') :: s -> 'IndexedFold' i s a       -> (i, a)
-- ('^@?!') :: s -> 'IndexedLens'' i s a      -> (i, a)
-- ('^@?!') :: s -> 'IndexedTraversal'' i s a -> (i, a)
-- @
(^@?!) :: HasCallStack => s -> IndexedGetting i (Endo (i, a)) s a -> (i, a)
s ^@?! l = ifoldrOf l (\i x _ -> (i,x)) (error "(^@?!): empty Fold") s
{-# INLINE (^@?!) #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which is equal to a given value.
--
-- @
-- 'Data.List.elemIndex' ≡ 'elemIndexOf' 'folded'
-- @
--
-- @
-- 'elemIndexOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> 'Maybe' i
-- 'elemIndexOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> 'Maybe' i
-- @
elemIndexOf :: Eq a => IndexedGetting i (First i) s a -> a -> s -> Maybe i
elemIndexOf l a = findIndexOf l (a ==)
{-# INLINE elemIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which are equal to a given value.
--
-- @
-- 'Data.List.elemIndices' ≡ 'elemIndicesOf' 'folded'
-- @
--
-- @
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> [i]
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> [i]
-- @
elemIndicesOf :: Eq a => IndexedGetting i (Endo [i]) s a -> a -> s -> [i]
elemIndicesOf l a = findIndicesOf l (a ==)
{-# INLINE elemIndicesOf #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfies a predicate.
--
-- @
-- 'Data.List.findIndex' ≡ 'findIndexOf' 'folded'
-- @
--
-- @
-- 'findIndexOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> 'Maybe' i
-- 'findIndexOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> 'Maybe' i
-- @
findIndexOf :: IndexedGetting i (First i) s a -> (a -> Bool) -> s -> Maybe i
findIndexOf l p = preview (l . filtered p . asIndex)
{-# INLINE findIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfy a predicate.
--
-- @
-- 'Data.List.findIndices' ≡ 'findIndicesOf' 'folded'
-- @
--
-- @
-- 'findIndicesOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> [i]
-- 'findIndicesOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> [i]
-- @
findIndicesOf :: IndexedGetting i (Endo [i]) s a -> (a -> Bool) -> s -> [i]
findIndicesOf l p = toListOf (l . filtered p . asIndex)
{-# INLINE findIndicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Filter an 'IndexedFold' or 'IndexedGetter', obtaining an 'IndexedFold'.
--
-- >>> [0,0,0,5,5,5]^..traversed.ifiltered (\i a -> i <= a)
-- [0,5,5,5]
--
-- Compose with 'ifiltered' to filter another 'IndexedLens', 'IndexedIso', 'IndexedGetter', 'IndexedFold' (or 'IndexedTraversal') with
-- access to both the value and the index.
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
ifiltered :: (Indexable i p, Applicative f) => (i -> a -> Bool) -> Optical' p (Indexed i) f a a
ifiltered p f = Indexed $ \i a -> if p i a then indexed f i a else pure a
{-# INLINE ifiltered #-}

-- | Obtain an 'IndexedFold' by taking elements from another
-- 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
--
-- Note: Applying 'itakingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still allow you to use it as a
-- pseudo-'IndexedTraversal', but if you change the value of any target to one where the predicate returns
-- 'False', then you will break the 'Traversal' laws and 'Traversal' fusion will no longer be sound.
itakingWhile :: (Indexable i p, Profunctor q, Contravariant f, Applicative f)
         => (i -> a -> Bool)
         -> Optical' (Indexed i) q (Const (Endo (f s))) s a
         -> Optical' p q f s a
itakingWhile p l f = (flip appEndo noEffect .# getConst) `rmap` l g where
  g = Indexed $ \i a -> Const . Endo $ if p i a then (indexed f i a *>) else const noEffect
{-# INLINE itakingWhile #-}

-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
--
-- Note: As with `droppingWhile` applying 'idroppingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still
-- allow you to use it as a pseudo-'IndexedTraversal', but if you change the value of the first target to one
-- where the predicate returns 'True', then you will break the 'Traversal' laws and 'Traversal' fusion will
-- no longer be sound.
idroppingWhile :: (Indexable i p, Profunctor q, Applicative f)
              => (i -> a -> Bool)
              -> Optical (Indexed i) q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
idroppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = Indexed $ \ i a -> Compose $ state $ \b -> let
      b' = b && p i a
    in (if b' then pure a else indexed f i a, b')
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

noEffect = undefined

collect = undefined

apDefault = undefined

swap = undefined
