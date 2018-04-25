{-# LANGUAGE RankNTypes #-}
-- | This module provides very basic lens functionality, without extra dependencies.
--
-- For the documentation of the combinators see <http://hackage.haskell.org/package/lens lens> package.
-- This module uses the same vocabulary.
module Distribution.Compat.Lens (
    -- * Types
    Lens,
    Lens',
    Traversal,
    Traversal',
    -- ** LensLike
    LensLike,
    LensLike',
    -- ** rank-1 types
    Getting,
    AGetter,
    ASetter,
    ALens,
    ALens',
    -- * Getter
    view,
    use,
    -- * Setter
    set,
    over,
    -- * Fold
    toDListOf,
    toListOf,
    toSetOf,
    -- * Lens
    cloneLens,
    aview,
    -- * Common lenses
    _1, _2,
    non,
    fromNon,
    -- * Operators
    (&),
    (^.),
    (.~), (?~), (%~),
    (.=), (?=), (%=),
    (^#),
    (#~), (#%~),
    -- * Internal Comonads
    Pretext (..),
    -- * Cabal developer info
    -- $development
    ) where

import Prelude()
import Distribution.Compat.Prelude

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Control.Monad.State.Class (MonadState (..), gets, modify)

import qualified Distribution.Compat.DList as DList
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type LensLike  f s t a b = (a -> f b) -> s -> f t
type LensLike' f s   a   = (a -> f a) -> s -> f s

type Lens      s t a b = forall f. Functor f     => LensLike f s t a b
type Traversal s t a b = forall f. Applicative f => LensLike f s t a b

type Lens'      s a = Lens s s a a
type Traversal' s a = Traversal s s a a

type Getting r s a = LensLike (Const r) s s a a

type AGetter s   a   = LensLike (Const a)     s s a a  -- this doens't exist in 'lens'
type ASetter s t a b = LensLike Identity      s t a b
type ALens   s t a b = LensLike (Pretext a b) s t a b

type ALens' s a = ALens s s a a

-------------------------------------------------------------------------------
-- Getter
-------------------------------------------------------------------------------

view :: Getting a s a -> s ->  a
view l s = getConst (l Const s)
{-# INLINE view #-}

use :: MonadState s m => Getting a s a -> m a
use l = gets (view l)
{-# INLINE use #-}

-------------------------------------------------------------------------------
-- Setter
-------------------------------------------------------------------------------

set :: ASetter s t a  b -> b -> s -> t
set l x = over l (const x)

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (\x -> Identity (f x)) s)

-------------------------------------------------------------------------------
-- Fold
-------------------------------------------------------------------------------

toDListOf :: Getting (DList.DList a) s a -> s -> DList.DList a
toDListOf l s = getConst (l (\x -> Const (DList.singleton x)) s)

toListOf :: Getting (DList.DList a) s a -> s -> [a]
toListOf l = DList.runDList . toDListOf l

toSetOf  :: Getting (Set.Set a) s a -> s -> Set.Set a
toSetOf l s = getConst (l (\x -> Const (Set.singleton x)) s)

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

aview :: ALens s t a b -> s -> a
aview l = pretextPos  . l pretextSell
{-# INLINE aview #-}
{-
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens sa sbt afb s = sbt s <$> afb (sa s)
-}

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

_1 ::  Lens (a, c) (b, c) a b
_1 f (a, c) = flip (,) c <$> f a

_2 ::  Lens (c, a) (c, b) a b
_2 f (c, a) = (,) c <$> f a

-- | /Note:/ not an isomorphism here.
non :: Eq a => a -> Lens' (Maybe a) a
non def f s = wrap <$> f (unwrap s)
  where
    wrap x | x == def = Nothing
    wrap x            = Just x

    unwrap = fromMaybe def


fromNon :: Eq a =>  a -> Lens' a (Maybe a)
fromNon def f s = unwrap <$> f (wrap s)
  where
    wrap x | x == def = Nothing
    wrap x            = Just x

    unwrap = fromMaybe def

-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

-- | '&' is a reverse application operator
(&) :: a -> (a -> b) -> b
(&) = flip ($)
{-# INLINE (&) #-}
infixl 1 &

infixl 8 ^., ^#
infixr 4 .~, %~, ?~
infixr 4 #~, #%~
infixr 4 .=, %=, ?=

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)
{-# INLINE (?~) #-}

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

(?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = modify (l ?~ b)
{-# INLINE (?=) #-}

(%=) :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}

(^#) :: s -> ALens s t a b -> a
s ^# l = aview l s

(#~) :: ALens s t a b -> b -> s -> t
(#~) l b s = pretextPeek b (l pretextSell s)
{-# INLINE (#~) #-}

(#%~) :: ALens s t a b -> (a -> b) -> s -> t
(#%~) l f s = pretextPeeks f (l pretextSell s)
{-# INLINE (#%~) #-}

pretextSell :: a -> Pretext a b b
pretextSell a = Pretext (\afb -> afb a)
{-# INLINE pretextSell #-}

pretextPeeks :: (a -> b) -> Pretext a b t -> t
pretextPeeks f (Pretext m) = runIdentity $ m (\x -> Identity (f x))
{-# INLINE pretextPeeks #-}

pretextPeek :: b -> Pretext a b t -> t
pretextPeek b (Pretext m) = runIdentity $ m (\_ -> Identity b)
{-# INLINE pretextPeek #-}

pretextPos :: Pretext a b t -> a
pretextPos (Pretext m) = getConst (m Const)
{-# INLINE pretextPos #-}

cloneLens :: Functor f => ALens s t a b -> LensLike f s t a b
cloneLens l f s = runPretext (l pretextSell s) f
{-# INLINE cloneLens #-}

-------------------------------------------------------------------------------
-- Comonads
-------------------------------------------------------------------------------

-- | @lens@ variant is also parametrised by profunctor.
data Pretext a b t = Pretext { runPretext :: forall f. Functor f => (a -> f b) -> f t }

instance Functor (Pretext a b) where
    fmap f (Pretext pretext) = Pretext (\afb -> fmap f (pretext afb))

-------------------------------------------------------------------------------
-- Documentation
-------------------------------------------------------------------------------

-- $development
--
-- We cannot depend on @template-haskell@, because Cabal is a boot library.
-- This fact makes defining optics a manual task. Here is a small recipe to
-- make the process less tedious.
--
-- First start a repl
--
-- > cabal new-repl Cabal:parser-hackage-tests -fparsec-struct-diff
--
-- Because @--extra-package@ isn't yet implemented, we use a test-suite
-- with @generics-sop@ dependency.
--
-- In the repl, we load a helper script:
--
-- > :l ../generics-sop-lens.hs
--
-- Now we are set up to derive lenses!
--
-- > :m +Distribution.Types.SourceRepo
-- > putStr $ genericLenses (Proxy :: Proxy SourceRepo)
--
-- @
-- repoKind :: Lens' SourceRepo RepoKind
-- repoKind f s = fmap (\\x -> s { T.repoKind = x }) (f (T.repoKind s))
-- \{-# INLINE repoKind #-\}
-- ...
-- @
--
-- /Note:/ You may need to adjust type-aliases, e.g. `String` to `FilePath`.
