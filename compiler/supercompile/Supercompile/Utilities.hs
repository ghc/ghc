module Supercompile.Utilities (
    module Supercompile.Utilities,

    module UniqSupply,
    module Outputable,
    
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    
    module Data.Maybe,
    module Data.List
  ) where

import UniqSupply
import Outputable

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative (Applicative(..))
import Control.Monad hiding (join)

import Data.Function (on)
import Data.Maybe
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


-- | Copointed functors. The defining property is:
--
--   extract (fmap f a) == f (extract a)
class Functor f => Copointed f where
    extract :: f a -> a

instance Copointed ((,) a) where
    extract = snd


newtype Wrapper1 f a = Wrapper1 { unWrapper1 :: f a }


class Show1 f where
    showsPrec1 :: Show a => Int -> f a -> ShowS

instance (Show1 f, Show a) => Show (Wrapper1 f a) where
    showsPrec prec = showsPrec1 prec . unWrapper1


class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

instance (Eq1 f, Eq a) => Eq (Wrapper1 f a) where
    (==) = eq1 `on` unWrapper1


class Eq1 f => Ord1 f where
    compare1 :: Ord a => f a -> f a -> Ordering

instance (Ord1 f, Ord a) => Ord (Wrapper1 f a) where
    compare = compare1 `on` unWrapper1


class Outputable1 f where
    pprPrec1 :: Outputable a => Rational -> f a -> SDoc

instance (Outputable1 f, Outputable a) => Outputable (Wrapper1 f a) where
    pprPrec prec = pprPrec1 prec . unWrapper1

-- | Parenthesize an value if the boolean is true.
prettyParen :: Bool -> SDoc -> SDoc
prettyParen False = id
prettyParen True = parens

appPrec, opPrec, noPrec :: Num a => a
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others


bananas :: SDoc -> SDoc
bananas d = text "(|" <> d <> text "|)"


newtype PrettyFunction = PrettyFunction (Rational -> SDoc)

instance Outputable PrettyFunction where
    pprPrec prec (PrettyFunction f) = f prec

asPrettyFunction :: Outputable a => a -> PrettyFunction
asPrettyFunction x = PrettyFunction (\prec -> pprPrec prec x)


newtype Identity a = I { unI :: a }

instance Copointed Identity where
    extract = unI

instance Monad Identity where
    return = I
    mx >>= fxmy = fxmy (unI mx)

instance Functor Identity where
    fmap f (I x) = I (f x)

instance Foldable.Foldable Identity where
    foldMap f (I x) = f x

instance Traversable.Traversable Identity where
    traverse f (I x) = pure I <*> f x

instance Show1 Identity where
    showsPrec1 prec (I x) = showParen (prec >= appPrec) (showString "Identity" . showsPrec appPrec x)

instance Eq1 Identity where
    eq1 (I x1) (I x2) = x1 == x2

instance Ord1 Identity where
    compare1 (I x1) (I x2) = x1 `compare` x2

instance Outputable1 Identity where
    pprPrec1 prec (I x) = pprPrec prec x


newtype (O f g) a = Comp { unComp :: f (g a) }

infixr 9 `O`

instance (Copointed f, Copointed g) => Copointed (O f g) where
    extract = extract . extract . unComp

instance (Functor f, Show1 f, Show1 g) => Show1 (O f g) where
    showsPrec1 prec (Comp x) = showParen (prec >= appPrec) (showString "Comp" . showsPrec1 appPrec (fmap Wrapper1 x))

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (O f g) where
    eq1 (Comp x1) (Comp x2) = fmap Wrapper1 x1 `eq1` fmap Wrapper1 x2

instance (Functor f, Ord1 f, Ord1 g) => Ord1 (O f g) where
    compare1 (Comp x1) (Comp x2) = fmap Wrapper1 x1 `compare1` fmap Wrapper1 x2

instance (Functor f, Outputable1 f, Outputable1 g) => Outputable1 (O f g) where
    pprPrec1 prec (Comp x) = pprPrec1 prec (fmap Wrapper1 x)

instance (Functor f, Functor g) => Functor (O f g) where
    fmap f (Comp x) = Comp (fmap (fmap f) x)

instance (Foldable.Foldable f, Foldable.Foldable g) => Foldable.Foldable (O f g) where
    foldMap f = Foldable.foldMap (Foldable.foldMap f) . unComp

instance (Traversable.Traversable f, Traversable.Traversable g) => Traversable.Traversable (O f g) where
    traverse f = fmap Comp . Traversable.traverse (Traversable.traverse f) . unComp


-- | Natural numbers on the cheap (for efficiency reasons)
type Nat = Int


newtype Fin = Fin { unFin :: Int } deriving (Eq, Ord)

instance Show Fin where
    show (Fin x) = show x

instance Outputable Fin where
    pprPrec prec (Fin x) = pprPrec prec x


type FinSet = IS.IntSet
type FinMap = IM.IntMap


data Tag = TG { tagFin :: Fin, tagOccurrences :: Nat } deriving (Eq, Ord, Show)

instance Outputable Tag where
    ppr (TG i occs) = ppr i <> brackets (ppr occs)

mkTag :: Int -> Tag
mkTag i = TG (Fin i) 1

injectTag :: Int -> Tag -> Tag
injectTag cls (TG (Fin i) occs) = TG (Fin (cls * i)) occs

tagInt :: Tag -> Int
tagInt = unFin . tagFin


data Tagged a = Tagged { tag :: !Tag, tagee :: !a }

instance Copointed Tagged where
    extract = tagee

instance Functor Tagged where
    fmap f (Tagged tg x) = Tagged tg (f x)

instance Foldable.Foldable Tagged where
    foldMap f (Tagged _ x) = f x

instance Traversable.Traversable Tagged where
    traverse f (Tagged tg x) = pure (Tagged tg) <*> f x

instance Show1 Tagged where
    showsPrec1 prec (Tagged tg x) = showParen (prec >= appPrec) (showString "Tagged" . showsPrec appPrec tg . showsPrec appPrec x)

instance Eq1 Tagged where
    eq1 (Tagged tg1 x1) (Tagged tg2 x2) = tg1 == tg2 && x1 == x2

instance Ord1 Tagged where
    compare1 (Tagged tg1 x1) (Tagged tg2 x2) = (tg1, x1) `compare` (tg2, x2)

instance Outputable1 Tagged where
    pprPrec1 prec (Tagged tg x) = braces (ppr tg) <+> pprPrec prec x


type Size = Int

data Sized a = Sized { size :: !Size, sizee :: !a }

instance Copointed Sized where
    extract = sizee

instance Functor Sized where
    fmap f (Sized sz x) = Sized sz (f x)

instance Foldable.Foldable Sized where
    foldMap f (Sized _ x) = f x

instance Traversable.Traversable Sized where
    traverse f (Sized sz x) = pure (Sized sz) <*> f x

instance Show1 Sized where
    showsPrec1 prec (Sized sz x) = showParen (prec >= appPrec) (showString "Sized" . showsPrec appPrec sz . showsPrec appPrec x)

instance Eq1 Sized where
    eq1 (Sized sz1 x1) (Sized sz2 x2) = sz1 == sz2 && x1 == x2

instance Ord1 Sized where
    compare1 (Sized sz1 x1) (Sized sz2 x2) = (sz1, x1) `compare` (sz2, x2)

instance Outputable1 Sized where
    pprPrec1 prec (Sized sz x) = bananas (text (show sz)) <> pprPrec prec x


pPrint :: Outputable a => a -> SDoc
pPrint = ppr

pPrintPrec :: Outputable a => Rational -> a -> SDoc
pPrintPrec = pprPrec


orElse :: Maybe a -> a -> a
orElse = flip fromMaybe


third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
third3 f (a, b, c) = (a, b, f c)
