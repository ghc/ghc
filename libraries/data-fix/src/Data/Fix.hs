{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Trustworthy               #-}

-- needed for Data instance
{-# LANGUAGE UndecidableInstances      #-}

#define HAS_POLY_TYPEABLE MIN_VERSION_base(4,7,0)

#if HAS_POLY_TYPEABLE
{-# LANGUAGE StandaloneDeriving        #-}
#endif

-- | Fixed points of a functor.
--
-- Type @f@ should be a 'Functor' if you want to use
-- simple recursion schemes or 'Traversable' if you want to
-- use monadic recursion schemes. This style allows you to express
-- recursive functions in non-recursive manner.
-- You can imagine that a non-recursive function
-- holds values of the previous iteration.
--
-- An example:
--
-- First we define a base functor. The arguments @b@ are recursion points.
--
-- >>> data ListF a b = Nil | Cons a b deriving (Show, Functor)
--
-- The list is then a fixed point of 'ListF'
--
-- >>> type List a = Fix (ListF a)
--
-- We can write @length@ function. Note that the function we give
-- to 'foldFix' is not recursive. Instead the results
-- of recursive calls are in @b@ positions, and we need to deal
-- only with one layer of the structure.
--
-- >>> :{
-- let length :: List a -> Int
--     length = foldFix $ \x -> case x of
--         Nil      -> 0
--         Cons _ n -> n + 1
-- :}
--
-- If you already have recursive type, like '[Int]',
-- you can first convert it to `Fix (ListF a)` and then `foldFix`.
-- Alternatively you can use @recursion-schemes@ combinators
-- which work directly on recursive types.
--
module Data.Fix (
    -- * Fix
    Fix (..),
    hoistFix,
    hoistFix',
    foldFix,
    unfoldFix,
    -- * Mu - least fixed point
    Mu (..),
    hoistMu,
    foldMu,
    unfoldMu,
    -- * Nu - greatest fixed point
    Nu (..),
    hoistNu,
    foldNu,
    unfoldNu,
    -- * Refolding
    refold,
    -- * Monadic variants
    foldFixM,
    unfoldFixM,
    refoldM,
    -- * Deprecated aliases
    cata, ana, hylo,
    cataM, anaM, hyloM,
) where

-- Explicit imports help dodge unused imports warnings,
-- as we say what we want from Prelude
import Data.Traversable (Traversable (..))
import Prelude (Eq (..), Functor (..), Monad (..), Ord (..), Read (..), Show (..), showParen, showString, ($), (.), (=<<))

#ifdef __GLASGOW_HASKELL__
#if! HAS_POLY_TYPEABLE
import Prelude (const, error, undefined)
#endif
#endif

import Control.Monad        (liftM)
import Data.Function        (on)
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1, compare1, eq1, readsPrec1, showsPrec1)
import Data.Hashable        (Hashable (..))
import Data.Hashable.Lifted (Hashable1, hashWithSalt1)
import Data.Typeable        (Typeable)
import GHC.Generics         (Generic)
import Text.Read            (Lexeme (Ident), Read (..), lexP, parens, prec, readS_to_Prec, step)

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData (..), NFData1, rnf1)
#endif

#if HAS_POLY_TYPEABLE
import Data.Data (Data)
#else
import Data.Data
#endif

-- $setup
-- >>> :set -XDeriveFunctor
-- >>> import Prelude (showChar, Int, Num (..))
-- >>> import Data.Functor.Classes
-- >>> data ListF a b = Nil | Cons a b deriving (Show, Functor)
--
-- >>> :{
-- >>> instance Show a => Show1 (ListF a) where
-- >>>     liftShowsPrec _  _ d Nil        = showString "Nil"
-- >>>     liftShowsPrec sp _ d (Cons a b) = showParen (d > 10) $ showString "Cons " . showsPrec 11 a . showChar ' ' . sp 11 b
-- >>> :}
--
-- >>> :{
-- >>> let elimListF n c Nil        = 0
-- >>>     elimListF n c (Cons a b) = c a b
-- >>> :}

-------------------------------------------------------------------------------
-- Fix
-------------------------------------------------------------------------------

-- | A fix-point type.
newtype Fix f = Fix { unFix :: f (Fix f) }
  deriving (Generic)

-- | Change base functor in 'Fix'.
hoistFix :: Functor f => (forall a. f a -> g a) -> Fix f -> Fix g
hoistFix nt = go where go (Fix f) = Fix (nt (fmap go f))

-- | Like 'hoistFix' but 'fmap'ping over @g@.
hoistFix' :: Functor g => (forall a. f a -> g a) -> Fix f -> Fix g
hoistFix' nt = go where go (Fix f) = Fix (fmap go (nt f))

-- | Fold 'Fix'.
--
-- >>> let fp = unfoldFix (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldFix (elimListF 0 (+)) fp
-- 6
--
foldFix :: Functor f => (f a -> a) -> Fix f -> a
foldFix f = go where go = f . fmap go . unFix

-- | Unfold 'Fix'.
--
-- >>> unfoldFix (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil))))))))
--
unfoldFix :: Functor f => (a -> f a) -> a -> Fix f
unfoldFix f = go where go = Fix . fmap go . f

-------------------------------------------------------------------------------
-- Functor instances
-------------------------------------------------------------------------------

instance Eq1 f => Eq (Fix f) where
    Fix a == Fix b = eq1 a b

instance Ord1 f => Ord (Fix f) where
    compare (Fix a) (Fix b) = compare1 a b

instance Show1 f => Show (Fix f) where
    showsPrec d (Fix a) =
        showParen (d >= 11)
            $ showString "Fix "
            . showsPrec1 11 a

#ifdef __GLASGOW_HASKELL__
instance Read1 f => Read (Fix f) where
    readPrec = parens $ prec 10 $ do
        Ident "Fix" <- lexP
        fmap Fix (step (readS_to_Prec readsPrec1))
#endif

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance Hashable1 f => Hashable (Fix f) where
    hashWithSalt salt = hashWithSalt1 salt . unFix

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 f => NFData (Fix f) where
    rnf = rnf1 . unFix
#endif

-------------------------------------------------------------------------------
-- Typeable and Data
-------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
#if HAS_POLY_TYPEABLE
deriving instance Typeable Fix
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)
#else
instance Typeable1 f => Typeable (Fix f) where
   typeOf t = mkTyConApp fixTyCon [typeOf1 (undefined `asArgsTypeOf` t)]
     where asArgsTypeOf :: f a -> Fix f -> f a
           asArgsTypeOf = const

fixTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
fixTyCon = mkTyCon3 "recursion-schemes" "Data.Functor.Foldable" "Fix"
#else
fixTyCon = mkTyCon "Data.Functor.Foldable.Fix"
#endif
{-# NOINLINE fixTyCon #-}

instance (Typeable1 f, Data (f (Fix f))) => Data (Fix f) where
  gfoldl f z (Fix a) = z Fix `f` a
  toConstr _ = fixConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z (Fix))
    _ -> error "gunfold"
  dataTypeOf _ = fixDataType

fixConstr :: Constr
fixConstr = mkConstr fixDataType "Fix" [] Prefix

fixDataType :: DataType
fixDataType = mkDataType "Data.Functor.Foldable.Fix" [fixConstr]
#endif
#endif

-------------------------------------------------------------------------------
-- Mu
-------------------------------------------------------------------------------

-- | Least fixed point. Efficient folding.
newtype Mu f = Mu { unMu :: forall a. (f a -> a) -> a }

instance (Functor f, Eq1 f) => Eq (Mu f) where
    (==) = (==) `on` foldMu Fix

instance (Functor f, Ord1 f) => Ord (Mu f) where
    compare = compare `on` foldMu Fix

instance (Functor f, Show1 f) => Show (Mu f) where
    showsPrec d f = showParen (d > 10) $
        showString "unfoldMu unFix " . showsPrec 11 (foldMu Fix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Mu f) where
    readPrec = parens $ prec 10 $ do
        Ident "unfoldMu" <- lexP
        Ident "unFix" <- lexP
        fmap (unfoldMu unFix) (step readPrec)
#endif

-- | Change base functor in 'Mu'.
hoistMu :: (forall a. f a -> g a) -> Mu f -> Mu g
hoistMu n (Mu mk) = Mu $ \roll -> mk (roll . n)

-- | Fold 'Mu'.
--
-- >>> let mu = unfoldMu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldMu (elimListF 0 (+)) mu
-- 6
foldMu :: (f a -> a) -> Mu f -> a
foldMu f (Mu mk) = mk f

-- | Unfold 'Mu'.
--
-- >>> unfoldMu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- unfoldMu unFix (Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil)))))))))
unfoldMu :: Functor f => (a -> f a) -> a -> Mu f
unfoldMu f x = Mu $ \mk -> refold mk f x

-------------------------------------------------------------------------------
-- Nu
-------------------------------------------------------------------------------

-- | Greatest fixed point. Efficient unfolding.
data Nu f = forall a. Nu (a -> f a) a

instance (Functor f, Eq1 f) => Eq (Nu f) where
    (==) = (==) `on` foldNu Fix

instance (Functor f, Ord1 f) => Ord (Nu f) where
    compare = compare `on` foldNu Fix

instance (Functor f, Show1 f) => Show (Nu f) where
    showsPrec d f = showParen (d > 10) $
        showString "unfoldNu unFix " . showsPrec 11 (foldNu Fix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Nu f) where
    readPrec = parens $ prec 10 $ do
        Ident "unfoldNu" <- lexP
        Ident "unFix" <- lexP
        fmap (unfoldNu unFix) (step readPrec)
#endif

-- | Change base functor in 'Nu'.
hoistNu :: (forall a. f a -> g a) -> Nu f -> Nu g
hoistNu n (Nu next seed) = Nu (n . next) seed

-- | Fold 'Nu'.
--
-- >>> let nu = unfoldNu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldNu (elimListF 0 (+)) nu
-- 6
--
foldNu :: Functor f => (f a -> a) -> Nu f -> a
foldNu f (Nu next seed) = refold f next seed

-- | Unfold 'Nu'.
--
-- >>> unfoldNu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- unfoldNu unFix (Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil)))))))))
unfoldNu :: (a -> f a) -> a -> Nu f
unfoldNu = Nu

-------------------------------------------------------------------------------
-- refold
-------------------------------------------------------------------------------

-- | Refold one recursive type into another, one layer at the time.
--
refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = h where h = f . fmap h . g

-------------------------------------------------------------------------------
-- Monadic variants
-------------------------------------------------------------------------------

-- | Monadic 'foldFix'.
--
foldFixM:: (Monad m, Traversable t)
    => (t a -> m a) -> Fix t -> m a
foldFixM f = go where go = (f =<<) . mapM go . unFix

-- | Monadic anamorphism.
unfoldFixM :: (Monad m, Traversable t)
    => (a -> m (t a)) -> (a -> m (Fix t))
unfoldFixM f = go where go = liftM Fix . (mapM go =<<) . f

-- | Monadic hylomorphism.
refoldM :: (Monad m, Traversable t)
    => (t b -> m b) -> (a -> m (t a)) -> (a -> m b)
refoldM phi psi = go where go = (phi =<<) . (mapM go =<<) . psi

-------------------------------------------------------------------------------
-- Deprecated aliases
-------------------------------------------------------------------------------

-- | Catamorphism or generic function fold.
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata = foldFix
{-# DEPRECATED cata "Use foldFix" #-}

-- | Anamorphism or generic function unfold.
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana = unfoldFix
{-# DEPRECATED ana "Use unfoldFix" #-}

-- | Hylomorphism is anamorphism followed by catamorphism.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b)
hylo = refold
{-# DEPRECATED hylo "Use refold" #-}

-- | Monadic catamorphism.
cataM :: (Monad m, Traversable t)
    => (t a -> m a) -> Fix t -> m a
cataM = foldFixM
{-# DEPRECATED cataM "Use foldFixM" #-}

-- | Monadic anamorphism.
anaM :: (Monad m, Traversable t)
    => (a -> m (t a)) -> (a -> m (Fix t))
anaM = unfoldFixM
{-# DEPRECATED anaM "Use unfoldFixM" #-}

-- | Monadic hylomorphism.
hyloM :: (Monad m, Traversable t)
    => (t b -> m b) -> (a -> m (t a)) -> (a -> m b)
hyloM = refoldM
{-# DEPRECATED hyloM "Use refoldM" #-}
