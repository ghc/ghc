{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}

module Fold
    (
    -- * Types
      Step(..)
    , Fold (..)
    , sum
    , chunksOf
    , drain
    , drainBy
    )
where

import Data.Bifunctor (Bifunctor(..))
#if defined(FUSION_PLUGIN)
import Fusion.Plugin.Types (Fuse(..))
#endif
import Prelude hiding (Foldable(..), take)

------------------------------------------------------------------------------
-- Step of a fold
------------------------------------------------------------------------------

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | Represents the result of the @step@ of a 'Fold'.  'Partial' returns an
-- intermediate state of the fold, the fold step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Done' returns the final result and the fold cannot be driven further.
--
-- /Pre-release/
--
#if defined(FUSION_PLUGIN)
{-# ANN type Step Fuse #-}
#endif
data Step s b
    = Partial !s
    | Done !b

-- | 'first' maps over 'Partial' and 'second' maps over 'Done'.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second f (Done a) = Done (f a)

-- | 'fmap' maps over 'Done'.
--
-- @
-- fmap = 'second'
-- @
--
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

{-
-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial s -> pure $ Partial s
        Done b -> Done <$> f b
-}

------------------------------------------------------------------------------
-- The Fold type
------------------------------------------------------------------------------

-- | The type @Fold m a b@ having constructor @Fold step initial extract@
-- represents a fold over an input stream of values of type @a@ to a final
-- value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator, the type @s@ is
-- internal to the specific fold definition. The initial value of the fold
-- state @s@ is returned by @initial@. The @step@ function consumes an input
-- and either returns the final result @b@ if the fold is done or the next
-- intermediate state (see 'Step'). At any point the fold driver can extract
-- the result from the intermediate state using the @extract@ function.
--
-- NOTE: The constructor is not yet exposed via exposed modules, smart
-- constructors are provided to create folds.  If you think you need the
-- constructor of this type please consider using the smart constructors in
-- "Streamly.Internal.Data.Fold' instead.
--
-- /since 0.8.0 (type changed)/
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step1 initial1 extract) = Fold step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = f a >>= step x

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @foldl'*@ constructors.
--
-- A fold with an extract function can be expressed using fmap:
--
-- @
-- mkfoldlx :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
-- mkfoldlx step initial extract = fmap extract (foldl' step initial)
-- @
--
-- See also: "Streamly.Prelude.foldl'"
--
-- @since 0.8.0
--
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Fold m a b
foldl' step initial =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return

-- | Determine the sum of all elements of a stream of numbers. Returns additive
-- identity (@0@) when the stream is empty. Note that this is not numerically
-- stable for floating point numbers.
--
-- > sum = fmap getSum $ Fold.foldMap Sum
--
-- @since 0.7.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => Fold m a a
sum =  foldl' (+) 0

data Tuple' a b = Tuple' !a !b deriving Show

{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Fold m a b
take n (Fold fstep finitial fextract) = Fold step initial extract

    where

    initial = do
        res <- finitial
        case res of
            Partial s ->
                if n > 0
                then return $ Partial $ Tuple' 0 s
                else Done <$> fextract s
            Done b -> return $ Done b

    step (Tuple' i r) a = do
        res <- fstep r a
        case res of
            Partial sres -> do
                let i1 = i + 1
                    s1 = Tuple' i1 sres
                if i1 < n
                then return $ Partial s1
                else Done <$> fextract sres
            Done bres -> return $ Done bres

    extract (Tuple' _ r) = fextract r

#if defined(FUSION_PLUGIN)
{-# ANN type ManyState Fuse #-}
#endif
data ManyState s1 s2
    = ManyFirst !s1 !s2
    | ManyLoop !s1 !s2

-- | Collect zero or more applications of a fold.  @many split collect@ applies
-- the @split@ fold repeatedly on the input stream and accumulates zero or more
-- fold results using @collect@.
--
-- >>> two = Fold.take 2 Fold.toList
-- >>> twos = Fold.many two Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- Stops when @collect@ stops.
--
-- See also: "Streamly.Prelude.concatMap", "Streamly.Prelude.foldMany"
--
-- @since 0.8.0
--
{-# INLINE many #-}
many :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
many (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE handleSplitStep #-}
    handleSplitStep branch cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ branch ss1 cs
            Done sb -> runCollector ManyFirst cs sb

    {-# INLINE handleCollectStep #-}
    handleCollectStep branch cres =
        case cres of
            Partial cs -> do
                sres <- sinitial
                handleSplitStep branch cs sres
            Done cb -> return $ Done cb

    -- Do not inline this
    runCollector branch cs sb = cstep cs sb >>= handleCollectStep branch

    initial = cinitial >>= handleCollectStep ManyFirst

    {-# INLINE step_ #-}
    step_ ss cs a = do
        sres <- sstep ss a
        handleSplitStep ManyLoop cs sres

    {-# INLINE step #-}
    step (ManyFirst ss cs) a = step_ ss cs a
    step (ManyLoop ss cs) a = step_ ss cs a

    extract (ManyFirst _ cs) = cextract cs
    extract (ManyLoop ss cs) = do
        sb <- sextract ss
        cres <- cstep cs sb
        case cres of
            Partial s -> cextract s
            Done b -> return b

{-# INLINE chunksOf #-}
chunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksOf n split = many (take n split)

{-# INLINABLE drain #-}
drain :: Monad m => Fold m a ()
drain = foldl' (\_ _ -> ()) ()

{-# INLINABLE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Fold m a ()
drainBy f = lmapM f drain
