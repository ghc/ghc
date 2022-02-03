-- | Utilities related to Monad and Applicative classes
--   Mostly for backwards compatibility.

module GHC.Utils.Monad
        ( Applicative(..)
        , (<$>)

        , MonadFix(..)
        , MonadIO(..)

        , zipWith3M, zipWith3M_, zipWith4M, zipWithAndUnzipM
        , mapAndUnzipM, mapAndUnzip3M, mapAndUnzip4M, mapAndUnzip5M
        , mapAccumLM
        , liftFstM, liftSndM
        , mapSndM
        , concatMapM
        , mapMaybeM
        , fmapMaybeM, fmapEitherM
        , anyM, allM, orM
        , foldlM, foldlM_, foldrM
        , maybeMapM
        , whenM, unlessM
        , filterOutM
        ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import GHC.Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable (sequenceA_, foldlM, foldrM)
import Data.List (unzip4, unzip5, zipWith4)

-------------------------------------------------------------------------------
-- Common functions
--  These are used throughout the compiler
-------------------------------------------------------------------------------

{-

Note [Inline @zipWithNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle for 'zipWith3M', 'zipWith4M' and 'zipWith3M_' is the same
as for 'zipWithM' and 'zipWithM_' in "Control.Monad", see
Note [Fusion for zipN/zipWithN] in GHC/List.hs for more details.

The 'zipWithM'/'zipWithM_' functions are inlined so that the `zipWith` and
`sequenceA` functions with which they are defined have an opportunity to fuse.

Furthermore, 'zipWith3M'/'zipWith4M' and 'zipWith3M_' have been explicitly
rewritten in a non-recursive way similarly to 'zipWithM'/'zipWithM_', and for
more than just uniformity: after [D5241](https://phabricator.haskell.org/D5241)
for issue #14037, all @zipN@/@zipWithN@ functions fuse, meaning
'zipWith3M'/'zipWIth4M' and 'zipWith3M_'@ now behave like 'zipWithM' and
'zipWithM_', respectively, with regards to fusion.

As such, since there are not any differences between 2-ary 'zipWithM'/
'zipWithM_' and their n-ary counterparts below aside from the number of
arguments, the `INLINE` pragma should be replicated in the @zipWithNM@
functions below as well.

-}

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
{-# INLINE zipWith3M #-}
-- Inline so that fusion with 'zipWith3' and 'sequenceA' has a chance to fire.
-- See Note [Inline @zipWithNM@ functions] above.
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
{-# INLINE zipWith3M_ #-}
-- Inline so that fusion with 'zipWith4' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith3M_ f xs ys zs = sequenceA_ (zipWith3 f xs ys zs)

zipWith4M :: Monad m => (a -> b -> c -> d -> m e)
          -> [a] -> [b] -> [c] -> [d] -> m [e]
{-# INLINE zipWith4M #-}
-- Inline so that fusion with 'zipWith5' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith4M f xs ys ws zs = sequenceA (zipWith4 f xs ys ws zs)

zipWithAndUnzipM :: Monad m
                 => (a -> b -> m (c, d)) -> [a] -> [b] -> m ([c], [d])
{-# INLINABLE zipWithAndUnzipM #-}  -- this allows specialization to a given monad
zipWithAndUnzipM f (x:xs) (y:ys)
  = do { (c, d) <- f x y
       ; (cs, ds) <- zipWithAndUnzipM f xs ys
       ; return (c:cs, d:ds) }
zipWithAndUnzipM _ _ _ = return ([], [])

{-

Note [Inline @mapAndUnzipNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle is the same as 'mapAndUnzipM' in "Control.Monad".
The 'mapAndUnzipM' function is inlined so that the `unzip` and `traverse`
functions with which it is defined have an opportunity to fuse, see
Note [Inline @unzipN@ functions] in Data/OldList.hs for more details.

Furthermore, the @mapAndUnzipNM@ functions have been explicitly rewritten in a
non-recursive way similarly to 'mapAndUnzipM', and for more than just
uniformity: after [D5249](https://phabricator.haskell.org/D5249) for Trac
ticket #14037, all @unzipN@ functions fuse, meaning 'mapAndUnzip3M',
'mapAndUnzip4M' and 'mapAndUnzip5M' now behave like 'mapAndUnzipM' with regards
to fusion.

As such, since there are not any differences between 2-ary 'mapAndUnzipM' and
its n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @mapAndUnzipNM@ functions below as well.

-}

-- | mapAndUnzipM for triples
mapAndUnzip3M :: Monad m => (a -> m (b,c,d)) -> [a] -> m ([b],[c],[d])
{-# INLINE mapAndUnzip3M #-}
-- Inline so that fusion with 'unzip3' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip3M f xs =  unzip3 <$> traverse f xs

mapAndUnzip4M :: Monad m => (a -> m (b,c,d,e)) -> [a] -> m ([b],[c],[d],[e])
{-# INLINE mapAndUnzip4M #-}
-- Inline so that fusion with 'unzip4' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip4M f xs =  unzip4 <$> traverse f xs

mapAndUnzip5M :: Monad m => (a -> m (b,c,d,e,f)) -> [a] -> m ([b],[c],[d],[e],[f])
{-# INLINE mapAndUnzip5M #-}
-- Inline so that fusion with 'unzip5' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip5M f xs =  unzip5 <$> traverse f xs

-- TODO: mapAccumLM is used in many places. Surely most of
-- these don't actually want to be lazy. We should add a strict
-- variant and use it where appropriate.

-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM f s xs =
  go s xs
  where
    go s (x:xs) = do
      (s1, x')  <- f s x
      (s2, xs') <- go s1 xs
      return    (s2, x' : xs')
    go s [] = return (s, [])

-- | Monadic version of mapSnd
mapSndM :: Monad m => (b -> m c) -> [(a,b)] -> m [(a,c)]
mapSndM f xs = go xs
  where
    go []         = return []
    go ((a,b):xs) = do { c <- f b; rs <- go xs; return ((a,c):rs) }

liftFstM :: Monad m => (a -> b) -> m (a, r) -> m (b, r)
liftFstM f thing = do { (a,r) <- thing; return (f a, r) }

liftSndM :: Monad m => (a -> b) -> m (r, a) -> m (r, b)
liftSndM f thing = do { (r,a) <- thing; return (r, f a) }

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Applicative version of mapMaybe
mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where g a = liftA2 (maybe id (:)) (f a)

-- | Monadic version of fmap
fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = f x >>= (return . Just)

-- | Monadic version of fmap
fmapEitherM :: Monad m => (a -> m b) -> (c -> m d) -> Either a c -> m (Either b d)
fmapEitherM fl _ (Left  a) = fl a >>= (return . Left)
fmapEitherM _ fr (Right b) = fr b >>= (return . Right)

-- | Monadic version of 'any', aborts the computation at the first @True@ value
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = go xs
  where
    go [] = return False
    go (x:xs) = do b <- f x
                   if b then return True
                        else go xs

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f bs = go bs
  where
    go []     = return True
    go (b:bs) = (f b) >>= (\bv -> if bv then go bs else return False)

-- | Monadic version of or
orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

-- | Monadic version of foldl that discards its result
foldlM_ :: (Monad m, Foldable t) => (a -> b -> m a) -> a -> t b -> m ()
foldlM_ = foldM_

-- | Monadic version of fmap specialised for Maybe
maybeMapM :: Monad m => (a -> m b) -> (Maybe a -> m (Maybe b))
maybeMapM _ Nothing  = return Nothing
maybeMapM m (Just x) = liftM Just $ m x

-- | Monadic version of @when@, taking the condition in the monad
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb
                    ; when b thing }

-- | Monadic version of @unless@, taking the condition in the monad
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = do { cond <- condM
                       ; unless cond acc }

-- | Like 'filterM', only it reverses the sense of the test.
filterOutM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterOutM p =
  foldr (\ x -> liftA2 (\ flg -> if flg then id else (x:)) (p x)) (pure [])

{- Note [The one-shot state monad trick]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Summary: many places in GHC use a state monad, and we really want those
functions to be eta-expanded (#18202).

The problem
~~~~~~~~~~~
Consider
    newtype M a = MkM (State -> (State, a))

    instance Monad M where
       mf >>= k = MkM (\s -> case mf  of MkM f  ->
                             case f s of (s',r) ->
                             case k r of MkM g  ->
                             g s')

    fooM :: Int -> M Int
    fooM x = g y >>= \r -> h r
      where
        y = expensive x

Now suppose you say (repeat 20 (fooM 4)), where
  repeat :: Int -> M Int -> M Int
performs its argument n times.  You would expect (expensive 4) to be
evaluated only once, not 20 times.  So foo should have arity 1 (not 2);
it should look like this (modulo casts)

  fooM x = let y = expensive x in
           \s -> case g y of ...

But creating and then repeating, a monadic computation is rare.  If you
/aren't/ re-using (M a) value, it's /much/ more efficient to make
foo have arity 2, thus:

  fooM x s = case g (expensive x) of ...

Why more efficient?  Because now foo takes its argument both at once,
rather than one at a time, creating a heap-allocated function closure. See
https://www.joachim-breitner.de/blog/763-Faster_Winter_5__Eta-Expanding_ReaderT
for a very good explanation of the issue which led to these optimisations
into GHC.

The trick
~~~~~~~~~
With state monads like M the general case is that we *aren't* reusing
(M a) values so it is much more efficient to avoid allocating a
function closure for them. So the state monad trick is a way to keep
the monadic syntax but to make GHC eta-expand functions like `fooM`.
To do that we use the "oneShot" magic function.

Here is the trick:
  * Define a "smart constructor"
       mkM :: (State -> (State,a)) -> M a
       mkM f = MkM (oneShot m)

  * Never call MkM directly, as a constructor.  Instead, always call mkM.

And that's it!  The magic 'oneShot' function does this transformation:
   oneShot (\s. e)  ==>   \s{os}. e
which pins a one-shot flag {os} onto the binder 's'.  That tells GHC
that it can assume the lambda is called only once, and thus can freely
float computations in and out of the lambda.

To be concrete, let's see what happens to fooM:

 fooM = \x. g (expensive x) >>= \r -> h r
      = \x. let mf = g (expensive x)
                k  = \r -> h r
            in MkM (oneShot (\s -> case mf  of MkM' f  ->
                                   case f s of (s',r) ->
                                   case k r of MkM' g  ->
                                   g s'))
      -- The MkM' are just newtype casts nt_co
      = \x. let mf = g (expensive x)
                k  = \r -> h r
            in (\s{os}. case (mf |> nt_co) s of (s',r) ->
                        (k r) |> nt_co s')
               |> sym nt_co

      -- Crucial step: float let-bindings into that \s{os}
      = \x. (\s{os}. case (g (expensive x) |> nt_co) s of (s',r) ->
                     h r |> nt_co s')
            |> sym nt_co

and voila! fooM has arity 2.

The trick is very similar to the built-in "state hack"
(see Note [The state-transformer hack] in "GHC.Core.Opt.Arity") but is
applicable on a monad-by-monad basis under programmer control.

Using pattern synonyms
~~~~~~~~~~~~~~~~~~~~~~
Using a smart constructor is fine, but there is no way to check that we
have found *all* uses, especially if the uses escape a single module.
A neat (but more sophisticated) alternative is to use pattern synonyms:

   -- We rename the existing constructor.
   newtype M a = MkM' (State -> (State, a))

   -- The pattern has the old constructor name.
   pattern MkM f <- MkM' f
      where
        MkM f = MkM' (oneShot f)

Now we can simply grep to check that there are no uses of MkM'
/anywhere/, to guarantee that we have not missed any.  (Using the
smart constructor alone we still need the data constructor in
patterns.)  That's the advantage of the pattern-synonym approach, but
it is more elaborate.

The pattern synonym approach is due to Sebastian Graaf (#18238)

Do note that for monads for multiple arguments more than one oneShot
function might be required. For example in FCode we use:

    newtype FCode a = FCode' { doFCode :: StgToCmmConfig -> CgState -> (a, CgState) }

    pattern FCode :: (StgToCmmConfig -> CgState -> (a, CgState))
                  -> FCode a
    pattern FCode m <- FCode' m
      where
        FCode m = FCode' $ oneShot (\cgInfoDown -> oneShot (\state ->m cgInfoDown state))

INLINE pragmas and (>>)
~~~~~~~~~~~~~~~~~~~~~~~
A nasty gotcha is described in #20008.  In brief, be careful if you get (>>) via
its default method:

    instance Applicative M where
      pure a = MkM (\s -> (s, a))
      (<*>)  = ap

    instance Monad UM where
      {-# INLINE (>>=) #-}
      m >>= k  = MkM (\s -> blah)

Here we define (>>), via its default method, in terms of (>>=). If you do this,
be sure to put an INLINE pragma on (>>=), as above.  That tells it to inline
(>>=) in the RHS of (>>), even when it is applied to only two arguments, which
in turn conveys the one-shot info from (>>=) to (>>).  Lacking the INLINE, GHC
may eta-expand (>>), and with a non-one-shot lambda.  #20008 has more discussion.

Derived instances
~~~~~~~~~~~~~~~~~
One caveat of both approaches is that derived instances don't use the smart
constructor /or/ the pattern synonym. So they won't benefit from the automatic
insertion of "oneShot".

   data M a = MkM' (State -> (State,a))
            deriving (Functor) <-- Functor implementation will use MkM'!

Conclusion: don't use 'derviving' in these cases.

Multi-shot actions (cf #18238)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes we really *do* want computations to be shared! Remember our
example (repeat 20 (fooM 4)). See Note [multiShotIO] in GHC.Types.Unique.Supply

We can force fooM to have arity 1 using multiShot:

    fooM :: Int -> M Int
    fooM x = multiShotM (g y >>= \r -> h r)
      where
        y = expensive x

    multiShotM :: M a -> M a
    {-# INLINE multiShotM #-}
    multiShotM (MkM m) = MkM (\s -> inline m s)
         -- Really uses the data constructor,
         -- not the smart constructor!

Now we can see how fooM optimises (ignoring casts)

   multiShotM (g y >>= \r -> h r)
   ==> {inline (>>=)}
       multiShotM (\s{os}. case g y s of ...)
   ==> {inline multiShotM}
       let m = \s{os}. case g y s of ...
       in \s. inline m s
   ==> {inline m}
       \s. (\s{os}. case g y s of ...) s
   ==> \s. case g y s of ...

and voila! the one-shot flag has gone.  It's possible that y has been
replaced by (expensive x), but full laziness should pull it back out.
(This part seems less robust.)

The magic `inline` function does two things
* It prevents eta reduction.  If we wrote just
      multiShotIO (IO m) = IO (\s -> m s)
  the lamda would eta-reduce to 'm' and all would be lost.

* It helps ensure that 'm' really does inline.

Note that 'inline' evaporates in phase 0.  See Note [inlineId magic]
in GHC.Core.Opt.ConstantFold.match_inline.

The INLINE pragma on multiShotM is very important, else the
'inline' call will evaporate when compiling the module that
defines 'multiShotM', before it is ever exported.
-}
