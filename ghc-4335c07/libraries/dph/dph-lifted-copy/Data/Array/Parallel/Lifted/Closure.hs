{-# OPTIONS -fno-warn-missing-methods #-}
module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  mkClosure, mkClosureP, ($:), ($:^),
  closure, liftedClosure, liftedApply,

  closure1, closure2, closure3, closure4, closure5,
  closure6, closure7, closure8
) where
import Data.Array.Parallel.PArray.PReprInstances ()
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.Lifted.PArray

import GHC.Exts (Int#)

infixr 0 :->
infixl 0 $:, $:^

-- | The type of closures.
--   This bundles up:
--      1) the vectorised version of the function that takes an explicit environment
--      2) the lifted version, that works on arrays.
--           the first parameter of this function is the 'lifting context'
--           that gives the length of the array.
--      3) the environment of the closure.
-- 
--   The vectoriser closure-converts the source program so that all functions
--   types are expressed in this form.
--
data a :-> b 
  = forall e. PA e 
  => Clo !(e -> a -> b)                            -- vectorised function
         !(Int# -> PData e -> PData a -> PData b)  -- lifted function
         e                                         -- environment


-- | Apply a lifted function by wrapping up the provided array data
--   into some real `PArray`s, and passing it those.
lifted  :: (PArray e -> PArray a -> PArray b)      -- ^ lifted function to call.
        -> Int#                                    -- ^ lifting context
        -> PData e                                 -- ^ environments 
        -> PData a                                 -- ^ arguments
        -> PData b                                 -- ^ returned elements
{-# INLINE lifted #-}
lifted f n# es as 
  = case f (PArray n# es) (PArray n# as) of
     PArray _ bs -> bs


-- | Construct a closure.
mkClosure 
        :: forall a b e
        .  PA e
        => (e -> a -> b)                           -- ^ vectorised function, with explicit environment.
        -> (PArray e -> PArray a -> PArray b)      -- ^ lifted function, taking an array of environments.
        -> e                                       -- ^ environment
        -> (a :-> b)
{-# INLINE CONLIKE mkClosure #-}
mkClosure fv fl e
  = Clo fv (lifted fl) e


-- | Construct a closure.
--   This is like the `mkClosure` function above, except that the provided
--   lifted version of the function can take raw array data, instead of 
--   data wrapped up into a `PArray`.
closure :: forall a b e
        .  PA e
        => (e -> a -> b)                           -- ^ vectorised function, with explicit environment.
        -> (Int# -> PData e -> PData a -> PData b) -- ^ lifted function, taking an array of environments.
        -> e                                       -- ^ environment
        -> (a :-> b)
{-# INLINE closure #-}
closure fv fl e = Clo fv fl e


-- | Apply a closure to its argument.
--
($:) :: forall a b. (a :-> b) -> a -> b
{-# INLINE [1] ($:) #-}
Clo f _ e $: a = f e a

{-# RULES

"mkClosure/($:)" forall fv fl e x.
  mkClosure fv fl e $: x = fv e x

 #-}


-- | Arrays of closures (aka array closures)
--   We need to represent arrays of closures when vectorising partial applications.
--
--   For example, consider:
--     @mapP (+) xs   ::  [: Int -> Int :]@
--
--   Representing this an array of thunks doesn't work because we can't evaluate
--   in a data parallel manner. Instead, we want *one* function applied to many
--   array elements.
-- 
--   Instead, such an array of closures is represented as the vectorised 
--   and lifted versions of (+), along with an environment array xs that
--   contains the partially applied arguments.
--
--     @mapP (+) xs  ==>  AClo plus_v plus_l xs@
--
--   When we find out what the final argument is, we can then use the lifted
--   closure application function to compute the result:
--
--    @PArray n (AClo plus_v plus_l xs) $:^ (PArray n' ys) 
--           => PArray n (plus_l n xs ys)@
--
data instance PData (a :-> b)
  =  forall e. PA e 
  => AClo !(e -> a -> b)                           -- vectorised function, with explicit environment.
          !(Int# -> PData e -> PData a -> PData b) -- lifted function, taking an array of environments.
           (PData e)                               -- array of environments.


-- |Lifted closure construction
--
mkClosureP :: forall a b e.
              PA e => (e -> a -> b)
                   -> (PArray e -> PArray a -> PArray b)
                   -> PArray e -> PArray (a :-> b)
{-# INLINE mkClosureP #-}
mkClosureP fv fl (PArray n# es) 
  = PArray n# (AClo fv (lifted fl) es)


liftedClosure :: forall a b e.
                 PA e => (e -> a -> b)
                      -> (Int# -> PData e -> PData a -> PData b)
                      -> PData e
                      -> PData (a :-> b)
{-# INLINE liftedClosure #-}
liftedClosure fv fl es = AClo fv fl es


-- |Lifted closure application
--
($:^) :: forall a b. PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
PArray n# (AClo _ f es) $:^ PArray _ as 
  = PArray n# (f n# es as)


liftedApply :: forall a b. Int# -> PData (a :-> b) -> PData a -> PData b
{-# INLINE liftedApply #-}
liftedApply n# (AClo _ f es) as 
  = f n# es as


-- PRepr instance for closures ------------------------------------------------
type instance PRepr (a :-> b) = a :-> b

instance (PA a, PA b) => PA (a :-> b) where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id

instance PR (a :-> b) where
  {-# INLINE emptyPR #-}
  emptyPR = AClo (\_ _  -> error "empty array closure")
                 (\_ _  -> error "empty array closure")
                 (emptyPD :: PData ())

  {-# INLINE replicatePR #-}
  replicatePR n# (Clo f f' e) 
    = AClo f f' (replicatePD n# e)

  {-# INLINE replicatelPR #-}
  replicatelPR segd (AClo f f' es)
    = AClo f f' (replicatelPD segd es)

  {-# INLINE indexPR #-}
  indexPR (AClo f f' es) i#
    = Clo f f'  (indexPD es i#)

  {-# INLINE bpermutePR #-}
  bpermutePR (AClo f f' es) n# is
    = AClo f f' (bpermutePD es n# is)

  {-# INLINE packByTagPR #-}
  packByTagPR (AClo f f' es) n# tags t#
    = AClo f f' (packByTagPD es n# tags t#)


-- Closure construction -------------------------------------------------------
-- | Arity-1 closures.
closure1 :: (a -> b) -> (PArray a -> PArray b) -> (a :-> b)
{-# INLINE closure1 #-}
closure1 fv fl = mkClosure (\_ -> fv) (\_ -> fl) ()

-- | Arity-2 closures.
closure2 :: PA a
         => (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)

{-# INLINE closure2 #-}
closure2 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _ x  = mkClosure  fv fl x
    fl_1 _ xs = mkClosureP fv fl xs

-- | Arity-3 closures.
closure3 :: (PA a, PA b)
         => (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)

{-# INLINE closure3 #-}
closure3 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x,y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x,y) z = fv x y z
    fl_3 ps zs = case unzipPA# ps of (xs,ys) -> fl xs ys zs

-- | Arity-4 closures.
closure4 :: (PA a, PA b, PA c)
         => (a -> b -> c -> d -> e)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e)
         -> (a :-> b :-> c :-> d :-> e)

{-# INLINE closure4 #-}
closure4 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x, y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x, y)  z  = mkClosure  fv_4 fl_4 (x, y, z)
    fl_3 xys     zs = case unzipPA# xys of (xs, ys) -> mkClosureP fv_4 fl_4 (zip3PA# xs ys zs)

    fv_4 (x, y, z) v = fv x y z v
    fl_4 ps vs = case unzip3PA# ps of (xs, ys, zs) -> fl xs ys zs vs


-- | Arity-5 closures.
closure5 :: (PA a, PA b, PA c, PA d, PA e)
         => (a -> b -> c -> d -> e -> f)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f)
         -> (a :-> b :-> c :-> d :-> e :-> f)

{-# INLINE closure5 #-}
closure5 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x, y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x, y)  z  = mkClosure  fv_4 fl_4 (x, y, z)
    fl_3 xys     zs = case unzipPA# xys of (xs, ys) -> mkClosureP fv_4 fl_4 (zip3PA# xs ys zs)

    fv_4 (w, x, y) z = mkClosure fv_5 fl_5 (w, x, y, z) 
    fl_4 ps zs = case unzip3PA# ps of (ws, xs, ys) -> mkClosureP fv_5 fl_5  (zip4PA# ws xs ys zs)

    fv_5 (v, w, x, y) z = fv v w x y z
    fl_5 ps zs = case unzip4PA# ps of (vs, ws, xs, ys) -> fl vs ws xs ys zs


-- | Arity-6 closures.
closure6 :: (PA a, PA b, PA c, PA d, PA e, PA f)
         => (a -> b -> c -> d -> e -> f -> g)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g)

{-# INLINE closure6 #-}
closure6 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x, y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x, y)  z  = mkClosure  fv_4 fl_4 (x, y, z)
    fl_3 xys     zs = case unzipPA# xys of (xs, ys) -> mkClosureP fv_4 fl_4 (zip3PA# xs ys zs)

    fv_4 (w, x, y) z = mkClosure fv_5 fl_5 (w, x, y, z) 
    fl_4 ps zs = case unzip3PA# ps of (ws, xs, ys) -> mkClosureP fv_5 fl_5  (zip4PA# ws xs ys zs)

    fv_5 (v, w, x, y) z = mkClosure fv_6 fl_6 (v, w, x, y, z) 
    fl_5 ps zs = case unzip4PA# ps of (vs, ws, xs, ys) -> mkClosureP fv_6 fl_6  (zip5PA# vs ws xs ys zs)

    fv_6 (u, v, w, x, y) z = fv u v w x y z
    fl_6 ps zs = case unzip5PA# ps of (us, vs, ws, xs, ys) -> fl us vs ws xs ys zs

-- | Arity-7 closures.
closure7 :: (PA a, PA b, PA c, PA d, PA e, PA f, PA g)
         => (a -> b -> c -> d -> e -> f -> g -> h)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e 
         -> PArray f -> PArray g -> PArray h)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h)

{-# INLINE closure7 #-}
closure7 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x, y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x, y)  z  = mkClosure  fv_4 fl_4 (x, y, z)
    fl_3 xys     zs = case unzipPA# xys of (xs, ys) -> mkClosureP fv_4 fl_4 (zip3PA# xs ys zs)

    fv_4 (w, x, y) z = mkClosure fv_5 fl_5 (w, x, y, z) 
    fl_4 ps zs = case unzip3PA# ps of (ws, xs, ys) -> mkClosureP fv_5 fl_5  (zip4PA# ws xs ys zs)

    fv_5 (v, w, x, y) z = mkClosure fv_6 fl_6 (v, w, x, y, z) 
    fl_5 ps zs = case unzip4PA# ps of (vs, ws, xs, ys) -> mkClosureP fv_6 fl_6  (zip5PA# vs ws xs ys zs)

    fv_6 (u, v, w, x, y) z = mkClosure fv_7 fl_7 (u, v, w, x, y, z) 
    fl_6 ps zs = case unzip5PA# ps of (us, vs, ws, xs, ys) -> mkClosureP fv_7 fl_7  (zip6PA# us vs ws xs ys zs)

    fv_7 (t, u, v, w, x, y) z = fv t u v w x y z
    fl_7 ps zs = case unzip6PA# ps of (ts, us, vs, ws, xs, ys) -> fl ts us vs ws xs ys zs

-- | Arity-8 closures.
closure8 :: (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h)
         => (a -> b -> c -> d -> e -> f -> g -> h -> i)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e 
         -> PArray f -> PArray g -> PArray h -> PArray i)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i)

{-# INLINE closure8 #-}
closure8 fv fl = mkClosure fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  fv_2 fl_2 x
    fl_1 _  xs = mkClosureP fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  fv_3 fl_3 (x, y)
    fl_2 xs ys = mkClosureP fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x, y)  z  = mkClosure  fv_4 fl_4 (x, y, z)
    fl_3 xys     zs = case unzipPA# xys of (xs, ys) -> mkClosureP fv_4 fl_4 (zip3PA# xs ys zs)

    fv_4 (w, x, y) z = mkClosure fv_5 fl_5 (w, x, y, z) 
    fl_4 ps zs = case unzip3PA# ps of (ws, xs, ys) -> mkClosureP fv_5 fl_5  (zip4PA# ws xs ys zs)

    fv_5 (v, w, x, y) z = mkClosure fv_6 fl_6 (v, w, x, y, z) 
    fl_5 ps zs = case unzip4PA# ps of (vs, ws, xs, ys) -> mkClosureP fv_6 fl_6  (zip5PA# vs ws xs ys zs)

    fv_6 (u, v, w, x, y) z = mkClosure fv_7 fl_7 (u, v, w, x, y, z) 
    fl_6 ps zs = case unzip5PA# ps of (us, vs, ws, xs, ys) -> mkClosureP fv_7 fl_7  (zip6PA# us vs ws xs ys zs)

    fv_7 (t, u, v, w, x, y) z = mkClosure fv_8 fl_8 (t, u, v, w, x, y, z) 
    fl_7 ps zs = case unzip6PA# ps of (ts, us, vs, ws, xs, ys) -> mkClosureP fv_8 fl_8  (zip7PA# ts us vs ws xs ys zs)

    fv_8 (s, t, u, v, w, x, y) z = fv s t u v w x y z
    fl_8 ps zs = case unzip7PA# ps of (ss, ts, us, vs, ws, xs, ys) -> fl ss ts us vs ws xs ys zs

   
