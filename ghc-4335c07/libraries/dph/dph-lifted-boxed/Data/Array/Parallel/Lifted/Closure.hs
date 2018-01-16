{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Closures.
--   Used when closure converting the source program during vectorisation.
module Data.Array.Parallel.Lifted.Closure 
        ( -- * Closures.
          (:->)(..)
        , ($:)

        -- * Array Closures.
        , PData(..), PDatas(..)
        , ($:^), liftedApply

        -- * Closure Construction.
        , closure1,  closure2,  closure3,  closure4
        , closure1', closure2', closure3', closure4')
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import qualified Data.Vector            as V
import GHC.Exts


-- Closures -------------------------------------------------------------------
-- | Define the fixity of the closure type constructor.
infixr 0 :->
infixl 1 $:, $:^

-- | The type of closures.
--   This bundles up:
---   1) the 'vectorised' version of the function that takes an explicit environment
--    2) the 'lifted' version, that works on arrays.
--       The first parameter of the lifted version is the 'lifting context'
--       that gives the length of the arrays being operated on.
--    3) the environment of the closure.
-- 
--   The vectoriser closure-converts the source program so that all functions
--   are expressed in this form.
data (a :-> b)
        = forall env. PA env
        => Clo  (env -> a -> b)
                (Int -> PData env -> PData a -> PData b)
                env

-- | Closure application.
($:) :: (a :-> b) -> a -> b
($:) (Clo fv _fl env) x  = fv env x
{-# INLINE_CLOSURE ($:) #-}


-- Array Closures -------------------------------------------------------------
-- | Arrays of closures (aka array closures)
--   We need to represent arrays of closures when vectorising partial applications.
--
--   For example, consider:
--     @mapP (+) xs   ::  [: Int -> Int :]@
--
--   Representing this an array of thunks doesn't work because we can't evaluate
--   it in a data parallel manner. Instead, we want *one* function applied to many
--   array elements.
-- 
--   Instead, such an array of closures is represented as the vectorised  and
--   lifted versions of (+), along with an environment array xs that contains the
--   partially applied arguments.
--
--     @mapP (+) xs  ==>  AClo plus_v plus_l xs@
--
data instance PData (a :-> b)
        =  forall env. PA env
        => AClo  (env -> a -> b)
                 (Int -> PData env -> PData a -> PData b)
                 (PData env)

data instance PDatas (a :-> b)
        =  forall env. PA env
        => AClos (env -> a -> b)
                 (Int -> PData env -> PData a -> PData b)
                 (PDatas env)


-- | Lifted closure application.
($:^) :: PArray (a :-> b) -> PArray a -> PArray b
PArray n# (AClo _ f es) $:^ PArray _ as 
        = PArray n# (f (I# n#) es as)
{-# INLINE ($:^) #-}


-- | Lifted closure application, taking an explicit lifting context.
liftedApply :: Int -> PData (a :-> b) -> PData a -> PData b
liftedApply n (AClo _ fl envs) as
        = fl n envs as
{-# INLINE_CLOSURE liftedApply #-}


-- Closure Construction -------------------------------------------------------
-- These functions are used for building closure representations of primitive
-- functions. They're used in D.A.P.Lifted.Combinators where we define the 
-- closure converted lifted array combinators that vectorised code uses.

-- | Construct an arity-1 closure,
--   from unlifted and lifted versions of a primitive function.
closure1 
        :: (a -> b)
        -> (Int -> PData a -> PData b)
        -> (a :-> b)

closure1 fv fl  
        = Clo   (\_env -> fv)
                (\n _env -> fl n)
                ()
{-# INLINE_CLOSURE closure1 #-}


-- | Construct an arity-2 closure,
--   from lifted and unlifted versions of a primitive function.
closure2 
        :: forall a b c. PA a
        => (a -> b -> c)
        -> (Int -> PData a -> PData b -> PData c)
        -> (a :-> b :-> c)

closure2 fv fl
 = let  fv_1 _ xa   = Clo fv fl xa
        fl_1 _ _ xs = AClo fv fl xs
        
   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure2 #-}


-- | Construct an arity-3 closure
--   from lifted and unlifted versions of a primitive function.
closure3 
        :: forall a b c d. (PA a, PA b)
        => (a -> b -> c -> d)
        -> (Int -> PData a -> PData b -> PData c -> PData d)
        -> (a :-> b :-> c :-> d)
        
closure3 fv fl
 = let  fv_1   _ xa = Clo   fv_2 fl_2 xa
        fl_1 _ _ xs = AClo  fv_2 fl_2 xs

        -----
        fv_2 xa yb   = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys = AClo fv_3 fl_3 (PTuple2 xs ys)

        -----
        fv_3 (xa, yb) zc           = fv xa yb zc
        fl_3 n (PTuple2 xs ys) zs  = fl n xs ys zs

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure3 #-}

-- | Construct an arity-4 closure
--   from lifted and unlifted versions of a primitive function.
closure4 
        :: forall a b c d e. (PA a, PA b, PA c)
        => (a -> b -> c -> d -> e)
        -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e)
        -> (a :-> b :-> c :-> d :-> e)
        
closure4 fv fl
 = let  fv_1   _ xa = Clo   fv_2 fl_2 xa
        fl_1 _ _ xs = AClo  fv_2 fl_2 xs

        -----
        fv_2 xa yb   = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys = AClo fv_3 fl_3 (PTuple2 xs ys)

        -----
        fv_3 (xa, yb) zc           = Clo  fv_4 fl_4 ((xa, yb), zc)
        fl_3 _ (PTuple2 xs ys) zs  = AClo fv_4 fl_4 (PTuple2 (PTuple2 xs ys) zs)

        -----
        fv_4 ((xa, yb), zc)   wd        = fv xa yb zc wd
        fl_4 n (PTuple2 (PTuple2 xs ys) zs)  ws  = fl n xs ys zs ws

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure4 #-}


-- Closure constructors that take PArrays -------------------------------------
-- These versions are useful when defining prelude functions such as in 
-- D.A.P.Prelude.Int. They let us promote functions that work on PArrays 
-- to closures, while inferring the lifting context from the first argument.

-- | Construct an arity-1 closure.
closure1'
        :: forall a b
        .  (a -> b)
        -> (PArray a -> PArray b)
        -> (a :-> b)

closure1' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) pdata
         = case fl (PArray n# pdata) of
                 PArray _ pdata' -> pdata'
   in   closure1 fv fl'
{-# INLINE_CLOSURE closure1' #-}


-- | Construct an arity-2 closure.
closure2'
        :: forall a b c. PA a
        => (a -> b -> c)
        -> (PArray a -> PArray b -> PArray c)
        -> (a :-> b :-> c)

closure2' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) pdata1 pdata2
         = case fl (PArray n# pdata1) (PArray n# pdata2) of
                 PArray _ pdata' -> pdata'
   in   closure2 fv fl'
{-# INLINE_CLOSURE closure2' #-}


-- | Construct an arity-3 closure.
closure3'
        :: forall a b c d. (PA a, PA b) 
        => (a -> b -> c -> d)
        -> (PArray a -> PArray b -> PArray c -> PArray d)
        -> (a :-> b :-> c :-> d) 

closure3' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) pdata1 pdata2 pdata3
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) of
                 PArray _ pdata' -> pdata'
   in   closure3 fv fl'
{-# INLINE_CLOSURE closure3' #-}


-- | Construct an arity-3 closure.
closure4'
        :: forall a b c d e. (PA a, PA b, PA c) 
        => (a -> b -> c -> d -> e)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e)
        -> (a :-> b :-> c :-> d :-> e) 

closure4' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) pdata1 pdata2 pdata3 pdata4
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) (PArray n# pdata4) of
                 PArray _ pdata' -> pdata'
   in   closure4 fv fl'
{-# INLINE_CLOSURE closure4' #-}


-- PData instance for closures ------------------------------------------------
-- This needs to be here instead of in a module D.A.P.PArray.PData.Closure
-- to break an import loop.
--
instance PR (a :-> b) where
 toVectorPR (AClo fv fl envs)
        = V.map (Clo fv fl) $ toVectorPA envs

 fromVectorPR 
        = error $ unlines
        [ "Data.Array.Parallel.Lifted.Closure.fromVector"
        , "  can't create closure array of unknown vector of functions" ]

-- PRepr Instance -------------------------------------------------------------
-- This needs to be here instead of in D.A.P.PRepr.Instances 
-- to break an import loop.
--
type instance PRepr (a :-> b) 
        = a :-> b

instance (PA a, PA b) => PA (a :-> b) where
  toPRepr       = id
  fromPRepr     = id
  toArrPRepr    = id
  fromArrPRepr  = id
  toArrPReprs   = id
  fromArrPReprs = id
