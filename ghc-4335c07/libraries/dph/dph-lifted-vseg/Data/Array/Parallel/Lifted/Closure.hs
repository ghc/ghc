{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Closures.
--   Used when closure converting the source program during vectorisation.
module Data.Array.Parallel.Lifted.Closure 
        ( -- * Closures.
          (:->)(..)
        , ($:)

        -- * Array Closures.
        , PData(..)
        , ($:^), liftedApply

        -- * Closure Construction.
        , closure1,  closure2,  closure3,  closure4,  closure5,  closure6,  closure7,  closure8
        , closure1', closure2', closure3', closure4', closure5', closure6', closure7', closure8')
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Tuple2
import Data.Array.Parallel.PArray.PData.Tuple3
import Data.Array.Parallel.PArray.PData.Tuple4
import Data.Array.Parallel.PArray.PData.Tuple5
import Data.Array.Parallel.PArray.PData.Tuple6
import Data.Array.Parallel.PArray.PData.Tuple7

import Data.Array.Parallel.PArray.PRepr
import qualified Data.Typeable          as T
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

deriving instance T.Typeable (:->)

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
 = let  fv_1   _ xa                   = Clo  fv_2 fl_2 xa
        fl_1 _ _ xs                   = AClo fv_2 fl_2 xs

        fv_2   xa yb                  = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys                  = AClo fv_3 fl_3 (PTuple2 xs ys)

        fv_3 (xa, yb) zc              = Clo  fv_4 fl_4 (xa, yb, zc)
        fl_3 _ (PTuple2 xs ys) zs     = AClo fv_4 fl_4 (PTuple3 xs ys zs)

        fv_4 (xa, yb, zc) ad          = fv xa yb zc ad
        fl_4 n (PTuple3 xs ys zs) as  = fl n xs ys zs as

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure4 #-}


-- | Construct an arity-5 closure
--   from lifted and unlifted versions of a primitive function.
closure5
        :: forall a b c d e f. (PA a, PA b, PA c, PA d)
        => (a -> b -> c -> d -> e -> f)
        -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f)
        -> (a :-> b :-> c :-> d :-> e :-> f)
        
closure5 fv fl
 = let  fv_1   _ xa                     = Clo  fv_2 fl_2 xa
        fl_1 _ _ xs                     = AClo fv_2 fl_2 xs

        fv_2   xa yb                    = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys                    = AClo fv_3 fl_3 (PTuple2 xs ys)

        fv_3 (xa, yb) zc                = Clo  fv_4 fl_4 (xa, yb, zc)
        fl_3 _ (PTuple2 xs ys) zs       = AClo fv_4 fl_4 (PTuple3 xs ys zs)

        fv_4 (xa, yb, zc) ad            = Clo  fv_5 fl_5 (xa, yb, zc, ad)
        fl_4 _ (PTuple3 xs ys zs) as    = AClo fv_5 fl_5 (PTuple4 xs ys zs as)

        fv_5 (xa, yb, zc, ad) be        = fv xa yb zc ad be
        fl_5 n (PTuple4 xs ys zs as) bs = fl n xs ys zs as bs

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure5 #-}



-- | Construct an arity-6 closure
--   from lifted and unlifted versions of a primitive function.
closure6
        :: forall a b c d e f g. (PA a, PA b, PA c, PA d, PA e)
        => (a -> b -> c -> d -> e -> f -> g)
        -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g)
        
closure6 fv fl
 = let  fv_1   _ xa                     = Clo  fv_2 fl_2 xa
        fl_1 _ _ xs                     = AClo fv_2 fl_2 xs

        fv_2   xa yb                    = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys                    = AClo fv_3 fl_3 (PTuple2 xs ys)

        fv_3 (xa, yb) zc                = Clo  fv_4 fl_4 (xa, yb, zc)
        fl_3 _ (PTuple2 xs ys) zs       = AClo fv_4 fl_4 (PTuple3 xs ys zs)

        fv_4 (wa, xb, yc) zd            = Clo  fv_5 fl_5 (wa, xb, yc, zd)
        fl_4 _ (PTuple3 ws xs ys) zs    = AClo fv_5 fl_5 (PTuple4 ws xs ys zs)

        fv_5 (va, wb, xc, yd) ze          = Clo  fv_6 fl_6 (va, wb, xc, yd, ze)
        fl_5 _ (PTuple4 vs ws xs ys) zs   = AClo fv_6 fl_6 (PTuple5 vs ws xs ys zs)

        fv_6 (ua, vb, wc, xd, ye) zf      = fv ua vb wc xd ye zf 
        fl_6 n (PTuple5 us  vs ws xs ys) zs = fl n us vs ws xs ys zs 


   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure6 #-}


-- | Construct an arity-6 closure
--   from lifted and unlifted versions of a primitive function.
closure7
        :: forall a b c d e f g h. (PA a, PA b, PA c, PA d, PA e, PA f)
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g -> PData h)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h)
        
closure7 fv fl
 = let  fv_1   _ xa                     = Clo  fv_2 fl_2 xa
        fl_1 _ _ xs                     = AClo fv_2 fl_2 xs

        fv_2   xa yb                    = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys                    = AClo fv_3 fl_3 (PTuple2 xs ys)

        fv_3 (xa, yb) zc                = Clo  fv_4 fl_4 (xa, yb, zc)
        fl_3 _ (PTuple2 xs ys) zs       = AClo fv_4 fl_4 (PTuple3 xs ys zs)

        fv_4 (wa, xb, yc) zd            = Clo  fv_5 fl_5 (wa, xb, yc, zd)
        fl_4 _ (PTuple3 ws xs ys) zs    = AClo fv_5 fl_5 (PTuple4 ws xs ys zs)

        fv_5 (va, wb, xc, yd) ze          = Clo  fv_6 fl_6 (va, wb, xc, yd, ze)
        fl_5 _ (PTuple4 vs ws xs ys) zs   = AClo fv_6 fl_6 (PTuple5 vs ws xs ys zs)

        fv_6 (ua, vb, wc, xd, ye) zf        = Clo fv_7 fl_7 (ua, vb, wc, xd, ye, zf)
        fl_6 _ (PTuple5 us  vs ws xs ys) zs = AClo fv_7 fl_7 (PTuple6 us vs ws xs ys zs)

        fv_7 (ta, ub, vc, wd, xe, yf) zg      = fv ta ub vc wd xe yf zg
        fl_7 n (PTuple6 ts us vs ws xs ys) zs = fl n ts us vs ws xs ys zs 

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure7 #-}


-- | Construct an arity-6 closure
--   from lifted and unlifted versions of a primitive function.
closure8
        :: forall a b c d e f g h i. (PA a, PA b, PA c, PA d, PA e, PA f, PA g)
        => (a -> b -> c -> d -> e -> f -> g -> h -> i)
        -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g -> PData h -> PData i)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i)
        
closure8 fv fl
 = let  fv_1   _ xa                     = Clo  fv_2 fl_2 xa
        fl_1 _ _ xs                     = AClo fv_2 fl_2 xs

        fv_2   xa yb                    = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys                    = AClo fv_3 fl_3 (PTuple2 xs ys)

        fv_3 (xa, yb) zc                = Clo  fv_4 fl_4 (xa, yb, zc)
        fl_3 _ (PTuple2 xs ys) zs       = AClo fv_4 fl_4 (PTuple3 xs ys zs)

        fv_4 (wa, xb, yc) zd            = Clo  fv_5 fl_5 (wa, xb, yc, zd)
        fl_4 _ (PTuple3 ws xs ys) zs    = AClo fv_5 fl_5 (PTuple4 ws xs ys zs)

        fv_5 (va, wb, xc, yd) ze          = Clo  fv_6 fl_6 (va, wb, xc, yd, ze)
        fl_5 _ (PTuple4 vs ws xs ys) zs   = AClo fv_6 fl_6 (PTuple5 vs ws xs ys zs)

        fv_6 (ua, vb, wc, xd, ye) zf        = Clo fv_7 fl_7 (ua, vb, wc, xd, ye, zf)
        fl_6 _ (PTuple5 us  vs ws xs ys) zs = AClo fv_7 fl_7 (PTuple6 us vs ws xs ys zs)

        fv_7 (ta, ub, vc, wd, xe, yf) zg      = Clo fv_8 fl_8 (ta, ub, vc, wd, xe, yf, zg)
        fl_7 _ (PTuple6 ts us vs ws xs ys) zs = AClo fv_8 fl_8 (PTuple7 ts us vs ws xs ys zs) 

        fv_8 (sa, tb, uc, vd, we, xf, yg) zh      = fv sa tb uc vd we xf yg zh
        fl_8 n (PTuple7 ss ts us vs ws xs ys) zs = fl n ss ts us vs ws xs ys zs 

   in   Clo fv_1 fl_1 ()
{-# INLINE_CLOSURE closure8 #-}

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
        fl' (I# n#) !pdata1 !pdata2
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
        fl' (I# n#) !pdata1 !pdata2 !pdata3
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) of
                 PArray _ pdata' -> pdata'
   in   closure3 fv fl'
{-# INLINE_CLOSURE closure3' #-}


-- | Construct an arity-4 closure.
closure4'
        :: forall a b c d e. (PA a, PA b, PA c) 
        => (a -> b -> c -> d -> e)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e)
        -> (a :-> b :-> c :-> d :-> e) 

closure4' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) !pdata1 !pdata2 !pdata3 !pdata4
         = case fl (PArray n# pdata1) (PArray n# pdata2) 
                   (PArray n# pdata3) (PArray n# pdata4) of
                 PArray _ pdata' -> pdata'
   in   closure4 fv fl'
{-# INLINE_CLOSURE closure4' #-}


-- | Construct an arity-5 closure.
closure5'
        :: forall a b c d e f. (PA a, PA b, PA c, PA d) 
        => (a -> b -> c -> d -> e -> f)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f)
        -> (a :-> b :-> c :-> d :-> e :-> f) 

closure5' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) !pdata1 !pdata2 !pdata3 !pdata4 !pdata5
         = case fl (PArray n# pdata1) (PArray n# pdata2) 
                   (PArray n# pdata3) (PArray n# pdata4) 
                   (PArray n# pdata5) of
                 PArray _ pdata' -> pdata'
   in   closure5 fv fl'
{-# INLINE_CLOSURE closure5' #-}

-- | Construct an arity-6 closure.
closure6'
        :: forall a b c d e f g. (PA a, PA b, PA c, PA d, PA e, PA f) 
        => (a -> b -> c -> d -> e -> f -> g)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g) 

closure6' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) !pdata1 !pdata2 !pdata3 !pdata4 !pdata5 !pdata6
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) (PArray n# pdata4) (PArray n# pdata5) (PArray n# pdata6) of
                 PArray _ pdata' -> pdata'
   in   closure6 fv fl'
{-# INLINE_CLOSURE closure6' #-}



-- | Construct an arity-7 closure.
closure7'
        :: forall a b c d e f g h. (PA a, PA b, PA c, PA d, PA e, PA f, PA g) 
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g -> PArray h)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) 

closure7' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) !pdata1 !pdata2 !pdata3 !pdata4 !pdata5 !pdata6 !pdata7
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) (PArray n# pdata4) 
		           (PArray n# pdata5) (PArray n# pdata6) (PArray n# pdata7) of
                 PArray _ pdata' -> pdata'
   in   closure7 fv fl'
{-# INLINE_CLOSURE closure7' #-}

-- | Construct an arity-8 closure.
closure8'
        :: forall a b c d e f g h i. (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h) 
        => (a -> b -> c -> d -> e -> f -> g -> h -> i)
        -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> 
		    PArray g -> PArray h -> PArray i)
        -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i) 

closure8' fv fl 
 = let  {-# INLINE fl' #-}
        fl' (I# n#) !pdata1 !pdata2 !pdata3 !pdata4 !pdata5 !pdata6 !pdata7 !pdata8
         = case fl (PArray n# pdata1) (PArray n# pdata2) (PArray n# pdata3) (PArray n# pdata4) 
		           (PArray n# pdata5) (PArray n# pdata6) (PArray n# pdata7) (PArray n# pdata8) of
                 PArray _ pdata' -> pdata'
   in   closure8 fv fl'
{-# INLINE_CLOSURE closure8' #-}



-- PData instance for closures ------------------------------------------------
-- This needs to be here instead of in a module D.A.P.PArray.PData.Closure
-- to break an import loop.
-- We use INLINE_CLOSURE for these bindings instead of INLINE_PDATA because
-- most of the functions return closure constructors, and we want to eliminate
-- these early in the compilation.
--
instance PR (a :-> b) where

  {-# NOINLINE validPR #-}
  validPR (AClo _ _ env)
        = validPA env

  {-# NOINLINE nfPR #-}
  nfPR (AClo fv fl envs)
        = fv `seq` fl `seq` nfPA envs `seq` ()

  -- We can't test functions for equality.
  -- We can't test the environments either, because they're existentially quantified.
  -- Provided the closures have the same type, we just call them similar.
  {-# NOINLINE similarPR #-}
  similarPR _ _
        = True

  {-# NOINLINE coversPR #-}
  coversPR weak (AClo _ _ envs) ix
        = coversPA weak envs ix

  {-# NOINLINE pprpPR #-}
  pprpPR (Clo _ _ env)
        = vcat
        [ text "Clo"
        , pprpPA env ]

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (AClo _ _ envs)
        = vcat
        [ text "AClo"
        , pprpDataPA envs ]

  {-# NOINLINE typeRepPR #-}
  typeRepPR (Clo _ _ env)
        = typeRepPA env

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (AClo _ _ envs)
        = typeRepDataPA envs


  -- Constructors -------------------------------
  {-# INLINE_CLOSURE emptyPR #-}
  emptyPR
   = let  die    = error "emptydPR[:->]: no function in empty closure array"
      in  AClo die die (emptyPA :: PData ())

  {-# INLINE_CLOSURE replicatePR #-}
  replicatePR n (Clo fv fl envs)
        = AClo fv fl (replicatePA n envs)

  {-# INLINE_CLOSURE replicatesPR #-}
  replicatesPR lens (AClo fv fl envs)
        = AClo fv fl (replicatesPA lens envs)


  -- Projections --------------------------------
  {-# INLINE_CLOSURE lengthPR #-}
  lengthPR (AClo _ _ envs)
        = lengthPA envs

  {-# INLINE_CLOSURE indexPR #-}
  indexPR (AClo fv fl envs) ix
        = Clo fv fl  $ indexPA envs ix

  {-# INLINE_CLOSURE indexsPR #-}
  indexsPR (AClos fv fl envs) srcixs
        = AClo fv fl $ indexsPA envs srcixs

  {-# INLINE_CLOSURE extractPR #-}
  extractPR (AClo fv fl envs) start len
        = AClo fv fl $ extractPA envs start len

  {-# INLINE_CLOSURE extractssPR #-}
  extractssPR (AClos fv fl envs) ssegd
        = AClo fv fl $ extractssPA envs ssegd

  {-# INLINE_CLOSURE extractvsPR #-}
  extractvsPR (AClos fv fl envs) vsegd
        = AClo fv fl $ extractvsPA envs vsegd


  -- Pack and Combine ---------------------------
  {-# INLINE_CLOSURE packByTagPR #-}
  packByTagPR (AClo fv fl envs) tags tag
        = AClo fv fl $ packByTagPA envs tags tag


  -- Conversions --------------------------------
  {-# NOINLINE toVectorPR #-}
  toVectorPR (AClo fv fl envs)
        = V.map (Clo fv fl) $ toVectorPA envs


  -- PDatas -------------------------------------
  -- When constructing an empty array of closures, we don't know what 
  {-# INLINE_CLOSURE emptydPR #-}
  emptydPR 
   = let die    = error "emptydPR[:->]: no function in empty closure array"
     in  AClos  die die (emptydPA :: PDatas ())

  {-# INLINE_CLOSURE singletondPR #-}
  singletondPR (AClo fv fl env)
        = AClos fv fl $ singletondPA env
        
  {-# INLINE_CLOSURE lengthdPR #-}
  lengthdPR (AClos _ _ env)
        = lengthdPA env
        
  {-# INLINE_CLOSURE indexdPR #-}
  indexdPR (AClos fv fl envs) ix
        = AClo fv fl $ indexdPA envs ix

  {-# NOINLINE toVectordPR #-}
  toVectordPR (AClos fv fl envs)
        = V.map (AClo fv fl) $ toVectordPA envs


  -- Unsupported --------------------------------
  -- To support these operators we'd need to manage closure arrays containing
  -- multiple hetrogenous functions. But this is more work than we care for
  -- right now. Note that the problematic functions are all constructors, and
  -- we can't know that all the parameters contain the same function.
  appendPR      = dieHetroFunctions "appendPR"
  appendvsPR    = dieHetroFunctions "appendsPR"
  combine2PR    = dieHetroFunctions "combine2PR"
  fromVectorPR  = dieHetroFunctions "fromVectorPR"
  appenddPR     = dieHetroFunctions "appenddPR"
  fromVectordPR = dieHetroFunctions "fromVectordPR"


dieHetroFunctions :: String -> a
dieHetroFunctions name
 = error $ unlines
   [ "Data.Array.Parallel.Lifted.Closure." ++ name
   , "  Unsupported Array Operation"
   , "  It looks like you're trying to define an array containing multiple"
   , "  hetrogenous functions, or trying to select between multiple arrays"
   , "  of functions in vectorised code. Although we could support this by"
   , "  constructing a new function that selects between them depending on"
   , "  what the array index is, to make that anywhere near efficient is"
   , "  more work than we care to do right now, and we expect this use case"
   , "  to be uncommon. If you want this to work then contact the DPH team"
   , "  and ask what you can do to help." ]


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
