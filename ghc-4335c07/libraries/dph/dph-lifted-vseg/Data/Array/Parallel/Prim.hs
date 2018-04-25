{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | This is the API used by the vectoriser.
--   The vectoriser wants a slightly different interface to the one used 
--   natively by the library. This module performs the impedance matching.
module Data.Array.Parallel.Prim 
        -- Core types
        ( PArray, PData, PDatas, PRepr, PA(..), PR(..)

        -- Array Functions
        , emptyPD
        , replicatePD
        , packByTagPD
        , combine2PD

        -- Scalar primitives
        , Scalar(..)
        , scalar_map
        , scalar_zipWith
        , scalar_zipWith3
        , scalar_zipWith4
        , scalar_zipWith5
        , scalar_zipWith6
        , scalar_zipWith7
        , scalar_zipWith8

        -- Types used in the generic representation
        , Void, void, fromVoid, pvoid, pvoids#
        , punit
        , Wrap(..)
        , Sum2(..), Sum3(..)
        
        -- Closures, and closure functions
        , (:->)(..)
        , closure,              ($:)
        , liftedClosure,        liftedApply
        , closure1, closure2, closure3, closure4, closure5
        , closure6, closure7, closure8
        
        -- Selectors
        , Sel2
        , tagsSel2
        , pickSel2#
        , replicateSel2#
        , elementsSel2_0#
        , elementsSel2_1#

        , Sels2
        , lengthSels2#

        -- Scalar constructors
        , emptyPA_Int#,         emptyPA_Double#
        , replicatePA_Int#,     replicatePA_Double#
        , packByTagPA_Int#,     packByTagPA_Double#
        , combine2PA_Int#,      combine2PA_Double#

        -- Tuple constructors
        , tup2, tup3, tup4, tup5)
where
import Data.Array.Parallel.PArray.PData.Base   
        (PArray(..), PData, PDatas, PR(..))

import Data.Array.Parallel.PArray.PData.Void
        ( Void, void, pvoid, pvoids, fromVoid )

import Data.Array.Parallel.PArray.PData.Unit
        ( punit )

import Data.Array.Parallel.PArray.PData.Sum2
        ( Sels2, lengthSels2 )

import Data.Array.Parallel.PArray.PRepr 
        ( PRepr, PA(..)
        , emptyPA, replicatePA, packByTagPA, combine2PA)
        
import Data.Array.Parallel.PArray.Scalar
        ( Scalar(..))

import Data.Array.Parallel.PArray.Types
        ( Wrap(..)
        , Sum2(..), Sum3(..))
        
import Data.Array.Parallel.Lifted.Closure
        ( (:->)(..))

import Data.Array.Parallel.Prelude.Tuple
        ( tup2, tup3, tup4, tup5)

import Data.Array.Parallel.Base                         (Tag, intToTag)
import qualified Data.Array.Parallel.Unlifted           as U
import qualified Data.Array.Parallel.PArray.Scalar      as Scalar
import qualified Data.Array.Parallel.Lifted.Closure     as C
import GHC.Exts


-- Array functions ------------------------------------------------------------
emptyPD :: PA a => PData a
emptyPD = emptyPA
{-# INLINE_PA emptyPD #-}


replicatePD :: PA a => Int# -> a -> PData a
replicatePD i# x 
        = replicatePA (I# i#) x
{-# INLINE_PA replicatePD #-}

        
packByTagPD :: PA a => PData a -> Int# -> U.Array Tag -> Int# -> PData a
packByTagPD xs _ tags tag#
        = packByTagPA xs tags (I# tag#)
{-# INLINE_PA packByTagPD #-}


combine2PD :: PA a => Int# -> U.Sel2 -> PData a -> PData a -> PData a
combine2PD _ sel xs ys
        = combine2PA sel xs ys
{-# INLINE_PA combine2PD #-}


-- Generic Representation ----------------------------------------------------
pvoids# :: Int# -> PDatas Void
pvoids# n#       = pvoids (I# n#)
{-# INLINE_PA pvoids# #-}


-- Closures -------------------------------------------------------------------
-- The vectoriser wants versions of these functions that take unboxed
-- integers for the first argument of the lifted function.

-- | Construct a closure.
closure :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> e
        -> (a :-> b)

closure fv fl e 
 = Clo fv 
         (\(I# c) v x -> fl c v x)
         e
{-# INLINE_CLOSURE closure #-}


-- | Apply a closure.
($:) :: forall a b. (a :-> b) -> a -> b
($:)    = (C.$:)
{-# INLINE_CLOSURE ($:) #-}


-- | Construct a lifted closure.
liftedClosure
        :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> PData e
        -> PData (a :-> b)
{-# INLINE_CLOSURE liftedClosure #-}

liftedClosure fv fl es
 = C.AClo fv 
        (\(I# c) v x -> fl c v x)
        es
        

-- | Apply a lifted closure.
liftedApply :: Int# -> PData (a :-> b) -> PData a -> PData b
liftedApply n# arr xs
        = C.liftedApply (I# n#) arr xs
{-# INLINE_CLOSURE liftedApply #-}


closure1 :: forall a b
         .  (a -> b)
         -> (PArray a -> PArray b)
         -> (a :-> b)
closure1 fv fl
 = let  fl' :: Int -> PData a -> PData b
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata 
         = case fl (PArray c# pdata) of
                 PArray _ pdata' -> pdata'
                        
   in   C.closure1 fv fl'
{-# INLINE_CLOSURE closure1 #-}


closure2 :: forall a b c. PA a
         => (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
closure2 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2
         = case fl (PArray c# pdata1) (PArray c# pdata2) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure2 fv fl'
{-# INLINE_CLOSURE closure2 #-}


closure3 :: forall a b c d.  (PA a, PA b)
         => (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)
closure3 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure3 fv fl'
{-# INLINE_CLOSURE closure3 #-}

{-# INLINE_CLOSURE closure4 #-}
closure4 :: forall a b c d e.  (PA a, PA b, PA c)
         => (a -> b -> c -> d -> e)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e)
         -> (a :-> b :-> c :-> d :-> e)
closure4 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d -> PData e
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3 pdata4
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) (PArray c# pdata4) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure4 fv fl'



{-# INLINE_CLOSURE closure5 #-}
closure5 :: forall a b c d e f.  (PA a, PA b, PA c, PA d)
         => (a -> b -> c -> d -> e -> f)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f)
         -> (a :-> b :-> c :-> d :-> e :-> f)
closure5 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3 pdata4 pdata5
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) (PArray c# pdata4) (PArray c# pdata5) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure5 fv fl'

{-# INLINE_CLOSURE closure6 #-}
closure6 :: forall a b c d e f g.  (PA a, PA b, PA c, PA d, PA e)
         => (a -> b -> c -> d -> e -> f -> g)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g)
closure6 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3 pdata4 pdata5 pdata6
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) (PArray c# pdata4) (PArray c# pdata5) (PArray c# pdata6) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure6 fv fl'
   
{-# INLINE_CLOSURE closure7 #-}
closure7 :: forall a b c d e f g h.  (PA a, PA b, PA c, PA d, PA e, PA f, PA g)
         => (a -> b -> c -> d -> e -> f -> g -> h)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g -> PArray h)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h)
closure7 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g  -> PData h
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3 pdata4 pdata5 pdata6 pdata7
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) (PArray c# pdata4) (PArray c# pdata5) (PArray c# pdata6) (PArray c# pdata7) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure7 fv fl'


{-# INLINE_CLOSURE closure8 #-}
closure8 :: forall a b c d e f g h i.  (PA a, PA b, PA c, PA d, PA e, PA f, PA g, PA h)
         => (a -> b -> c -> d -> e -> f -> g -> h -> i)
         -> (PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g -> PArray h -> PArray i)
         -> (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> i)
closure8 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d -> PData e -> PData f -> PData g -> PData h  -> PData i
        {-# INLINE_CLOSURE fl' #-}
        fl' (I# c#) pdata1 pdata2 pdata3 pdata4 pdata5 pdata6 pdata7 pdata8 
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) (PArray c# pdata4) 
                   (PArray c# pdata5) (PArray c# pdata6) (PArray c# pdata7) (PArray c# pdata8) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure8 fv fl'   
   
   
-- Selector functions ---------------------------------------------------------
-- The vectoriser wants versions of these that take unboxed integers
-- for some arguments.
type Sel2       = U.Sel2


replicateSel2# :: Int# -> Int# -> Sel2
replicateSel2# n# tag#
  = U.mkSel2
         (U.replicate n (intToTag tag))
         (U.enumFromStepLen 0 1 n)
         (if tag == 0 then n else 0)
         (if tag == 0 then 0 else n)
         (U.mkSelRep2 (U.replicate n (intToTag tag)))
  where
    n   = I# n#
    tag = I# tag#
{-# INLINE_PA replicateSel2# #-}


pickSel2# :: Sel2 -> Int# -> U.Array Bool
pickSel2# sel tag#
        = U.pick (U.tagsSel2 sel) (intToTag (I# tag#))
{-# INLINE_PA pickSel2# #-}


tagsSel2 :: Sel2 -> U.Array Tag
tagsSel2 = U.tagsSel2
{-# INLINE_PA tagsSel2 #-}


elementsSel2_0# :: Sel2 -> Int#
elementsSel2_0# sel
        = case U.elementsSel2_0 sel of { I# n# -> n# }
{-# INLINE_PA elementsSel2_0# #-}


elementsSel2_1# :: Sel2 -> Int#
elementsSel2_1# sel
        = case U.elementsSel2_1 sel of { I# n# -> n# }
{-# INLINE_PA elementsSel2_1# #-}


lengthSels2# :: Sels2 -> Int#
lengthSels2# sels2
        = case lengthSels2 sels2 of { I# n# -> n# }
{-# INLINE_PA lengthSels2# #-}


-- Scalar functions -----------------------------------------------------------
scalar_map 
        :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

scalar_map      = Scalar.map
{-# INLINE scalar_map #-}


scalar_zipWith
        :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

scalar_zipWith  = Scalar.zipWith
{-# INLINE scalar_zipWith #-}


scalar_zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

scalar_zipWith3 = Scalar.zipWith3
{-# INLINE scalar_zipWith3 #-}

{-# INLINE scalar_zipWith4 #-}
scalar_zipWith4
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e)
        => (a -> b -> c -> d -> e) -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e

scalar_zipWith4 = Scalar.zipWith4


{-# INLINE scalar_zipWith5 #-}
scalar_zipWith5
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f)
        => (a -> b -> c -> d -> e -> f) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f

scalar_zipWith5 = Scalar.zipWith5

{-# INLINE scalar_zipWith6 #-}
scalar_zipWith6
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g)
        => (a -> b -> c -> d -> e -> f -> g) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g

scalar_zipWith6 = Scalar.zipWith6

{-# INLINE scalar_zipWith7 #-}
scalar_zipWith7
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h)
        => (a -> b -> c -> d -> e -> f -> g -> h) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f -> PArray g -> PArray h

scalar_zipWith7 = Scalar.zipWith7

{-# INLINE scalar_zipWith8 #-}
scalar_zipWith8
        :: (Scalar a, Scalar b, Scalar c, Scalar d, Scalar e, Scalar f, Scalar g, Scalar h, Scalar i)
        => (a -> b -> c -> d -> e -> f -> g -> h -> i) 
        -> PArray a -> PArray b -> PArray c -> PArray d -> PArray e -> PArray f     
        -> PArray g -> PArray h -> PArray i

scalar_zipWith8 = Scalar.zipWith8

-- Int functions --------------------------------------------------------------
type PArray_Int# = U.Array Int


replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)
{-# INLINE_PA replicatePA_Int# #-}


emptyPA_Int# :: PArray_Int#
emptyPA_Int# = U.empty
{-# INLINE_PA emptyPA_Int# #-}


{-# RULES

"replicatePA_Int#" forall n# i#.
  replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)

 #-}


packByTagPA_Int# :: a
packByTagPA_Int#    
 = error "Data.Array.Parallel.Prim: 'packByTagPA_Int#' not implemented"
{-# NOINLINE packByTagPA_Int# #-}


combine2'PA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2'PA_Int# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Int# #-}


combine2PA_Int#
        :: Int#
        -> PArray_Int# -> PArray_Int#
        -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2PA_Int# _ sel _ xs ys = combine2'PA_Int# sel xs ys
{-# INLINE_PA combine2PA_Int# #-}


-- Double functions -----------------------------------------------------------
type PArray_Double# = U.Array Double


replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = U.replicate (I# n#) (D# d#)
{-# INLINE_PA replicatePA_Double# #-}


emptyPA_Double# :: PArray_Double#
emptyPA_Double# = U.empty
{-# INLINE_PA emptyPA_Double# #-}


packByTagPA_Double# :: a
packByTagPA_Double# 
 = error "Data.Array.Parallel.Prim: 'packByTagPA_Double#' not implemented"
{-# NOINLINE packByTagPA_Double# #-}


combine2'PA_Double#
        :: PArray_Int#
        -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2'PA_Double# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Double# #-}


combine2PA_Double#
        :: Int#
        -> PArray_Int# -> PArray_Int#
        -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2PA_Double# _ sel _ xs ys = combine2'PA_Double# sel xs ys
{-# INLINE_PA combine2PA_Double# #-}
