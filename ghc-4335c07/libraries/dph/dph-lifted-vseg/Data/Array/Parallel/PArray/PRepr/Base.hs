{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Definition of the PRepr/PA family and class.
--   This module manages the conversion between the user level view of the 
--   element data, and our internal generic view.
module Data.Array.Parallel.PArray.PRepr.Base 
        ( PRepr
        , PA (..)
        , toNestedArrPRepr

        -- * House Keeping
        , validPA
        , nfPA
        , similarPA
        , coversPA
        , pprpPA
        , pprpDataPA
        , typeRepPA
        , typeRepDataPA
        , typeRepDatasPA

        -- * Constructors
        , emptyPA
        , replicatePA,  replicatesPA
        , appendPA,     appendsPA

        -- * Projections
        , lengthPA
        , indexPA,      indexsPA,     indexvsPA
        , bpermutePA
        , extractPA,    extractssPA,  extractvsPA

        -- * Pack and Combine
        , packByTagPA
        , combine2PA

        -- * Conversions 
        , fromVectorPA, toVectorPA

        -- * PDatas
        , emptydPA
        , singletondPA
        , lengthdPA
        , indexdPA
        , appenddPA
        , fromVectordPA, toVectordPA)
where
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.Base                 (Tag)
import Data.Vector                              (Vector)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T


-- PRepr / PA -----------------------------------------------------------------
-- | Family of Representable types. These are the types that we know how to
--   represent generically. `PRepr` takes an arbitrary type and produces the
--   generic type we use to  represent it.
--
--   Instances for simple types are defined by the library. 
--   For algebraic types, it's up to the vectoriser/client module to create
--   a suitable instance.
--
type family PRepr a


-- | A PA dictionary contains the functions that we use to convert a
--   representable type to and from its generic representation.
--
--   The conversions methods should all be O(1). 
class PR (PRepr a) => PA a where
  toPRepr       :: a                -> PRepr a
  fromPRepr     :: PRepr a          -> a

  toArrPRepr    :: PData a          -> PData (PRepr a)
  fromArrPRepr  :: PData (PRepr a)  -> PData a

  toArrPReprs   :: PDatas a         -> PDatas (PRepr a)
  fromArrPReprs :: PDatas (PRepr a) -> PDatas a


-- | Convert a nested array to its generic representation.
toNestedArrPRepr
        :: PA a 
        => PData (PArray a)
        -> PData (PArray (PRepr a))

toNestedArrPRepr (PNested vsegd pdatas segd flat)
        = PNested vsegd (toArrPReprs pdatas) segd (toArrPRepr flat)


-- PA Wrappers ----------------------------------------------------------------
--  These wrappers work on (PData a) arrays when we know the element type 'a'
--  is generically representable. We implement the array operators by converting
--  the PData to our generic representation type, and use the corresponding
--  method from the PR dictionary.
--
--  The wrappers are used in situations when we only have PA dictionary, 
--  instead of a PR dictionary. This happens in the PR (a :-> b) instance, 
--  as we need to work on a generically represented environment, and only
--  have an existential PA dictionary. We also use them in the PA functions
--  defined by D.A.P.PArray.
--
--  See the D.A.P.PArray.PData.Base for docs on what these functions do.
--  Each of the following functions has a corresponding method in the PR class.
--
{-# INLINE_PA validPA #-}
validPA         :: PA a => PData a -> Bool
validPA arr
 = validPR (toArrPRepr arr)


{-# INLINE_PA nfPA #-}
nfPA            :: PA a => PData a -> ()
nfPA arr
 = nfPR 
 $ toArrPRepr arr


{-# INLINE_PA similarPA #-}
similarPA       :: PA a => a -> a -> Bool
similarPA x y
 = similarPR (toPRepr x) (toPRepr y)


{-# INLINE_PA coversPA #-}
coversPA        :: PA a => Bool -> PData a -> Int -> Bool
coversPA weak pdata ix
 = coversPR weak (toArrPRepr pdata) ix


{-# INLINE_PA pprpPA #-}
pprpPA          :: PA a => a -> Doc
pprpPA x
 = pprpPR (toPRepr x)


{-# INLINE_PA pprpDataPA #-}
pprpDataPA      :: PA a => PData a -> Doc
pprpDataPA x
 = pprpDataPR (toArrPRepr x)


{-# INLINE_PA typeRepPA #-}
typeRepPA       :: PA a => a -> T.TypeRep
typeRepPA x
 = typeRepPR (toPRepr x)

{-# INLINE_PA typeRepDataPA #-}
typeRepDataPA    :: PA a => PData a -> T.TypeRep
typeRepDataPA x
 = typeRepDataPR (toArrPRepr x)

{-# INLINE_PA typeRepDatasPA #-}
typeRepDatasPA    :: PA a => PDatas a -> T.TypeRep
typeRepDatasPA x
 = typeRepDatasPR (toArrPReprs x)


-- Constructors ---------------------------------
{-# INLINE_PA emptyPA #-}
emptyPA         :: PA a => PData a
emptyPA 
  = fromArrPRepr emptyPR


{-# INLINE_PA replicatePA #-}
replicatePA     :: PA a => Int -> a -> PData a
replicatePA n x
 = fromArrPRepr
 $ replicatePR n $ toPRepr x


{-# INLINE_PA replicatesPA #-}
replicatesPA    :: PA a => U.Segd -> PData a -> PData a
replicatesPA segd xs
 = fromArrPRepr
 $ replicatesPR segd (toArrPRepr xs)


{-# INLINE_PA appendPA #-}
appendPA        :: PA a => PData a -> PData a -> PData a
appendPA xs ys
 = fromArrPRepr
 $ appendPR (toArrPRepr xs) (toArrPRepr ys)


{-# INLINE_PA appendsPA #-}
appendsPA       :: PA a => U.Segd -> U.VSegd -> PDatas a -> U.VSegd 
                        -> PDatas a -> PData a
appendsPA segdResult segd1 xs segd2 ys
 = fromArrPRepr
 $ appendvsPR segdResult segd1 (toArrPReprs xs) segd2 (toArrPReprs ys)


-- Projections ----------------------------------
{-# INLINE_PA lengthPA #-}
lengthPA        :: PA a => PData a -> Int
lengthPA xs
 = lengthPR (toArrPRepr xs)


{-# INLINE_PA indexPA #-}
indexPA         :: PA a => PData a    -> Int -> a
indexPA xs i
 = fromPRepr 
 $ indexPR (toArrPRepr xs) i


{-# INLINE_PA indexsPA #-}
indexsPA        :: PA a => PDatas a -> U.Array (Int, Int) -> PData a
indexsPA pdatas srcixs
 = fromArrPRepr
 $ indexsPR (toArrPReprs pdatas) srcixs


{-# INLINE_PA indexvsPA #-}
indexvsPA        :: PA a => PDatas a -> U.VSegd -> U.Array (Int, Int) -> PData a
indexvsPA pdatas vsegd srcixs
 = fromArrPRepr
 $ indexvsPR (toArrPReprs pdatas) vsegd srcixs


{-# INLINE_PDATA bpermutePA #-}
bpermutePA      :: PA a => PData a -> U.Array Int -> PData a
bpermutePA xs ixs
 = fromArrPRepr
 $ bpermutePR (toArrPRepr xs) ixs


{-# INLINE_PA extractPA #-}
extractPA       :: PA a => PData a -> Int -> Int -> PData a
extractPA xs start len
 = fromArrPRepr
 $ extractPR (toArrPRepr xs) start len


{-# INLINE_PA extractssPA #-}
extractssPA      :: PA a => PDatas a -> U.SSegd -> PData a
extractssPA xss segd
 = fromArrPRepr
 $ extractssPR (toArrPReprs xss) segd


{-# INLINE_PA extractvsPA #-}
extractvsPA      :: PA a => PDatas a -> U.VSegd -> PData a
extractvsPA xss segd
 = fromArrPRepr
 $ extractvsPR (toArrPReprs xss) segd


-- Pack and Combine -----------------------------
{-# INLINE_PA packByTagPA #-}
packByTagPA     :: PA a => PData a -> U.Array Tag -> Tag -> PData a
packByTagPA xs tags tag
 = fromArrPRepr
 $ packByTagPR (toArrPRepr xs) tags tag


{-# INLINE_PA combine2PA #-}
combine2PA      :: PA a => U.Sel2 -> PData a -> PData a -> PData a
combine2PA sel xs ys
 = fromArrPRepr
 $ combine2PR sel (toArrPRepr xs) (toArrPRepr ys)
 
 
-- Conversions ----------------------------------
{-# INLINE_PA fromVectorPA #-}
fromVectorPA    :: PA a => Vector a -> PData a
fromVectorPA vec
 = fromArrPRepr
 $ fromVectorPR (V.map toPRepr vec)


{-# INLINE_PA toVectorPA #-}
toVectorPA      :: PA a => PData a -> Vector a
toVectorPA pdata
 = V.map fromPRepr
 $ toVectorPR (toArrPRepr pdata)
 

{-# INLINE_PA emptydPA #-}
emptydPA        :: PA a => PDatas a
emptydPA 
 = fromArrPReprs
 $ emptydPR

 
{-# INLINE_PA singletondPA #-}
singletondPA    :: PA a => PData a -> PDatas a
singletondPA pdata
 = fromArrPReprs
 $ singletondPR (toArrPRepr pdata)


{-# INLINE_PA lengthdPA #-}
lengthdPA       :: PA a => PDatas a -> Int
lengthdPA pdatas
 = lengthdPR (toArrPReprs pdatas)


{-# INLINE_PA indexdPA #-}
indexdPA        :: PA a => PDatas a -> Int -> PData a
indexdPA pdatas ix
 = fromArrPRepr
 $ indexdPR (toArrPReprs pdatas) ix
 
 
{-# INLINE_PA appenddPA #-}
appenddPA       :: PA a => PDatas a -> PDatas a -> PDatas a
appenddPA xs ys
 = fromArrPReprs
 $ appenddPR (toArrPReprs xs) (toArrPReprs ys)


{-# INLINE_PA fromVectordPA #-}
fromVectordPA   :: PA a => V.Vector (PData a) -> PDatas a
fromVectordPA vec
 = fromArrPReprs
 $ fromVectordPR (V.map toArrPRepr vec)


{-# INLINE_PA toVectordPA #-}
toVectordPA     :: PA a => PDatas a -> V.Vector (PData a)
toVectordPA pdatas
 = V.map fromArrPRepr 
 $ toVectordPR (toArrPReprs pdatas)

