#ifdef HEAD
module  Array ( 
    module Ix,  -- export all of Ix 
    Array, array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, ixmap ) where

import Ix
#if STD_PRELUDE
import List( (\\) )

infixl 9  !, //
#else
import PreludeBuiltin
#endif
#endif /* HEAD */
#ifdef BODY

#if STD_PRELUDE
data Array a b = MkArray (a,a) (a -> b) deriving ()

array                 :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b ivs =
    if and [inRange b i | (i,_) <- ivs]
        then MkArray b
                     (\j -> case [v | (i,v) <- ivs, i == j] of
                            [v]   -> v
                            []    -> error "Array.!: \ 
                                           \undefined array element"
                            _     -> error "Array.!: \ 
                                           \multiply defined array element")
        else error "Array.array: out-of-range array association"

listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray _ f)     =  f

bounds                :: (Ix a) => Array a b -> (a,a)
bounds (MkArray b _)  =  b

indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds

elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

assocs                :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // us               =  array (bounds a)
                            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                             ++ us)

accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
                                   -> Array a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]
                                   -> Array a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])

ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
                                         -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

instance  (Ix a)         => Functor (Array a) where
    map fn (MkArray b f) =  MkArray b (fn . f) 

#else /* STD_PRELUDE */

data Ix ix => Array ix elt              = Array            (ix,ix) (PrimArray elt)
data Ix ix => ByteArray ix      	= ByteArray	   (ix,ix) PrimByteArray
data Ix ix => MutableArray     s ix elt = MutableArray     (ix,ix) (PrimMutableArray s elt)
data Ix ix => MutableByteArray s ix     = MutableByteArray (ix,ix) (PrimMutableByteArray s)

array                 :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array ixs@(ix_start, ix_end) ivs = runST (do
  { mut_arr <- primNewArray (rangeSize ixs) arrEleBottom
  ; mapM_ (\ (i,v) -> primWriteArray mut_arr (index ixs i) v) ivs 
  ; arr <- primUnsafeFreezeArray mut_arr
  ; return (Array ixs arr)
  }
  )
 where
  arrEleBottom = error "(Array.!): undefined array element"

listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)                   :: (Ix a) => Array a b -> a -> b
(Array bounds arr) ! i = primIndexArray arr (index bounds i)

bounds                :: (Ix a) => Array a b -> (a,a)
bounds (Array b _)    =  b

indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds

elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

assocs                :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // us               =  array (bounds a)
                            ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                             ++ us)

accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
                                   -> Array a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]
                                   -> Array a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])

ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
                                         -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

instance  (Ix a)         => Functor (Array a)


#endif /* STD_PRELUDE */

#ifdef PROVIDE_ARRAY
data PrimArray              a -- immutable arrays with Int indices
data PrimByteArray

data Ref                  s a -- mutable variables
data PrimMutableArray     s a -- mutable arrays with Int indices
data PrimMutableByteArray s

----------------------------------------------------------------
-- pointer equality tests:
----------------------------------------------------------------

instance Eq (Ref s a)                where (==) = primSameRef
instance Eq (PrimMutableArray s a)   where (==) = primSameMutableArray

instance Eq (PrimMutableByteArray s) where (==) = primSameMutableByteArray

instance (Ix ix) => Eq (MutableArray s ix elt) where
  MutableArray _ arr1 == MutableArray _ arr2 = arr1 == arr2

instance (Ix ix) => Eq (MutableByteArray s ix) where
  MutableByteArray _ arr1 == MutableByteArray _ arr2 = arr1 == arr2

#endif /* PROVIDE_ARRAYS */

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'             =  assocs a == assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <=  a'            =  assocs a <=  assocs a'

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
                    showString "array " .
                    shows (bounds a) . showChar ' ' .
                    shows (assocs a)                  )

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
           (\r -> [(array b as, u) | ("array",s) <- lex r,
                                     (b,t)       <- reads s,
                                     (as,u)      <- reads t   ])
#endif /* BODY */
