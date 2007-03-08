{-# OPTIONS_GHC -fparr -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PArr
-- Copyright   :  (c) 2001-2002 Manuel M T Chakravarty & Gabriele Keller
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Manuel M. T. Chakravarty <chak@cse.unsw.edu.au>
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
--  Basic implementation of Parallel Arrays.
--
--  This module has two functions: (1) It defines the interface to the
--  parallel array extension of the Prelude and (2) it provides a vanilla
--  implementation of parallel arrays that does not require to flatten the
--  array code.  The implementation is not very optimised.
--
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98 plus unboxed values and parallel arrays
--
--  The semantic difference between standard Haskell arrays (aka "lazy
--  arrays") and parallel arrays (aka "strict arrays") is that the evaluation
--  of two different elements of a lazy array is independent, whereas in a
--  strict array either non or all elements are evaluated.  In other words,
--  when a parallel array is evaluated to WHNF, all its elements will be
--  evaluated to WHNF.  The name parallel array indicates that all array
--  elements may, in general, be evaluated to WHNF in parallel without any
--  need to resort to speculative evaluation.  This parallel evaluation
--  semantics is also beneficial in the sequential case, as it facilitates
--  loop-based array processing as known from classic array-based languages,
--  such as Fortran.
--
--  The interface of this module is essentially a variant of the list
--  component of the Prelude, but also includes some functions (such as
--  permutations) that are not provided for lists.  The following list
--  operations are not supported on parallel arrays, as they would require the
--  availability of infinite parallel arrays: `iterate', `repeat', and `cycle'.
--
--  The current implementation is quite simple and entirely based on boxed
--  arrays.  One disadvantage of boxed arrays is that they require to
--  immediately initialise all newly allocated arrays with an error thunk to
--  keep the garbage collector happy, even if it is guaranteed that the array
--  is fully initialised with different values before passing over the
--  user-visible interface boundary.  Currently, no effort is made to use
--  raw memory copy operations to speed things up.
--
--- TODO ----------------------------------------------------------------------
--
--  * We probably want a standard library `PArray' in addition to the prelude
--    extension in the same way as the standard library `List' complements the
--    list functions from the prelude.
--
--  * Currently, functions that emphasis the constructor-based definition of
--    lists (such as, head, last, tail, and init) are not supported.  
--
--    Is it worthwhile to support the string processing functions lines,
--    words, unlines, and unwords?  (Currently, they are not implemented.)
--
--    It can, however, be argued that it would be worthwhile to include them
--    for completeness' sake; maybe only in the standard library `PArray'.
--
--  * Prescans are often more useful for array programming than scans.  Shall
--    we include them into the Prelude or the library?
--
--  * Due to the use of the iterator `loop', we could define some fusion rules
--    in this module.
--
--  * We might want to add bounds checks that can be deactivated.
--

module GHC.PArr (
  -- [::],		-- Built-in syntax

  mapP,			-- :: (a -> b) -> [:a:] -> [:b:]
  (+:+),		-- :: [:a:] -> [:a:] -> [:a:]
  filterP,		-- :: (a -> Bool) -> [:a:] -> [:a:]
  concatP,		-- :: [:[:a:]:] -> [:a:]
  concatMapP,		-- :: (a -> [:b:]) -> [:a:] -> [:b:]
--  head, last, tail, init,   -- it's not wise to use them on arrays
  nullP,	        -- :: [:a:] -> Bool
  lengthP,		-- :: [:a:] -> Int
  (!:),			-- :: [:a:] -> Int -> a
  foldlP,		-- :: (a -> b -> a) -> a -> [:b:] -> a
  foldl1P,		-- :: (a -> a -> a) ->      [:a:] -> a
  scanlP,		-- :: (a -> b -> a) -> a -> [:b:] -> [:a:]
  scanl1P,		-- :: (a -> a -> a) ->      [:a:] -> [:a:]
  foldrP,		-- :: (a -> b -> b) -> b -> [:a:] -> b
  foldr1P,		-- :: (a -> a -> a) ->      [:a:] -> a
  scanrP,		-- :: (a -> b -> b) -> b -> [:a:] -> [:b:]
  scanr1P,		-- :: (a -> a -> a) ->      [:a:] -> [:a:]
--  iterate, repeat,	      -- parallel arrays must be finite
  replicateP,		-- :: Int -> a -> [:a:]
--  cycle,		      -- parallel arrays must be finite
  takeP,		-- :: Int -> [:a:] -> [:a:]
  dropP,		-- :: Int -> [:a:] -> [:a:]
  splitAtP,		-- :: Int -> [:a:] -> ([:a:],[:a:])
  takeWhileP,		-- :: (a -> Bool) -> [:a:] -> [:a:]
  dropWhileP,		-- :: (a -> Bool) -> [:a:] -> [:a:]
  spanP,		-- :: (a -> Bool) -> [:a:] -> ([:a:], [:a:])
  breakP,		-- :: (a -> Bool) -> [:a:] -> ([:a:], [:a:])
--  lines, words, unlines, unwords,  -- is string processing really needed
  reverseP,	        -- :: [:a:] -> [:a:]
  andP,			-- :: [:Bool:] -> Bool
  orP, 			-- :: [:Bool:] -> Bool
  anyP,			-- :: (a -> Bool) -> [:a:] -> Bool
  allP,			-- :: (a -> Bool) -> [:a:] -> Bool
  elemP,		-- :: (Eq a) => a -> [:a:] -> Bool
  notElemP,		-- :: (Eq a) => a -> [:a:] -> Bool
  lookupP,		-- :: (Eq a) => a -> [:(a, b):] -> Maybe b
  sumP,			-- :: (Num a) => [:a:] -> a
  productP, 		-- :: (Num a) => [:a:] -> a
  maximumP,		-- :: (Ord a) => [:a:] -> a
  minimumP,		-- :: (Ord a) => [:a:] -> a
  zipP,			-- :: [:a:] -> [:b:]          -> [:(a, b)   :]
  zip3P,		-- :: [:a:] -> [:b:] -> [:c:] -> [:(a, b, c):]
  zipWithP,		-- :: (a -> b -> c)      -> [:a:] -> [:b:] -> [:c:]
  zipWith3P,		-- :: (a -> b -> c -> d) -> [:a:]->[:b:]->[:c:]->[:d:]
  unzipP,		-- :: [:(a, b)   :] -> ([:a:], [:b:])
  unzip3P,		-- :: [:(a, b, c):] -> ([:a:], [:b:], [:c:])

  -- overloaded functions
  --
  enumFromToP,		-- :: Enum a => a -> a      -> [:a:]
  enumFromThenToP,	-- :: Enum a => a -> a -> a -> [:a:]

  -- the following functions are not available on lists
  --
  toP,			-- :: [a] -> [:a:]
  fromP,		-- :: [:a:] -> [a]
  sliceP,		-- :: Int -> Int -> [:e:] -> [:e:]
  foldP,		-- :: (e -> e -> e) -> e -> [:e:] -> e
  fold1P,		-- :: (e -> e -> e) ->      [:e:] -> e
  permuteP,		-- :: [:Int:] -> [:e:] ->          [:e:]
  bpermuteP,		-- :: [:Int:] -> [:e:] ->          [:e:]
  dpermuteP,		-- :: [:Int:] -> [:e:] -> [:e:] -> [:e:]
  crossP,		-- :: [:a:] -> [:b:] -> [:(a, b):]
  crossMapP,		-- :: [:a:] -> (a -> [:b:]) -> [:(a, b):]
  indexOfP		-- :: (a -> Bool) -> [:a:] -> [:Int:]
) where

#ifndef __HADDOCK__

import Prelude

import GHC.ST   ( ST(..), STRep, runST )
import GHC.Exts	( Int#, Array#, Int(I#), MutableArray#, newArray#,
		  unsafeFreezeArray#, indexArray#, writeArray#, (<#), (>=#) )

infixl 9  !:
infixr 5  +:+
infix  4  `elemP`, `notElemP`


-- representation of parallel arrays
-- ---------------------------------

-- this rather straight forward implementation maps parallel arrays to the
-- internal representation used for standard Haskell arrays in GHC's Prelude
-- (EXPORTED ABSTRACTLY)
--
-- * This definition *must* be kept in sync with `TysWiredIn.parrTyCon'!
--
data [::] e = PArr Int# (Array# e)


-- exported operations on parallel arrays
-- --------------------------------------

-- operations corresponding to list operations
--

mapP   :: (a -> b) -> [:a:] -> [:b:]
mapP f  = fst . loop (mapEFL f) noAL

(+:+)     :: [:a:] -> [:a:] -> [:a:]
a1 +:+ a2  = fst $ loop (mapEFL sel) noAL (enumFromToP 0 (len1 + len2 - 1))
		       -- we can't use the [:x..y:] form here for tedious
		       -- reasons to do with the typechecker and the fact that
		       -- `enumFromToP' is defined in the same module
	     where
	       len1 = lengthP a1
	       len2 = lengthP a2
	       --
	       sel i | i < len1  = a1!:i
		     | otherwise = a2!:(i - len1)

filterP   :: (a -> Bool) -> [:a:] -> [:a:]
filterP p  = fst . loop (filterEFL p) noAL

concatP     :: [:[:a:]:] -> [:a:]
concatP xss  = foldlP (+:+) [::] xss

concatMapP   :: (a -> [:b:]) -> [:a:] -> [:b:]
concatMapP f  = concatP . mapP f

--  head, last, tail, init,   -- it's not wise to use them on arrays

nullP      :: [:a:] -> Bool
nullP [::]  = True
nullP _     = False

lengthP             :: [:a:] -> Int
lengthP (PArr n# _)  = I# n#

(!:) :: [:a:] -> Int -> a
(!:)  = indexPArr

foldlP     :: (a -> b -> a) -> a -> [:b:] -> a
foldlP f z  = snd . loop (foldEFL (flip f)) z

foldl1P        :: (a -> a -> a) -> [:a:] -> a
foldl1P f [::]  = error "Prelude.foldl1P: empty array"
foldl1P f a     = snd $ loopFromTo 1 (lengthP a - 1) (foldEFL f) (a!:0) a

scanlP     :: (a -> b -> a) -> a -> [:b:] -> [:a:]
scanlP f z  = fst . loop (scanEFL (flip f)) z

scanl1P        :: (a -> a -> a) -> [:a:] -> [:a:]
scanl1P f [::]  = error "Prelude.scanl1P: empty array"
scanl1P f a     = fst $ loopFromTo 1 (lengthP a - 1) (scanEFL f) (a!:0) a

foldrP :: (a -> b -> b) -> b -> [:a:] -> b
foldrP  = error "Prelude.foldrP: not implemented yet" -- FIXME

foldr1P :: (a -> a -> a) -> [:a:] -> a
foldr1P  = error "Prelude.foldr1P: not implemented yet" -- FIXME

scanrP :: (a -> b -> b) -> b -> [:a:] -> [:b:]
scanrP  = error "Prelude.scanrP: not implemented yet" -- FIXME

scanr1P :: (a -> a -> a) -> [:a:] -> [:a:]
scanr1P  = error "Prelude.scanr1P: not implemented yet" -- FIXME

--  iterate, repeat	      -- parallel arrays must be finite

replicateP             :: Int -> a -> [:a:]
{-# INLINE replicateP #-}
replicateP n e  = runST (do
  marr# <- newArray n e
  mkPArr n marr#)

--  cycle		      -- parallel arrays must be finite

takeP   :: Int -> [:a:] -> [:a:]
takeP n  = sliceP 0 (n - 1)

dropP     :: Int -> [:a:] -> [:a:]
dropP n a  = sliceP n (lengthP a - 1) a

splitAtP      :: Int -> [:a:] -> ([:a:],[:a:])
splitAtP n xs  = (takeP n xs, dropP n xs)

takeWhileP :: (a -> Bool) -> [:a:] -> [:a:]
takeWhileP  = error "Prelude.takeWhileP: not implemented yet" -- FIXME

dropWhileP :: (a -> Bool) -> [:a:] -> [:a:]
dropWhileP  = error "Prelude.dropWhileP: not implemented yet" -- FIXME

spanP :: (a -> Bool) -> [:a:] -> ([:a:], [:a:])
spanP  = error "Prelude.spanP: not implemented yet" -- FIXME

breakP   :: (a -> Bool) -> [:a:] -> ([:a:], [:a:])
breakP p  = spanP (not . p)

--  lines, words, unlines, unwords,  -- is string processing really needed

reverseP   :: [:a:] -> [:a:]
reverseP a  = permuteP (enumFromThenToP (len - 1) (len - 2) 0) a
		       -- we can't use the [:x, y..z:] form here for tedious
		       -- reasons to do with the typechecker and the fact that
		       -- `enumFromThenToP' is defined in the same module
	      where
	        len = lengthP a

andP :: [:Bool:] -> Bool
andP  = foldP (&&) True

orP :: [:Bool:] -> Bool
orP  = foldP (||) True

anyP   :: (a -> Bool) -> [:a:] -> Bool
anyP p  = orP . mapP p

allP :: (a -> Bool) -> [:a:] -> Bool
allP p  = andP . mapP p

elemP   :: (Eq a) => a -> [:a:] -> Bool
elemP x  = anyP (== x)

notElemP   :: (Eq a) => a -> [:a:] -> Bool
notElemP x  = allP (/= x)

lookupP :: (Eq a) => a -> [:(a, b):] -> Maybe b
lookupP  = error "Prelude.lookupP: not implemented yet" -- FIXME

sumP :: (Num a) => [:a:] -> a
sumP  = foldP (+) 0

productP :: (Num a) => [:a:] -> a
productP  = foldP (*) 1

maximumP      :: (Ord a) => [:a:] -> a
maximumP [::]  = error "Prelude.maximumP: empty parallel array"
maximumP xs    = fold1P max xs

minimumP :: (Ord a) => [:a:] -> a
minimumP [::]  = error "Prelude.minimumP: empty parallel array"
minimumP xs    = fold1P min xs

zipP :: [:a:] -> [:b:] -> [:(a, b):]
zipP  = zipWithP (,)

zip3P :: [:a:] -> [:b:] -> [:c:] -> [:(a, b, c):]
zip3P  = zipWith3P (,,)

zipWithP         :: (a -> b -> c) -> [:a:] -> [:b:] -> [:c:]
zipWithP f a1 a2  = let 
		      len1 = lengthP a1
		      len2 = lengthP a2
		      len  = len1 `min` len2
		    in
		    fst $ loopFromTo 0 (len - 1) combine 0 a1
		    where
		      combine e1 i = (Just $ f e1 (a2!:i), i + 1)

zipWith3P :: (a -> b -> c -> d) -> [:a:]->[:b:]->[:c:]->[:d:]
zipWith3P f a1 a2 a3 = let 
			len1 = lengthP a1
			len2 = lengthP a2
			len3 = lengthP a3
			len  = len1 `min` len2 `min` len3
		      in
		      fst $ loopFromTo 0 (len - 1) combine 0 a1
		      where
			combine e1 i = (Just $ f e1 (a2!:i) (a3!:i), i + 1)

unzipP   :: [:(a, b):] -> ([:a:], [:b:])
unzipP a  = (fst $ loop (mapEFL fst) noAL a, fst $ loop (mapEFL snd) noAL a)
-- FIXME: these two functions should be optimised using a tupled custom loop
unzip3P   :: [:(a, b, c):] -> ([:a:], [:b:], [:c:])
unzip3P a  = (fst $ loop (mapEFL fst3) noAL a, 
	      fst $ loop (mapEFL snd3) noAL a,
	      fst $ loop (mapEFL trd3) noAL a)
	     where
	       fst3 (a, _, _) = a
	       snd3 (_, b, _) = b
	       trd3 (_, _, c) = c

-- instances
--

instance Eq a => Eq [:a:] where
  a1 == a2 | lengthP a1 == lengthP a2 = andP (zipWithP (==) a1 a2)
	   | otherwise		      = False

instance Ord a => Ord [:a:] where
  compare a1 a2 = case foldlP combineOrdering EQ (zipWithP compare a1 a2) of
		    EQ | lengthP a1 == lengthP a2 -> EQ
		       | lengthP a1 <  lengthP a2 -> LT
		       | otherwise		  -> GT
		  where
		    combineOrdering EQ    EQ    = EQ
		    combineOrdering EQ    other = other
		    combineOrdering other _     = other

instance Functor [::] where
  fmap = mapP

instance Monad [::] where
  m >>= k  = foldrP ((+:+) . k      ) [::] m
  m >>  k  = foldrP ((+:+) . const k) [::] m
  return x = [:x:]
  fail _   = [::]

instance Show a => Show [:a:]  where
  showsPrec _  = showPArr . fromP
    where
      showPArr []     s = "[::]" ++ s
      showPArr (x:xs) s = "[:" ++ shows x (showPArr' xs s)

      showPArr' []     s = ":]" ++ s
      showPArr' (y:ys) s = ',' : shows y (showPArr' ys s)

instance Read a => Read [:a:]  where
  readsPrec _ a = [(toP v, rest) | (v, rest) <- readPArr a]
    where
      readPArr = readParen False (\r -> do
					  ("[:",s) <- lex r
					  readPArr1 s)
      readPArr1 s = 
	(do { (":]", t) <- lex s; return ([], t) }) ++
	(do { (x, t) <- reads s; (xs, u) <- readPArr2 t; return (x:xs, u) })

      readPArr2 s = 
	(do { (":]", t) <- lex s; return ([], t) }) ++
	(do { (",", t) <- lex s; (x, u) <- reads t; (xs, v) <- readPArr2 u; 
	      return (x:xs, v) })

-- overloaded functions
-- 

-- Ideally, we would like `enumFromToP' and `enumFromThenToP' to be members of
-- `Enum'.  On the other hand, we really do not want to change `Enum'.  Thus,
-- for the moment, we hope that the compiler is sufficiently clever to
-- properly fuse the following definitions.

enumFromToP	:: Enum a => a -> a -> [:a:]
enumFromToP x y  = mapP toEnum (eftInt (fromEnum x) (fromEnum y))
  where
    eftInt x y = scanlP (+) x $ replicateP (y - x + 1) 1

enumFromThenToP	      :: Enum a => a -> a -> a -> [:a:]
enumFromThenToP x y z  = 
  mapP toEnum (efttInt (fromEnum x) (fromEnum y) (fromEnum z))
  where
    efttInt x y z = scanlP (+) x $ 
		      replicateP (abs (z - x) `div` abs delta + 1) delta
      where
       delta = y - x

-- the following functions are not available on lists
--

-- create an array from a list (EXPORTED)
--
toP   :: [a] -> [:a:]
toP l  = fst $ loop store l (replicateP (length l) ())
	 where
	   store _ (x:xs) = (Just x, xs)

-- convert an array to a list (EXPORTED)
--
fromP   :: [:a:] -> [a]
fromP a  = [a!:i | i <- [0..lengthP a - 1]]

-- cut a subarray out of an array (EXPORTED)
--
sliceP :: Int -> Int -> [:e:] -> [:e:]
sliceP from to a = 
  fst $ loopFromTo (0 `max` from) (to `min` (lengthP a - 1)) (mapEFL id) noAL a

-- parallel folding (EXPORTED)
--
-- * the first argument must be associative; otherwise, the result is undefined
--
foldP :: (e -> e -> e) -> e -> [:e:] -> e
foldP  = foldlP

-- parallel folding without explicit neutral (EXPORTED)
--
-- * the first argument must be associative; otherwise, the result is undefined
--
fold1P :: (e -> e -> e) -> [:e:] -> e
fold1P  = foldl1P

-- permute an array according to the permutation vector in the first argument
-- (EXPORTED)
--
permuteP       :: [:Int:] -> [:e:] -> [:e:]
permuteP is es 
  | isLen /= esLen = error "GHC.PArr: arguments must be of the same length"
  | otherwise      = runST (do
		       marr <- newArray isLen noElem
		       permute marr is es
		       mkPArr isLen marr)
  where
    noElem = error "GHC.PArr.permuteP: I do not exist!"
	     -- unlike standard Haskell arrays, this value represents an
	     -- internal error
    isLen = lengthP is
    esLen = lengthP es

-- permute an array according to the back-permutation vector in the first
-- argument (EXPORTED)
--
-- * the permutation vector must represent a surjective function; otherwise,
--   the result is undefined
--
bpermuteP       :: [:Int:] -> [:e:] -> [:e:]
bpermuteP is es  = fst $ loop (mapEFL (es!:)) noAL is

-- permute an array according to the permutation vector in the first
-- argument, which need not be surjective (EXPORTED)
--
-- * any elements in the result that are not covered by the permutation
--   vector assume the value of the corresponding position of the third
--   argument 
--
dpermuteP :: [:Int:] -> [:e:] -> [:e:] -> [:e:]
dpermuteP is es dft
  | isLen /= esLen = error "GHC.PArr: arguments must be of the same length"
  | otherwise      = runST (do
		       marr <- newArray dftLen noElem
		       trans 0 (isLen - 1) marr dft copyOne noAL
		       permute marr is es
		       mkPArr dftLen marr)
  where
    noElem = error "GHC.PArr.permuteP: I do not exist!"
	     -- unlike standard Haskell arrays, this value represents an
	     -- internal error
    isLen  = lengthP is
    esLen  = lengthP es
    dftLen = lengthP dft

    copyOne e _ = (Just e, noAL)

-- computes the cross combination of two arrays (EXPORTED)
--
crossP       :: [:a:] -> [:b:] -> [:(a, b):]
crossP a1 a2  = fst $ loop combine (0, 0) $ replicateP len ()
		where
		  len1 = lengthP a1
		  len2 = lengthP a2
		  len  = len1 * len2
		  --
		  combine _ (i, j) = (Just $ (a1!:i, a2!:j), next)
				     where
				       next | (i + 1) == len1 = (0    , j + 1)
					    | otherwise       = (i + 1, j)

{- An alternative implementation
   * The one above is certainly better for flattened code, but here where we
     are handling boxed arrays, the trade off is less clear.  However, I
     think, the above one is still better.

crossP a1 a2  = let
		  len1 = lengthP a1
		  len2 = lengthP a2
		  x1   = concatP $ mapP (replicateP len2) a1
		  x2   = concatP $ replicateP len1 a2
		in
		zipP x1 x2
 -}

-- |Compute a cross of an array and the arrays produced by the given function
-- for the elements of the first array.
--
crossMapP :: [:a:] -> (a -> [:b:]) -> [:(a, b):]
crossMapP a f = let
		  bs   = mapP f a
		  segd = mapP lengthP bs
		  as   = zipWithP replicateP segd a
	        in
		zipP (concatP as) (concatP bs)

{- The following may seem more straight forward, but the above is very cheap
   with segmented arrays, as `mapP lengthP', `zipP', and `concatP' are
   constant time, and `map f' uses the lifted version of `f'.

crossMapP a f = concatP $ mapP (\x -> mapP ((,) x) (f x)) a

 -}

-- computes an index array for all elements of the second argument for which
-- the predicate yields `True' (EXPORTED)
--
indexOfP     :: (a -> Bool) -> [:a:] -> [:Int:]
indexOfP p a  = fst $ loop calcIdx 0 a
		where
		  calcIdx e idx | p e       = (Just idx, idx + 1)
				| otherwise = (Nothing , idx    )


-- auxiliary functions
-- -------------------

-- internally used mutable boxed arrays
--
data MPArr s e = MPArr Int# (MutableArray# s e)

-- allocate a new mutable array that is pre-initialised with a given value
--
newArray             :: Int -> e -> ST s (MPArr s e)
{-# INLINE newArray #-}
newArray (I# n#) e  = ST $ \s1# ->
  case newArray# n# e s1# of { (# s2#, marr# #) ->
  (# s2#, MPArr n# marr# #)}

-- convert a mutable array into the external parallel array representation
--
mkPArr                           :: Int -> MPArr s e -> ST s [:e:]
{-# INLINE mkPArr #-}
mkPArr (I# n#) (MPArr _ marr#)  = ST $ \s1# ->
  case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
  (# s2#, PArr n# arr# #) }

-- general array iterator
--
-- * corresponds to `loopA' from ``Functional Array Fusion'', Chakravarty &
--   Keller, ICFP 2001
--
loop :: (e -> acc -> (Maybe e', acc))    -- mapping & folding, once per element
     -> acc				 -- initial acc value
     -> [:e:]				 -- input array
     -> ([:e':], acc)
{-# INLINE loop #-}
loop mf acc arr = loopFromTo 0 (lengthP arr - 1) mf acc arr

-- general array iterator with bounds
--
loopFromTo :: Int			 -- from index
	   -> Int			 -- to index
	   -> (e -> acc -> (Maybe e', acc))
	   -> acc
	   -> [:e:]
	   -> ([:e':], acc)
{-# INLINE loopFromTo #-}
loopFromTo from to mf start arr = runST (do
  marr      <- newArray (to - from + 1) noElem
  (n', acc) <- trans from to marr arr mf start
  arr       <- mkPArr n' marr
  return (arr, acc))
  where
    noElem = error "GHC.PArr.loopFromTo: I do not exist!"
	     -- unlike standard Haskell arrays, this value represents an
	     -- internal error

-- actual loop body of `loop'
--
-- * for this to be really efficient, it has to be translated with the
--   constructor specialisation phase "SpecConstr" switched on; as of GHC 5.03
--   this requires an optimisation level of at least -O2
--
trans :: Int				-- index of first elem to process
      -> Int				-- index of last elem to process
      -> MPArr s e'			-- destination array
      -> [:e:]				-- source array
      -> (e -> acc -> (Maybe e', acc))	-- mutator
      -> acc				-- initial accumulator
      -> ST s (Int, acc)		-- final destination length/final acc
{-# INLINE trans #-}
trans from to marr arr mf start = trans' from 0 start
  where
    trans' arrOff marrOff acc 
      | arrOff > to = return (marrOff, acc)
      | otherwise   = do
		        let (oe', acc') = mf (arr `indexPArr` arrOff) acc
			marrOff' <- case oe' of
				      Nothing -> return marrOff 
				      Just e' -> do
					writeMPArr marr marrOff e'
					return $ marrOff + 1
                        trans' (arrOff + 1) marrOff' acc'

-- Permute the given elements into the mutable array.
--
permute :: MPArr s e -> [:Int:] -> [:e:] -> ST s ()
permute marr is es = perm 0
  where
    perm i
      | i == n = return ()
      | otherwise  = writeMPArr marr (is!:i) (es!:i) >> perm (i + 1)
      where
        n = lengthP is


-- common patterns for using `loop'
--

-- initial value for the accumulator when the accumulator is not needed
--
noAL :: ()
noAL  = ()

-- `loop' mutator maps a function over array elements
--
mapEFL   :: (e -> e') -> (e -> () -> (Maybe e', ()))
{-# INLINE mapEFL #-}
mapEFL f  = \e a -> (Just $ f e, ())

-- `loop' mutator that filter elements according to a predicate
--
filterEFL   :: (e -> Bool) -> (e -> () -> (Maybe e, ()))
{-# INLINE filterEFL #-}
filterEFL p  = \e a -> if p e then (Just e, ()) else (Nothing, ())

-- `loop' mutator for array folding
--
foldEFL   :: (e -> acc -> acc) -> (e -> acc -> (Maybe (), acc))
{-# INLINE foldEFL #-}
foldEFL f  = \e a -> (Nothing, f e a)

-- `loop' mutator for array scanning
--
scanEFL   :: (e -> acc -> acc) -> (e -> acc -> (Maybe acc, acc))
{-# INLINE scanEFL #-}
scanEFL f  = \e a -> (Just a, f e a)

-- elementary array operations
--

-- unlifted array indexing 
--
indexPArr                       :: [:e:] -> Int -> e
{-# INLINE indexPArr #-}
indexPArr (PArr n# arr#) (I# i#) 
  | i# >=# 0# && i# <# n# =
    case indexArray# arr# i# of (# e #) -> e
  | otherwise = error $ "indexPArr: out of bounds parallel array index; " ++
			"idx = " ++ show (I# i#) ++ ", arr len = "
			++ show (I# n#)

-- encapsulate writing into a mutable array into the `ST' monad
--
writeMPArr                           :: MPArr s e -> Int -> e -> ST s ()
{-# INLINE writeMPArr #-}
writeMPArr (MPArr n# marr#) (I# i#) e 
  | i# >=# 0# && i# <# n# =
    ST $ \s# ->
    case writeArray# marr# i# e s# of s'# -> (# s'#, () #)
  | otherwise = error $ "writeMPArr: out of bounds parallel array index; " ++
			"idx = " ++ show (I# i#) ++ ", arr len = "
			++ show (I# n#)

#endif /* __HADDOCK__ */

