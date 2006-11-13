-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parallel strategy combinators. See
-- <http://www.macs.hw.ac.uk/~dsg/gph/papers/html/Strategies/strategies.html>
-- for more information.
--
-- Original authors:
--	Phil Trinder, Hans-Wolfgang Loidl, Kevin Hammond et al. 
--
-----------------------------------------------------------------------------
module Control.Parallel.Strategies where

-- based on hslibs/concurrent/Strategies.lhs; see it for more detailed
-- code comments. 

import Control.Parallel as Parallel (par, pseq)
import Data.Array
import Data.Complex

import Prelude hiding (seq)
import qualified Prelude (seq)

-- not a terribly portable way of getting at Ratio rep.
#ifdef __GLASGOW_HASKELL__
import GHC.Real	(Ratio(..))	-- The basic defns for Ratio
#endif

#ifdef __HUGS__
import Hugs.Prelude(Ratio(..) )
#endif

#ifdef __NHC__
import Ratio (Ratio(..) )
#endif

infixl 0 `using`,`demanding`,`sparking`              -- weakest precedence!

infixr 2 >||                -- another name for par
infixr 3 >|                 -- another name for seq
infixl 6 $||, $|            -- strategic function application (seq and par)
infixl 9 .|, .||, -|, -||   -- strategic (inverse) function composition

-- We need 'pseq', not the Prelude 'seq' here.  See the documentation
-- with 'pseq' in Control.Parallel.
seq = Parallel.pseq

------------------------------------------------------------------------------
-- *			Strategy Type, Application and Semantics	      
------------------------------------------------------------------------------

{-
The basic combinators for strategies are 'par' and 'seq' but with types that 
indicate that they only combine the results of a strategy application. 

NB: This version can be used with Haskell 1.4 (GHC 2.05 and beyond), *but*
    you won't get strategy checking on seq (only on par)!

The operators >| and >|| are alternative names for `seq` and `par`.
With the introduction of a Prelude function `seq` separating the Prelude 
function from the Strategy function becomes a pain. The notation also matches
the notation for strategic function application.
-}

type Done = ()

-- | A strategy takes a value and returns a 'Done' value to indicate that
--   the specifed evaluation has been performed.
type Strategy a = a -> Done


-- | Evaluates the first argument before the second.
(>|) :: Done -> Done -> Done 
{-# INLINE (>|) #-}
(>|) = Prelude.seq

-- | Evaluates the first argument in parallel with the second.
(>||) :: Done -> Done -> Done 
{-# INLINE (>||) #-}
(>||) = Parallel.par


-- | Takes a value and a strategy, and applies the strategy to the
-- value before returning the value. Used to express data-oriented 
-- parallelism. @x \`using\` s@ is a projection on @x@, i.e. both:
--
-- [a retraction] @x \`using\` s@ &#x2291; @x@
--
-- [idempotent] @(x \`using\` s) \`using\` s@ = @x \`using\` s@
--
using :: a -> Strategy a -> a
using x s = s x `seq` x


-- | Evaluates the second argument before the first.
-- Used to express control-oriented parallelism. The second
-- argument is usually a strategy application.
demanding :: a -> Done -> a
demanding = flip seq


-- | Evaluates the second argument in parallel with the first.
-- Used to express control-oriented
-- parallelism. The second argument is usually a strategy application.
sparking :: a -> Done -> a
sparking  = flip Parallel.par
-- Sparking should only be used
-- with a singleton sequence as it is not necessarily executed.

-- | A strategy corresponding to 'par': 
-- @x \`par\` e@ = @e \`using\` sPar x@.
--
-- 'sPar' has been superceded by 'sparking'.
-- Replace @e \`using\` sPar x@ with @e \`sparking\` rwhnf x@.
{-# DEPRECATED sPar "Use sparking instead." #-}
sPar :: a -> Strategy b
sPar x y = x `par` ()

-- | A strategy corresponding to 'seq': 
-- @x \`seq\` e@ = @e \`using\` sSeq x@.
--
-- 'sSeq' has been superceded by 'demanding'. 
-- Replace @e \`using\` sSeq x@ with @e \`demanding\` rwhnf x@.
{-# DEPRECATED sSeq "Use demanding instead." #-}
sSeq :: a -> Strategy b
sSeq x y = x `seq` ()

-----------------------------------------------------------------------------
-- *			Basic Strategies				     
-----------------------------------------------------------------------------

-- | Performs /no/ evaluation of its argument.
r0 :: Strategy a 
r0 x = ()

-- | Reduces its argument to weak head normal form.
rwhnf :: Strategy a 
rwhnf x = x `seq` ()  

class NFData a where
  -- | Reduces its argument to (head) normal form.
  rnf :: Strategy a
  -- Default method. Useful for base types. A specific method is necessay for
  -- constructed types
  rnf = rwhnf

class (NFData a, Integral a) => NFDataIntegral a
class (NFData a, Ord a) => NFDataOrd a

------------------------------------------------------------------------------
-- *                     Strategic Function Application
------------------------------------------------------------------------------

{-
These are very
handy when writing pipeline parallelism asa sequence of @$@, @$|@ and
@$||@'s. There is no need of naming intermediate values in this case. The
separation of algorithm from strategy is achieved by allowing strategies
only as second arguments to @$|@ and @$||@.
-}

-- | Sequential function application. The argument is evaluated using
--   the given strategy before it is given to the function.
($|) :: (a -> b) -> Strategy a -> a -> b
f $| s  = \ x -> f x `demanding` s x

-- | Parallel function application. The argument is evaluated using
-- the given strategy, in parallel with the function application.
($||) :: (a -> b) -> Strategy a -> a -> b
f $|| s = \ x -> f x `sparking` s x

-- | Sequential function composition. The result of
-- the second function is evaluated using the given strategy, 
-- and then given to the first function.
(.|) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
(.|) f s g = \ x -> let  gx = g x 
                    in   f gx `demanding` s gx

-- | Parallel function composition. The result of the second
-- function is evaluated using the given strategy,
-- in parallel with the application of the first function.
(.||) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
(.||) f s g = \ x -> let  gx = g x 
                     in   f gx `sparking` s gx

-- | Sequential inverse function composition, 
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the 
-- given strategy, and then given to the second function.
(-|) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-|) f s g = \ x -> let  fx = f x 
                    in   g fx `demanding` s fx

-- | Parallel inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the 
-- given strategy, in parallel with the application of the 
-- second function.
(-||) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-||) f s g = \ x -> let  fx = f x 
                     in   g fx `sparking` s fx 

------------------------------------------------------------------------------
--			Marking a Strategy
------------------------------------------------------------------------------

{-
Marking a strategy.

Actually, @markStrat@  sticks a label @n@  into the sparkname  field of the
thread executing strategy @s@. Together with a runtime-system that supports
propagation of sparknames to the children this means that this strategy and
all its children have  the sparkname @n@ (if the  static sparkname field in
the @parGlobal@ annotation contains the value 1). Note, that the @SN@ field
of starting the marked strategy itself contains the sparkname of the parent
thread. The END event contains @n@ as sparkname.
-}

#if 0
markStrat :: Int -> Strategy a -> Strategy a 
markStrat n s x = unsafePerformPrimIO (
     _casm_ ``%r = set_sparkname(CurrentTSO, %0);'' n `thenPrimIO` \ z ->
     returnPrimIO (s x))
#endif

-----------------------------------------------------------------------------
--			Strategy Instances and Functions		     
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- *	                Tuples
-----------------------------------------------------------------------------

{-
We currently support up to 9-tuples. If you need longer tuples you have to 
add the instance explicitly to your program.
-}

instance (NFData a, NFData b) => NFData (a,b) where
  rnf (x,y) = rnf x `seq` rnf y

instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
  rnf (x,y,z) = rnf x `seq` rnf y `seq` rnf z 

instance (NFData a, NFData b, NFData c, NFData d) => NFData (a,b,c,d) where
  rnf (x1,x2,x3,x4) = rnf x1 `seq` 
		        rnf x2 `seq` 
		        rnf x3 `seq` 
		        rnf x4 

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) => 
         NFData (a1, a2, a3, a4, a5) where
  rnf (x1, x2, x3, x4, x5) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) => 
         NFData (a1, a2, a3, a4, a5, a6) where
  rnf (x1, x2, x3, x4, x5, x6) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) => 
         NFData (a1, a2, a3, a4, a5, a6, a7) where
  rnf (x1, x2, x3, x4, x5, x6, x7) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8) => 
         NFData (a1, a2, a3, a4, a5, a6, a7, a8) where
  rnf (x1, x2, x3, x4, x5, x6, x7, x8) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7 `seq`
                  rnf x8

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9) => 
         NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  rnf (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7 `seq`
                  rnf x8 `seq`
                  rnf x9

-- | Apply two strategies to the elements of a pair sequentially
--   from left to right.
seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
seqPair strata stratb (x,y) = strata x `seq` stratb y 

-- | Apply two strategies to the elements of a pair in parallel.
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair strata stratb (x,y) = strata x `par` stratb y `par` ()
-- The reason for the last 'par' is so that the strategy terminates 
-- quickly. This is important if the strategy is used as the 1st 
-- argument of a seq

-- | Apply three strategies to the elements of a triple in sequentially
--   from left to right.
seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
seqTriple strata stratb stratc p@(x,y,z) = 
  strata x `seq` 
  stratb y `seq`
  stratc z 

-- | Apply three strategies to the elements of a triple in parallel.
parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
parTriple strata stratb stratc (x,y,z) = 
  strata x `par` 
  stratb y `par` 
  stratc z `par`
  ()

{-
Weak head normal form and normal form are identical for integers, so the 
default rnf is sufficient. 
-}
instance NFData Int 
instance NFData Integer
instance NFData Float
instance NFData Double

instance NFDataIntegral Int
instance NFDataOrd Int

--Rational and complex numbers.

instance (Integral a, NFData a) => NFData (Ratio a) where
  rnf (x:%y) = rnf x `seq` 
               rnf y `seq`
               ()

instance (RealFloat a, NFData a) => NFData (Complex a) where
  rnf (x:+y) = rnf x `seq` 
	         rnf y `seq`
               ()

instance NFData Char
instance NFData Bool
instance NFData ()

-----------------------------------------------------------------------------
-- 			Lists						    
----------------------------------------------------------------------------

instance NFData a => NFData [a] where
  rnf [] = ()
  rnf (x:xs) = rnf x `seq` rnf xs

----------------------------------------------------------------------------
-- *                   Lists: Parallel Strategies
----------------------------------------------------------------------------

-- | Applies a strategy to every element of a list in parallel.
parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)

-- | Applies a strategy to the first @n@ elements of a list in parallel.
parListN :: (Integral b) => b -> Strategy a -> Strategy [a]
parListN n strat []     = ()
parListN 0 strat xs     = ()
parListN n strat (x:xs) = strat x `par` (parListN (n-1) strat xs)

-- | Evaluates @n@ elements of the spine of the argument list and applies
-- the given strategy to the @n@th element (if there is one) in parallel with
-- the result. E.g. @parListNth 2 [e1, e2, e3]@ evaluates @e3@.
parListNth :: Int -> Strategy a -> Strategy [a]
parListNth n strat xs 
  | null rest = ()
  | otherwise = strat (head rest) `par` ()
  where
    rest = drop n xs

-- | Splits a list into chunks (sub-sequences) of length @n@,
-- and applies a strategy sequentially to the elements in each
-- chunk. The chunks are evaluated in parallel.
-- This is useful for increasing the grain size.
parListChunk :: Int -> Strategy a -> Strategy [a]
parListChunk n strat [] = ()
parListChunk n strat xs = seqListN n strat xs `par` 
			    parListChunk n strat (drop n xs)

-- | Applies a function to each element of a list and 
-- and evaluates the result list in parallel,
-- using the given strategy for each element.
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

-- | Uses 'parMap' to apply a list-valued function to each
-- element of a list in parallel, and concatenates the results.
parFlatMap :: Strategy [b] -> (a -> [b]) -> [a] -> [b]
parFlatMap strat f xs = concat (parMap strat f xs)

-- | Zips together two lists using a function,
-- and evaluates the result list in parallel.
parZipWith :: Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith strat z as bs = 
  zipWith z as bs `using` parList strat

----------------------------------------------------------------------------
-- *                     Lists: Sequential Strategies
----------------------------------------------------------------------------

-- | Sequentially applies a strategy to each element of a list.
seqList :: Strategy a -> Strategy [a]
seqList strat []     = ()
seqList strat (x:xs) = strat x `seq` (seqList strat xs)

-- | Sequentially applies a strategy to the first n elements of a list.
seqListN :: (Integral a) => a -> Strategy b -> Strategy [b]
seqListN n strat []     = ()
seqListN 0 strat xs     = ()
seqListN n strat (x:xs) = strat x `seq` (seqListN (n-1) strat xs)

-- | Applies a strategy to the @n@th element of a list
--  (if there is one) before returning the result. 
--  E.g. @seqListNth 2 [e1, e2, e3]@ evaluates @e3@.
seqListNth :: Int -> Strategy b -> Strategy [b]
seqListNth n strat xs 
  | null rest = ()
  | otherwise = strat (head rest) 
  where
    rest = drop n xs

-- | Parallel n-buffer function added for the revised version of the strategies
-- paper. 'parBuffer' supersedes the older @fringeList@. It has the same
-- semantics.
parBuffer :: Int -> Strategy a -> [a] -> [a]
parBuffer n s xs = 
  return xs (start n xs)
  where
    return (x:xs) (y:ys) = (x:return xs ys) `sparking` s y
    return xs     []     = xs

    start n []     = []
    start 0 ys     = ys
    start n (y:ys) = start (n-1) ys `sparking` s y

{-
 'fringeList' implements a `rolling buffer' of length n, i.e.applies a
 strategy to the nth element of list when the head is demanded. More
 precisely:

   semantics:         fringeList n s = id :: [b] -> [b]
   dynamic behaviour: evalutates the nth element of the list when the
		      head is demanded.
   
 The idea is to provide a `rolling buffer' of length n.
fringeList :: (Integral a) => a -> Strategy b -> [b] -> [b]
fringeList n strat [] = []
fringeList n strat (r:rs) = 
  seqListNth n strat rs `par`
  r:fringeList n strat rs
-}

------------------------------------------------------------------------------
-- *			Arrays
------------------------------------------------------------------------------
instance (Ix a, NFData a, NFData b) => NFData (Array a b) where
  rnf x = rnf (bounds x) `seq` seqList rnf (elems x) `seq` ()

-- | Apply a strategy to all elements of an array sequentially.
seqArr :: (Ix b) => Strategy a -> Strategy (Array b a)
seqArr s arr = seqList s (elems arr)

-- | Apply a strategy to all elements of an array in parallel.
parArr :: (Ix b) => Strategy a -> Strategy (Array b a)
parArr s arr = parList s (elems arr)

-- | Associations maybe useful even without mentioning Arrays.
{-# DEPRECATED Assoc "Does not belong in Control.Parallel.Strategies" #-}
data  Assoc a b =  a := b  deriving ()

instance (NFData a, NFData b) => NFData (Assoc a b) where
  rnf (x := y) = rnf x `seq` rnf y `seq` ()

------------------------------------------------------------------------------
-- *	                Some strategies specific for Lolita	
------------------------------------------------------------------------------

{-# DEPRECATED fstPairFstList "This was just an example. Write your own." #-}
fstPairFstList :: (NFData a) => Strategy [(a,b)]
fstPairFstList = seqListN 1 (seqPair rwhnf r0)

-- Some HACKs for Lolita. AFAIK force is just another name for our rnf and
-- sforce is a shortcut (definition here is identical to the one in Force.lhs)

{-# DEPRECATED force, sforce "Lolita-specific hacks." #-}
force :: (NFData a) => a -> a 
sforce :: (NFData a) => a -> b -> b

force = id $| rnf
sforce x y = force x `seq` y
