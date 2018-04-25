Time-stamp: <Fri Jul 12 1996 16:41:04 Stardate: [-31]7798.26 hwloidl>

This file should serve as a replacement for Force.lhs.
It uses strategies instead of a class of forcing functions.
Phil's current working copy of the strategies module is:
/local/grasp_tmp5/trinder/tests/SUN4/parallel/ParallelStrategiesV.hs

---------------------------------------------------------------------------

  This Module Defines the parallel strategy combinators

	Phil Trinder 10/1/96                             

***********************	parallelStrategies *************************

Version V, with `type Strategy a : a -> a'

> module Strategies (Strategies.., Parallel..)  where
>
>#if defined(GRAN)
> import PreludeGlaST                        -- only needed for markStrat
>#endif
> import Parallel

------------------------------------------------------------------------------
			Strategy Type
------------------------------------------------------------------------------

N.B. This may be rewritten to use constructor classes when
they become available in Haskell 1.3 

> type Strategy a = a -> a

A strategy function, s, guarantees to return the same argument value, but
may perform some evaluation on it. i.e. s applied to some argument a
is less defined than a:

s a [ a
    -

More formally, a strategy s is a projection, i.e. both 

  a retraction: s [ id
                  -
  and idempotent: s o s = s

------------------------------------------------------------------------------
			Basic Strategies
------------------------------------------------------------------------------

note that

seq, par :: a -> Strategy b

@rnf@ performs *no* evaluation on it's argument 

> r0 :: Strategy a 
> r0 x = x

@rwhnf@ reduces it's argument to weak head normal form 

> rwhnf :: Strategy a 
> rwhnf x = x `seq` x  

> class NFData a where
>   rnf :: Strategy a
>   rnf = rwhnf

@rnf@ evaluates it's argument to normal form. Its default method is reducing
to weak head normal form. This is useful for base types. 
A specific method is necessay for constructed types.

> class (NFData a, Integral a) => NFDataIntegral a
> class (NFData a, Ord a) => NFDataOrd a

------------------------------------------------------------------------------
			Using & Marking a Strategy
------------------------------------------------------------------------------

Strategy application

> using :: a -> Strategy a -> a
> using x s = s x -- `seq` x

>#if defined(GRAN)

Marking a strategy.

Actually, @markStrat@  sticks a label @n@  into the sparkname  field of the
thread executing strategy @s@. Together with a runtime-system that supports
propagation of sparknames to the children this means that this strategy and
all its children have  the sparkname @n@ (if the  static sparkname field in
the @parGlobal@ annotation contains the value 1). Note, that the @SN@ field
of starting the marked strategy itself contains the sparkname of the parent
thread. The END event contains @n@ as sparkname.

> markStrat :: Int -> Strategy a -> Strategy a 
> markStrat n s x = unsafePerformPrimIO (
>     _casm_ ``%r = set_sparkname(CurrentTSO, %0);'' n `thenPrimIO` \ z ->
>     returnPrimIO (s x))
>#endif  /* GRAN */

------------------------------------------------------------------------------
			Strategy Instances
------------------------------------------------------------------------------
			Lists
------------------------------------------------------------------------------

N.B. These functions traverse their result, but do *not* construct
a new version 

> instance NFData a => NFData [a] where
>  rnf nil@[] = nil
>  rnf xs@(x:rest) = rnf x `seq` 
>                    rnf rest `seq` 
>                    xs

> {-#SPECIALISE instance NFData [Char]#-}
> {-#SPECIALISE instance NFData [Int]#-}
> {-#SPECIALISE instance NFData [[Char]]#-}
> {-#SPECIALISE instance NFData [[Int]]#-}

------------------------------------------------------------------------------
			Tuples
------------------------------------------------------------------------------

> instance (NFData a, NFData b) => NFData (a,b) where
>   rnf p@(x,y) = rnf x `seq` 
> 		  rnf y `seq`
>                 p
>
> instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
>   rnf t@(x,y,z) = rnf x `seq` 
>		    rnf y `seq` 
>                   rnf z `seq` 
>                   t

------------------------------------------------------------------------------
			Numbers
------------------------------------------------------------------------------

Weak head normal form and normal form are identical for
enumerations like integers

> instance NFData Int 
> instance NFData Integer
> instance NFData Float
> instance NFData Double
> 
> instance NFDataIntegral Int
> instance NFDataOrd Int

> instance (NFData a) => NFData (Ratio a) where
>   rnf p@(x:%y) = rnf x `seq` 
> 		   rnf y `seq`
>                  p
>
> instance (NFData a) => NFData (Complex a) where
>   rnf p@(x:+y) = rnf x `seq` 
> 		   rnf y `seq`
>                  p
>

------------------------------------------------------------------------------
			Other basic types
------------------------------------------------------------------------------

> instance NFData Char
> instance NFData Bool

------------------------------------------------------------------------------
			Useful functions using Strategies
------------------------------------------------------------------------------
			Lists - Parallel
------------------------------------------------------------------------------

Applies a strategy to each element in a list in parallel

> parList :: Strategy a -> Strategy [a]
> parList strat nil@[]     = nil
> parList strat xs@(x:rest) = strat x `par` 
>			      (parList strat rest) `seq`
>                             xs

Applies a strategy to the first  n elements of a list  in parallel

> parListN :: (Integral b) => b -> Strategy a -> Strategy [a]
> parListN n strat nil@[]      = nil
> parListN 0 strat xs          = xs
> parListN n strat xs@(x:rest) = strat x `par` 
>			         (parListN (n-1) strat rest) `seq`
>                                xs

Evaluates just the Nth element of it's argument list (if there is
one) in parallel with the result. e.g. parListNth 2 [e1, e2, e3]
evaluates e2 

> parListNth :: Int -> Strategy a -> Strategy [a]
> parListNth n strat xs
>   | length (take n xs) >= n  = strat (xs !! (n-1)) `par` xs
>   | otherwise 	       = xs

parListChunk sequentially applies a strategy to chunks (sub-sequences) 
of a list in parallel. Useful to increase grain size.

> parListChunk :: (Integral b) => b -> Strategy a -> Strategy [a]
> parListChunk n strat [] = []
> parListChunk n strat xs = seqListN n strat xs `par` 
>			    parListChunk n strat (drop n xs) `par`
>                           xs

parMap semantics:  maps f over the list xs.
dynamic behaviour: evaluates each element of the result in parallel, to
		      the degree specified by the strategy parameter, strat.

> parMap :: Strategy b -> (a -> b) -> [a] -> [b]
> parMap strat f xs 	= strategy (map f xs)
>		    	  where
>		      	    strategy = parList strat 

> parFlatMap :: Strategy [b] -> (a -> [b]) -> [a] -> [b]
> parFlatMap strat f xs = concat (parMap strat f xs)

------------------------------------------------------------------------------
		Lists - Sequential
------------------------------------------------------------------------------

Sequentially applies a strategy to each element of a list

> seqList :: Strategy a -> Strategy [a]
> seqList strat nil@[]      = nil
> seqList strat xs@(x:rest) = strat x `seq` 
>			      (seqList strat rest) `seq`
>                             xs

Sequentially applies a strategy to the first  n elements of a list. e.g.
seqListN 2 [e1, e2, e3] evaluates e1 and e2

> seqListN :: (Integral b) => b -> Strategy a -> Strategy [a]
> seqListN n strat nil@[]      = nil
> seqListN 0 strat xs          = xs
> seqListN n strat xs@(x:rest) = strat x `seq` 
>			         (seqListN (n-1) strat rest) `seq`
>                                xs

Applies a strategy to the Nth element of it's argument (if there is
one) before returning the result. e.g. seqListNth 2 [e1, e2, e3]
evaluates e2

> seqListNth n strat xs 
>  | length (take n xs) >= n  = strat (xs !! (n-1)) `seq` xs
>  | otherwise 		      = xs

Implements a `rolling buffer' of length n, i.e.applies a strategy
to the nth element of list when the head is demanded.

> fringeList :: Int -> Strategy b -> Strategy [b] 
> fringeList n strat [] = []
> fringeList n strat (r:rs) = 
>   seqListNth (n-1) strat rs `par`
>   r:fringeList n strat rs

------------------------------------------------------------------------------
			Tuples
------------------------------------------------------------------------------

Applies a strategy to both elements of a pair in parallel. The reason for the
second `par` is so that the strategy terminates quickly. This is
important if the strategy is used as the 1st argument of a seq

> parPair :: Strategy a -> Strategy b -> Strategy (a,b)
> parPair strata stratb p@(x,y) = strata x `par` 
>				  stratb y `par` 
>				  p

Sequentially applies a strategy to both elements of a pair.

> seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
> seqPair strata stratb p@(x,y) = strata x `seq` 
>			          stratb y `seq`
>     			          p

The above doesn't work if one strategy is r0 (seq is strict in its first arg). A more expensive but correct version is:

% new_seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
% new_seqPair strata stratb p@(x,y) = let 
%                                       x' = strata x
%                                       y' = stratb y 
%                                     in
%                                     (x',y') `seq` p

The same for triples:

> parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
> parTriple strata stratb stratc p@(x,y,z) = strata x `par` 
>				           stratb y `par` 
>				           stratc z `par` 
>				           p

> seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
> seqTriple strata stratb stratc p@(x,y,z) = strata x `seq` 
>			                     stratb y `seq`
>			                     stratc z `seq`
>     			                     p

------------------------------------------------------------------------------
			Arrays
------------------------------------------------------------------------------

> instance (NFData a, Ix a, NFData b) => NFData (Array a b) where
>   rnf a = rnf (bounds a) `seq` foldr seq () (elems a) `seq` a

Associations maybe useful even withou mentioning Arrays.

See: .../lib/prelude/TyArrays.hs:
data  Assoc a b =  a := b  deriving ()

> instance (NFData a, NFData b) => NFData (Assoc a b) where
>   rnf a@(x := y) = rnf x `seq` rnf y `seq` a

------------------------------------------------------------------------------
	                Some strategies specific for Lolita	
------------------------------------------------------------------------------

The following is useful in mergePenGroups

> fstPairFstList = seqListN 1 (seqPair rwhnf r0)

Some HACKs for Lolita. AFAIK force is just another name for our rnf and
sforce is a shortcut (definition here is identical to the one in Force.lhs)

> force :: (NFData a) => Strategy a
> force = rnf
> sforce x y = force x `seq` y

