-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Parallel.Strategies
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Strategies.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Parallel strategy combinators
--
-----------------------------------------------------------------------------

{-
Time-stamp: <Wed Mar 21 2001 00:45:34 Stardate: [-30]6360.15 hwloidl>
$Id: Strategies.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $

This module defines parallel strategy combinators

	Phil Trinder, Hans-Wolfgang Loidl, Kevin Hammond et al. 

	Based on Version VII (1/5/96) `Strategies96' of type a -> ()

Author:    $Author: simonmar $
Date:      $Date: 2001/06/28 14:15:02 $
Revision:  $Revision: 1.1 $
Source:    $Source: /srv/cvs/cvs.haskell.org/fptools/libraries/base/Control/Parallel/Strategies.hs,v $
State:     $State: Exp $

This module defines evaluation strategies for controlling the parallel
evaluation of non-strict programs. They provide a clean separation between
algorithmic and behavioural code.

The functions described here, and their use is documented in

"Algorithm + Strategy = Parallelism", 
P.W. Trinder, K. Hammond, H-W. Loidl, S.L. Peyton Jones 
In Journal of Functional Programming 8(1):23--60, January 1998.
URL: http://www.cee.hw.ac.uk/~dsg/gph/papers/ps/strategies.ps.gz

This module supports Haskell 1.2, Haskell 1.4 and Haskell98.
The distinction is made based on the __HASKELL1__ CPP variable. 
Parts of the module could be rewritten using constructor classes.

-----------------------------------------------------------------------------
The history of the Strategies module:

Changelog:
$Log: Strategies.hs,v $
Revision 1.1  2001/06/28 14:15:02  simonmar
First cut of the Haskell Core Libraries
=======================================

NOTE: it's not meant to be a working snapshot.  The code is just here
to look at and so the NHC/Hugs guys can start playing around with it.

There is no build system.  For GHC, the libraries tree is intended to
be grafted onto an existing fptools/ tree, and the Makefile in
libraries/core is a quick hack for that setup.  This won't work at the
moment without the other changes needed in fptools/ghc, which I
haven't committed because they'll cause breakage.  However, with the
changes required these sources build a working Prelude and libraries.

The layout mostly follows the one we agreed on, with one or two minor
changes; in particular the Data/Array layout probably isn't final
(there are several choices here).

The document is in libraries/core/doc as promised.

The cbits stuff is just a copy of ghc/lib/std/cbits and has
GHC-specific stuff in it.  We should really separate the
compiler-specific C support from any compiler-independent C support
there might be.

Don't pay too much attention to the portability or stability status
indicated in the header of each source file at the moment - I haven't
gone through to make sure they're all consistent and make sense.

I'm using non-literate source outside of GHC/.  Hope that's ok with
everyone.

We need to discuss how the build system is going to work...

Revision 1.3  2001/03/22 03:51:12  hwloidl
                                                  -*- outline -*-
Time-stamp: <Thu Mar 22 2001 03:50:16 Stardate: [-30]6365.79 hwloidl>

This commit covers changes in GHC to get GUM (way=mp) and GUM/GdH (way=md)
working. It is a merge of my working version of GUM, based on GHC 4.06,
with GHC 4.11. Almost all changes are in the RTS (see below).

GUM is reasonably stable, we used the 4.06 version in large-ish programs for
recent papers. Couple of things I want to change, but nothing urgent.
GUM/GdH has just been merged and needs more testing. Hope to do that in the
next weeks. It works in our working build but needs tweaking to run.
GranSim doesn't work yet (*sigh*). Most of the code should be in, but needs
more debugging.

ToDo: I still want to make the following minor modifications before the release
- Better wrapper skript for parallel execution [ghc/compiler/main]
- Update parallel docu: started on it but it's minimal [ghc/docs/users_guide]
- Clean up [nofib/parallel]: it's a real mess right now (*sigh*)
- Update visualisation tools (minor things only IIRC) [ghc/utils/parallel]
- Add a Klingon-English glossary

* RTS:

Almost all changes are restricted to ghc/rts/parallel and should not
interfere with the rest. I only comment on changes outside the parallel
dir:

- Several changes in Schedule.c (scheduling loop; createThreads etc);
  should only affect parallel code
- Added ghc/rts/hooks/ShutdownEachPEHook.c
- ghc/rts/Linker.[ch]: GUM doesn't know about Stable Names (ifdefs)!!
- StgMiscClosures.h: END_TSO_QUEUE etc now defined here (from StgMiscClosures.hc)
                     END_ECAF_LIST was missing a leading stg_
- SchedAPI.h: taskStart now defined in here; it's only a wrapper around
              scheduleThread now, but might use some init, shutdown later
- RtsAPI.h: I have nuked the def of rts_evalNothing

* Compiler:

- ghc/compiler/main/DriverState.hs
  added PVM-ish flags to the parallel way
  added new ways for parallel ticky profiling and distributed exec

- ghc/compiler/main/DriverPipeline.hs
  added a fct run_phase_MoveBinary which is called with way=mp after linking;
  it moves the bin file into a PVM dir and produces a wrapper script for
  parallel execution
  maybe cleaner to add a MoveBinary phase in DriverPhases.hs but this way
  it's less intrusive and MoveBinary makes probably only sense for mp anyway

* Nofib:

- nofib/spectral/Makefile, nofib/real/Makefile, ghc/tests/programs/Makefile:
  modified to skip some tests if HWL_NOFIB_HACK is set; only tmp to record
  which test prgs cause problems in my working build right now

Revision 1.2  2000/11/18 02:13:11  hwloidl
Now provides explicit def of seq (rather than just re-exporting).
Required by the current version of the compiler.

Revision 1.1  2000/01/14 13:34:32  hwloidl
Module for specifying (parallel) behavioural code.

Revision 1.9  1997/10/01 00:27:19  hwloidl
Type of par and seq changed to Done -> Done -> Done with Done = ()
Works for Haskell 1.2 as well as Haskell 1.4 (checks the CPP variable
__HASKELL1__ to distinguish setups).
Fixed precedences for par and seq for Haskell 1.4 (stronger than using).
New infix operators >| and >|| as aliases for par and seq as strategy
combinators.

Revision 1.8  1997/05/20 21:13:22  hwloidl
Revised to use `demanding` and `sparking` (final JFP paper version)

Revision 1.7  1997/04/02 21:26:21  hwloidl
Minor changes in documentation, none in the code.


revision 1.5
Version VII.1; Strategies96; Type: a -> ()
Minor changes to previous version.
CPP flags now separate GUM from GranSim version.
Infix declaration for `using` (important for e.g. quicksort where the old
version puts parentheses in the wrong way).
Moer instances for NFData and markStartegies (in GranSim setup only).

revision 1.4
Version VII; Strategies96; Type: a -> ()
The type has changed again; with the old type it's not possible to describe
all the strategies we want (for example seqPair r0 rnf which should not
evaluate the first component of the pair at all). The () type acts as info
that the strategy has been applied.
The function `using` is used as inverse strategy application i.e.
on top level we usually have something like res `using` strat where ...
The markStrategy hack is included in this version: it attaches an Int value
to the currently running strategy (this can be inherited by all sub-strats)
It doesn't model the jumps between evaluating producer and consumer properly
(for that something like cost centers would be necessary).

revision 1.3
Version VI (V-based); Strategies95; Type: a -> a
Now uses library modules like FiniteMap with strategies in there.
CPP flags for using the same module with GUM and GranSim.
A few new strategies.

revision 1.2
Version V; Strategies95; Type: a -> a
The type of Strategies has changed from a -> () to a -> a
All strategies and instances of NFData have been redefined accordingly.
This branch started off after discussions between PWT, SLPJ and HWL in
mid Nov (start of development of the actual module: 10/1/96)

revision 1.1 Initial revision
-----------------------------------------------------------------------------
-- To use fakeinfo first replace all %%$ by \@ 
-- If you have fakeinfo makers in the file you need a slightly modified 
-- version of the lit-deatify script (called by lit2pgm). You get that 
-- version on Suns and Alphas in Glasgow by using 
--  \tr{lit2pgm -H "${HOME}/bin/`hw_os`"}
-- in your Makefile
-----------------------------------------------------------------------------

--@node Evaluation Strategies, , ,
--@chapter Evaluation Strategies

--@menu
--* Imports and infix declarations::  
--* Strategy Type and Application::  
--* Basic Strategies::		
--* Strategic Function Application::  
--* Marking a Strategy::	
--* Strategy Instances::	
--* Lolita-specific Strategies::  
--@end menu

--@node Imports and infix declarations, Strategy Type and Application, Evaluation Strategies, Evaluation Strategies
--@section Imports and infix declarations

> module Strategies(
>#if (__HASKELL1__>=4)
>                   module Strategies,
>                   module Parallel
>#else
>                   Strategies..
>#endif
>                  ) where
>
>#if defined(GRAN) && !(__HASKELL1__>=4)
> import PreludeGlaST                        -- only needed for markStrat
>#endif
>#if (__HASKELL1__>=4)

<> import Prelude hiding (seq)
<> import qualified Parallel

> import Parallel

>#else
> import Parallel renaming (par to par_from_Parallel, seq to seq_from_Parallel)
>#endif

>#if (__HASKELL1__>=4)
> import Ix
> import Array
>#endif

>#if defined(PAR_GRAN_LIST)
> import QSort -- tmp (only for parGranList)
>#endif

I lifted the precedence of @par@ and @seq@ by one level to make @using@ the 
combinator with the weakest precedence.
Oooops, there seems to be a bug in ghc 0.29 prohibiting another infix 
declaration of @par@ and @seq@ despite renaming the imported versions.

>#if (__HASKELL1__>=4)

<> infixr 2 `par`           -- was: 0
<> infixr 3 `seq`           -- was: 1 

>#else
> infixr 0 `par`           -- was: 0
> infixr 1 `seq`           -- was: 1 
>#endif

> infixl 0 `using`,`demanding`,`sparking`              -- weakest precedence!

> infixr 2 >||                -- another name for par
> infixr 3 >|                 -- another name for seq
> infixl 6 $||, $|            -- strategic function application (seq and par)
> infixl 9 .|, .||, -|, -||   -- strategic (inverse) function composition

> strategy_version = "$Revision: 1.1 $"
> strategy_id = "$Id: Strategies.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $"

------------------------------------------------------------------------------
			Strategy Type, Application and Semantics	      
------------------------------------------------------------------------------
--@node Strategy Type and Application, Basic Strategies, Imports and infix declarations, Evaluation Strategies
--@section Strategy Type and Application

--@cindex Strategy

> type Done = ()
> type Strategy a = a -> Done

A strategy takes a value and returns a dummy `done' value to indicate that
the specifed evaluation has been performed.

The basic combinators for strategies are @par@ and @seq@ but with types that 
indicate that they only combine the results of a strategy application. 

NB: This version can be used with Haskell 1.4 (GHC 2.05 and beyond), *but*
    you won't get strategy checking on seq (only on par)!

The infix fcts >| and >|| are alternative names for `seq` and `par`.
With the introduction of a Prelude function `seq` separating the Prelude 
function from the Strategy function becomes a pain. The notation also matches
the notation for strategic function application.

--@cindex par
--@cindex seq
--@cindex >|
--@cindex >||

>#if (__HASKELL1__>=4)

par and seq have the same types as before; >| and >|| are more specific
and can only be used when composing strategies.

<> par :: Done -> Done -> Done 
<> par = Parallel.par
<> seq :: a -> b -> b      -- that's the real type of seq defined in Prelude
<> seq = Parallel.seq

> (>|), (>||) :: Done -> Done -> Done 
> {-# INLINE (>|) #-}
> {-# INLINE (>||) #-}
> (>|) = Prelude.seq
> (>||) = Parallel.par
>#else
> par, seq, (>|), (>||) :: Done -> Done -> Done 
> par = par_from_Parallel
> seq = seq_from_Parallel
> {-# INLINE (>|) #-}
> {-# INLINE (>||) #-}
> (>|) = seq
> (>||) = par
>#endif

--@cindex using

> using :: a -> Strategy a -> a
>#if (__HASKELL1__>=4)
> using x s = s x `seq` x
>#else
> using x s = s x `seq_from_Parallel` x
>#endif

using takes a strategy and a value, and applies the strategy to the
value before returning the value. Used to express data-oriented parallelism

x `using` s is a projection on x, i.e. both

  a retraction: x `using` s [ x
			    -
  and idempotent: (x `using` s) `using` s = x `using` s

demanding and sparking are used to express control-oriented
parallelism. Their second argument is usually a sequence of strategy
applications combined `par` and `seq`. Sparking should only be used
with a singleton sequence as it is not necessarily excuted

--@cindex demanding
--@cindex sparking

> demanding, sparking :: a -> Done -> a
>#if (__HASKELL1__>=4)
> demanding = flip Parallel.seq
> sparking  = flip Parallel.par
>#else
> demanding = flip seq_from_Parallel
> sparking  = flip par_from_Parallel
>#endif

sPar and sSeq have been superceded by sparking and demanding: replace 
  e `using` sPar x	with  	e `sparking`  x 
  e `using` sSeq x	with 	e `demanding` x

<sPar is a strategy corresponding to par. i.e. x `par` e <=> e `using` sPar x
<
<> sPar :: a -> Strategy b
<> sPar x y = x `par` ()
<
<sSeq is a strategy corresponding to seq. i.e. x `seq` e <=> e `using` sSeq x
<
<> sSeq :: a -> Strategy b
<> sSeq x y = x `seq` ()

-----------------------------------------------------------------------------
			Basic Strategies				     
-----------------------------------------------------------------------------
--@node Basic Strategies, Strategic Function Application, Strategy Type and Application, Evaluation Strategies
--@section Basic Strategies

r0 performs *no* evaluation on its argument.

--@cindex r0

> r0 :: Strategy a 
> r0 x = ()

rwhnf reduces its argument to weak head normal form.

--@cindex rwhnf
--@cindex rnf
--@cindex NFData

>#if defined(__HASKELL98__)
> rwhnf :: Strategy a 
> rwhnf x = x `seq` ()  
>#elif (__HASKELL1__==4)
> rwhnf :: Eval a => Strategy a 
> rwhnf x = x `seq` ()  
>#else
> rwhnf :: Strategy a 
> rwhnf x = x `seq_from_Parallel` ()  
>#endif

>#if defined(__HASKELL98__)
> class NFData a where
>#elif (__HASKELL1__>=4)
> class Eval a => NFData a where
>#else
> class NFData a where
>#endif
>   -- rnf reduces its argument to (head) normal form
>   rnf :: Strategy a
>   -- Default method. Useful for base types. A specific method is necessay for
>   -- constructed types
>   rnf = rwhnf
>
> class (NFData a, Integral a) => NFDataIntegral a
> class (NFData a, Ord a) => NFDataOrd a

------------------------------------------------------------------------------
                        Strategic Function Application
------------------------------------------------------------------------------
--@node Strategic Function Application, Marking a Strategy, Basic Strategies, Evaluation Strategies
--@section Strategic Function Application

The two  infix functions @$|@   and @$||@  perform sequential and  parallel
function application, respectively. They  are parameterised with a strategy
that is applied to the argument of the  function application.  This is very
handy when  writing  pipeline parallelism  as  a sequence of  @$@, @$|@ and
@$||@'s. There is no  need of naming intermediate values  in this case. The
separation  of algorithm from strategy  is  achieved by allowing strategies
only as second arguments to @$|@ and @$||@.

--@cindex $|
--@cindex $||

> ($|), ($||) :: (a -> b) -> Strategy a -> a -> b

<> f $| s  = \ x -> f x `using` \ _ -> s x `seq` ()
<> f $|| s = \ x -> f x `using` \ _ -> s x `par` ()

> f $| s  = \ x -> f x `demanding` s x
> f $|| s = \ x -> f x `sparking`  s x

The same thing for function composition (.| and .||) and inverse function
composition (-| and -||) for those who read their programs from left to 
right.

--@cindex .|
--@cindex .||
--@cindex -|
--@cindex -||

> (.|), (.||) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
> (-|), (-||) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)

> (.|) f s g = \ x -> let  gx = g x 
>                     in   f gx `demanding` s gx
> (.||) f s g = \ x -> let  gx = g x 
>                      in   f gx `sparking` s gx

> (-|) f s g = \ x -> let  fx = f x 
>                     in   g fx `demanding` s fx
> (-||) f s g = \ x -> let  fx = f x 
>                      in   g fx `sparking` s fx 

------------------------------------------------------------------------------
			Marking a Strategy
------------------------------------------------------------------------------
--@node Marking a Strategy, Strategy Instances, Strategic Function Application, Evaluation Strategies
--@section Marking a Strategy

Marking a strategy.

Actually, @markStrat@  sticks a label @n@  into the sparkname  field of the
thread executing strategy @s@. Together with a runtime-system that supports
propagation of sparknames to the children this means that this strategy and
all its children have  the sparkname @n@ (if the  static sparkname field in
the @parGlobal@ annotation contains the value 1). Note, that the @SN@ field
of starting the marked strategy itself contains the sparkname of the parent
thread. The END event contains @n@ as sparkname.

--@cindex markStrat

>#if defined(GRAN) && !(__HASKELL1__>=4)
> markStrat :: Int -> Strategy a -> Strategy a 
> markStrat n s x = unsafePerformPrimIO (
>      _casm_ ``%r = set_sparkname(CurrentTSO, %0);'' n `thenPrimIO` \ z ->
>      returnPrimIO (s x))
>#endif

-----------------------------------------------------------------------------
			Strategy Instances and Functions		     
-----------------------------------------------------------------------------
--@node Strategy Instances, Lolita-specific Strategies, Marking a Strategy, Evaluation Strategies
--@section Strategy Instances
-----------------------------------------------------------------------------
	                Tuples
-----------------------------------------------------------------------------
--@menu
--* Tuples::			
--* Numbers::			
--* Characters::		
--* Booleans::			
--* Unit::			
--* Lists::			
--* Arrays::			
--@end menu

--@node Tuples, Numbers, Strategy Instances, Strategy Instances
--@subsection Tuples

We currently support up to 9-tuples. If you need longer tuples you have to 
add the instance explicitly to your program.

> instance (NFData a, NFData b) => NFData (a,b) where
>   rnf (x,y) = rnf x `seq` rnf y

> instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
>   rnf (x,y,z) = rnf x `seq` rnf y `seq` rnf z 

> instance (NFData a, NFData b, NFData c, NFData d) => NFData (a,b,c,d) where
>   rnf (x1,x2,x3,x4) = rnf x1 `seq` 
> 		        rnf x2 `seq` 
> 		        rnf x3 `seq` 
> 		        rnf x4 

> -- code automatically inserted by `hwl-insert-NFData-n-tuple'
> instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) => 
>          NFData (a1, a2, a3, a4, a5) where
>   rnf (x1, x2, x3, x4, x5) =
>                   rnf x1 `seq`
>                   rnf x2 `seq`
>                   rnf x3 `seq`
>                   rnf x4 `seq`
>                   rnf x5

> -- code automatically inserted by `hwl-insert-NFData-n-tuple'
> instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) => 
>          NFData (a1, a2, a3, a4, a5, a6) where
>   rnf (x1, x2, x3, x4, x5, x6) =
>                   rnf x1 `seq`
>                   rnf x2 `seq`
>                   rnf x3 `seq`
>                   rnf x4 `seq`
>                   rnf x5 `seq`
>                   rnf x6

> -- code automatically inserted by `hwl-insert-NFData-n-tuple'
> instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) => 
>          NFData (a1, a2, a3, a4, a5, a6, a7) where
>   rnf (x1, x2, x3, x4, x5, x6, x7) =
>                   rnf x1 `seq`
>                   rnf x2 `seq`
>                   rnf x3 `seq`
>                   rnf x4 `seq`
>                   rnf x5 `seq`
>                   rnf x6 `seq`
>                   rnf x7

> -- code automatically inserted by `hwl-insert-NFData-n-tuple'
> instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8) => 
>          NFData (a1, a2, a3, a4, a5, a6, a7, a8) where
>   rnf (x1, x2, x3, x4, x5, x6, x7, x8) =
>                   rnf x1 `seq`
>                   rnf x2 `seq`
>                   rnf x3 `seq`
>                   rnf x4 `seq`
>                   rnf x5 `seq`
>                   rnf x6 `seq`
>                   rnf x7 `seq`
>                   rnf x8

> -- code automatically inserted by `hwl-insert-NFData-n-tuple'
> instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9) => 
>          NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
>   rnf (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
>                   rnf x1 `seq`
>                   rnf x2 `seq`
>                   rnf x3 `seq`
>                   rnf x4 `seq`
>                   rnf x5 `seq`
>                   rnf x6 `seq`
>                   rnf x7 `seq`
>                   rnf x8 `seq`
>                   rnf x9

--@cindex seqPair

> seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
> seqPair strata stratb (x,y) = strata x `seq` stratb y 

--@cindex parPair

> parPair :: Strategy a -> Strategy b -> Strategy (a,b)
> parPair strata stratb (x,y) = strata x `par` stratb y `par` ()

The reason for the  second `par` is so that the strategy terminates 
quickly. This is important if the strategy is used as the 1st argument of a seq

--@cindex seqTriple

> seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
> seqTriple strata stratb stratc p@(x,y,z) = 
>   strata x `seq` 
>   stratb y `seq`
>   stratc z 

--@cindex parTriple

> parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
> parTriple strata stratb stratc (x,y,z) = 
>   strata x `par` 
>   stratb y `par` 
>   stratc z `par`
>   ()

-----------------------------------------------------------------------------
			Numbers						     
-----------------------------------------------------------------------------
--@node Numbers, Characters, Tuples, Strategy Instances
--@subsection Numbers

Weak head normal form and normal form are identical for integers, so the 
default rnf is sufficient. 

> instance NFData Int 
> instance NFData Integer
> instance NFData Float
> instance NFData Double

> instance NFDataIntegral Int
> instance NFDataOrd Int

Rational and complex numbers.

>#if !(__HASKELL1__>=4)
> instance (NFData a) => NFData (Ratio a) where
>   rnf (x:%y) = rnf x `seq` 
>                rnf y `seq`
>                ()

> instance (NFData a) => NFData (Complex a) where
>   rnf (x:+y) = rnf x `seq` 
> 	         rnf y `seq`
>                ()
>#endif

-----------------------------------------------------------------------------
			Characters					      
-----------------------------------------------------------------------------
--@node Characters, Booleans, Numbers, Strategy Instances
--@subsection Characters

> instance NFData Char

-----------------------------------------------------------------------------
			Bools
-----------------------------------------------------------------------------
--@node Booleans, Unit, Characters, Strategy Instances
--@subsection Booleans

> instance NFData Bool

-----------------------------------------------------------------------------
			Unit						     
-----------------------------------------------------------------------------
--@node Unit, Lists, Booleans, Strategy Instances
--@subsection Unit

> instance NFData ()

-----------------------------------------------------------------------------
			Lists						    
----------------------------------------------------------------------------
--@node Lists, Arrays, Unit, Strategy Instances
--@subsection Lists

> instance NFData a => NFData [a] where
>   rnf [] = ()
>   rnf (x:xs) = rnf x `seq` rnf xs

--@menu
--* Parallel Strategies for Lists::  
--* Sequential Strategies for Lists::  
--@end menu

----------------------------------------------------------------------------
                        Lists: Parallel Strategies
----------------------------------------------------------------------------
--@node Parallel Strategies for Lists, Sequential Strategies for Lists, Lists, Lists
--@subsubsection Parallel Strategies for Lists

Applies a strategy to every element of a list in parallel

--@cindex parList

> parList :: Strategy a -> Strategy [a]
> parList strat []     = ()
> parList strat (x:xs) = strat x `par` (parList strat xs)

Applies a strategy to the first  n elements of a list  in parallel

--@cindex parListN

> parListN :: (Integral b) => b -> Strategy a -> Strategy [a]
> parListN n strat []     = ()
> parListN 0 strat xs     = ()
> parListN n strat (x:xs) = strat x `par` (parListN (n-1) strat xs)

Evaluates N elements of the spine of the argument list and applies
`strat' to the Nth element (if there is one) in parallel with the
result. e.g. parListNth 2 [e1, e2, e3] evaluates e2

--@cindex parListNth

> parListNth :: Int -> Strategy a -> Strategy [a]
> parListNth n strat xs 
>   | null rest = ()
>   | otherwise = strat (head rest) `par` ()
>   where
>     rest = drop n xs

parListChunk sequentially applies a strategy to chunks
(sub-sequences) of a list in parallel. Useful to increase grain size

--@cindex parListChunk

> parListChunk :: Int -> Strategy a -> Strategy [a]
> parListChunk n strat [] = ()
> parListChunk n strat xs = seqListN n strat xs `par` 
> 			    parListChunk n strat (drop n xs)

parMap applies a function to each element of the argument list in
parallel.  The result of the function is evaluated using `strat'

--@cindex parMap

> parMap :: Strategy b -> (a -> b) -> [a] -> [b]
> parMap strat f xs 	= map f xs `using` parList strat

parFlatMap uses parMap to apply a list-valued function to each
element of the argument list in parallel.  The result of the function
is evaluated using `strat'

--@cindex parFlatMap

> parFlatMap :: Strategy [b] -> (a -> [b]) -> [a] -> [b]
> parFlatMap strat f xs = concat (parMap strat f xs)

parZipWith zips together two lists with a function z in parallel

--@cindex parZipWith

> parZipWith :: Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
> parZipWith strat z as bs = 
>   zipWith z as bs `using` parList strat

----------------------------------------------------------------------------
                        Lists: Sequential Strategies
----------------------------------------------------------------------------
--@node Sequential Strategies for Lists,  , Parallel Strategies for Lists, Lists
--@subsubsection Sequential Strategies for Lists

Sequentially applies a strategy to each element of a list

--@cindex seqList

> seqList :: Strategy a -> Strategy [a]
> seqList strat []     = ()
> seqList strat (x:xs) = strat x `seq` (seqList strat xs)

Sequentially applies a strategy to the first  n elements of a list

--@cindex seqListN

> seqListN :: (Integral a) => a -> Strategy b -> Strategy [b]
> seqListN n strat []     = ()
> seqListN 0 strat xs     = ()
> seqListN n strat (x:xs) = strat x `seq` (seqListN (n-1) strat xs)

seqListNth applies a strategy to the Nth element of it's argument
(if there is one) before returning the result. e.g. seqListNth 2 [e1,
e2, e3] evaluates e2

--@cindex seqListNth

>#if (__HASKELL1__>=4)
> seqListNth :: Int -> Strategy b -> Strategy [b]
>#else
> seqListNth :: (Integral a) => a -> Strategy b -> Strategy [b]
>#endif
> seqListNth n strat xs 
>   | null rest = ()
>   | otherwise = strat (head rest) 
>   where
>     rest = drop n xs

Parallel n-buffer function added for the revised version of the strategies
paper. @parBuffer@ supersedes the older @fringeList@. It has the same
semantics.

--@cindex parBuffer

> parBuffer :: Int -> Strategy a -> [a] -> [a]
> parBuffer n s xs = 
>   return xs (start n xs)
>   where
>     return (x:xs) (y:ys) = (x:return xs ys) `sparking` s y
>     return xs     []     = xs
>
>     start n []     = []
>     start 0 ys     = ys
>     start n (y:ys) = start (n-1) ys `sparking` s y

fringeList implements a `rolling buffer' of length n, i.e.applies a
strategy to the nth element of list when the head is demanded. More
precisely:

   semantics:         fringeList n s = id :: [b] -> [b]
   dynamic behaviour: evalutates the nth element of the list when the
		      head is demanded.
   
The idea is to provide a `rolling buffer' of length n.

--@cindex fringeList

<> fringeList :: (Integral a) => a -> Strategy b -> [b] -> [b]
<> fringeList n strat [] = []
<> fringeList n strat (r:rs) = 
<>   seqListNth n strat rs `par`
<>   r:fringeList n strat rs

------------------------------------------------------------------------------
			Arrays
------------------------------------------------------------------------------
--@node Arrays,  , Lists, Strategy Instances
--@subsection Arrays

> instance (Ix a, NFData a, NFData b) => NFData (Array a b) where
>   rnf x = rnf (bounds x) `seq` seqList rnf (elems x) `seq` ()

Apply a strategy to all elements of an array in parallel. This can be done 
either in sequentially or in parallel (same as with lists, really).

> seqArr :: (Ix b) => Strategy a -> Strategy (Array b a)
> seqArr s arr = seqList s (elems arr)

> parArr :: (Ix b) => Strategy a -> Strategy (Array b a)
> parArr s arr = parList s (elems arr)

Associations maybe useful even withou mentioning Arrays.

See: .../lib/prelude/TyArrays.hs:
data  Assoc a b =  a := b  deriving ()

>#if (__HASKELL1__<4)
> instance (NFData a, NFData b) => NFData (Assoc a b) where
>   rnf (x := y) = rnf x `seq` rnf y `seq` ()
>#endif

------------------------------------------------------------------------------
	                Some strategies specific for Lolita	
------------------------------------------------------------------------------
--@node Lolita-specific Strategies, Index, Strategy Instances, Evaluation Strategies
--@section Lolita-specific Strategies

The following is useful in mergePenGroups

--@cindex fstPairFstList

> fstPairFstList :: (NFData a) => Strategy [(a,b)]
> fstPairFstList = seqListN 1 (seqPair rwhnf r0)

Some HACKs for Lolita. AFAIK force is just another name for our rnf and
sforce is a shortcut (definition here is identical to the one in Force.lhs)

> force :: (NFData a) => a -> a 
> sforce :: (NFData a) => a -> b -> b

Same as definition below

<> force x = rnf x `seq` x

> force = id $| rnf
>#if (__HASKELL1__>=4)
> sforce x y = force x `seq` y
>#else
> sforce x y = force x `seq_from_Parallel` y
>#endif

--@node Bowing-alg specific strategies
--@section Bowing-alg specific strategies

NB: this strategy currently needs the quicksort implementation from the hbc syslib 

>#if defined(PAR_GRAN_LIST)
> parGranList :: Strategy a -> (a -> Int) -> [a] -> Strategy [a]
> parGranList s gran_estim l_in = \ l_out ->
>   parListByIdx s l_out $
>   sortedIdx gran_list (sortLe ( \ (i,_) (j,_) -> i>j) gran_list)
>   where -- spark list elems of l in the order specified by  (i:idxs)
>	  parListByIdx s l [] = ()
>	  parListByIdx s l (i:idxs) = parListByIdx s l idxs `sparking` s (l!!i)
>	  -- get the index of y in the list
>	  idx y [] = error "idx: x not in l"
>	  idx y ((x,_):xs) | y==x      = 0
>			  | otherwise = (idx y xs)+1
>	  -- the `schedule' for sparking: list of indices of sorted input list
>	  sortedIdx l idxs = [ idx x l | (x,_) <- idxs ]
>	  -- add granularity info to elems of the input list
>	  gran_list = map (\ l -> (gran_estim l, l)) l_in  
>#endif

--@node Index,  , Lolita-specific Strategies, Evaluation Strategies
--@section Index

--@index
--* $|::  @cindex\s-+$|
--* $||::  @cindex\s-+$||
--* -|::  @cindex\s-+-|
--* -||::  @cindex\s-+-||
--* .|::  @cindex\s-+.|
--* .||::  @cindex\s-+.||
--* NFData::  @cindex\s-+NFData
--* Strategy::  @cindex\s-+Strategy
--* demanding::  @cindex\s-+demanding
--* fringeList::  @cindex\s-+fringeList
--* fstPairFstList::  @cindex\s-+fstPairFstList
--* markStrat::  @cindex\s-+markStrat
--* parBuffer::  @cindex\s-+parBuffer
--* parFlatMap::  @cindex\s-+parFlatMap
--* parList::  @cindex\s-+parList
--* parListChunk::  @cindex\s-+parListChunk
--* parListN::  @cindex\s-+parListN
--* parListNth::  @cindex\s-+parListNth
--* parMap::  @cindex\s-+parMap
--* parPair::  @cindex\s-+parPair
--* parTriple::  @cindex\s-+parTriple
--* parZipWith::  @cindex\s-+parZipWith
--* r0::  @cindex\s-+r0
--* rnf::  @cindex\s-+rnf
--* rwhnf::  @cindex\s-+rwhnf
--* seqList::  @cindex\s-+seqList
--* seqListN::  @cindex\s-+seqListN
--* seqListNth::  @cindex\s-+seqListNth
--* seqPair::  @cindex\s-+seqPair
--* seqTriple::  @cindex\s-+seqTriple
--* sparking::  @cindex\s-+sparking
--* using::  @cindex\s-+using
--@end index
