%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
module FloatOut ( floatOutwards ) where

import CoreSyn
import CoreUtils
import CoreArity	( etaExpand )
import CoreMonad	( FloatOutSwitches(..) )

import DynFlags		( DynFlags, DynFlag(..) )
import ErrUtils		( dumpIfSet_dyn )
import CostCentre	( dupifyCC, CostCentre )
import Id		( Id, idType, idArity, isBottomingId )
import Type		( isUnLiftedType )
import SetLevels	( Level(..), LevelledExpr, LevelledBind,
			  setLevels, isTopLvl )
import UniqSupply       ( UniqSupply )
import Bag
import Util
import Maybes
import Outputable
import FastString
import qualified Data.IntMap as M

#include "HsVersions.h"
\end{code}

	-----------------
	Overall game plan
	-----------------

The Big Main Idea is:

  	To float out sub-expressions that can thereby get outside
	a non-one-shot value lambda, and hence may be shared.


To achieve this we may need to do two thing:

   a) Let-bind the sub-expression:

	f (g x)  ==>  let lvl = f (g x) in lvl

      Now we can float the binding for 'lvl'.  

   b) More than that, we may need to abstract wrt a type variable

	\x -> ... /\a -> let v = ...a... in ....

      Here the binding for v mentions 'a' but not 'x'.  So we
      abstract wrt 'a', to give this binding for 'v':

	    vp = /\a -> ...a...
	    v  = vp a

      Now the binding for vp can float out unimpeded.
      I can't remember why this case seemed important enough to
      deal with, but I certainly found cases where important floats
      didn't happen if we did not abstract wrt tyvars.

With this in mind we can also achieve another goal: lambda lifting.
We can make an arbitrary (function) binding float to top level by
abstracting wrt *all* local variables, not just type variables, leaving
a binding that can be floated right to top level.  Whether or not this
happens is controlled by a flag.


Random comments
~~~~~~~~~~~~~~~

At the moment we never float a binding out to between two adjacent
lambdas.  For example:

@
	\x y -> let t = x+x in ...
===>
	\x -> let t = x+x in \y -> ...
@
Reason: this is less efficient in the case where the original lambda
is never partially applied.

But there's a case I've seen where this might not be true.  Consider:
@
elEm2 x ys
  = elem' x ys
  where
    elem' _ []	= False
    elem' x (y:ys)	= x==y || elem' x ys
@
It turns out that this generates a subexpression of the form
@
	\deq x ys -> let eq = eqFromEqDict deq in ...
@
vwhich might usefully be separated to
@
	\deq -> let eq = eqFromEqDict deq in \xy -> ...
@
Well, maybe.  We don't do this at the moment.


%************************************************************************
%*									*
\subsection[floatOutwards]{@floatOutwards@: let-floating interface function}
%*									*
%************************************************************************

\begin{code}
floatOutwards :: FloatOutSwitches
	      -> DynFlags
	      -> UniqSupply 
	      -> [CoreBind] -> IO [CoreBind]

floatOutwards float_sws dflags us pgm
  = do {
	let { annotated_w_levels = setLevels float_sws pgm us ;
	      (fss, binds_s')    = unzip (map floatTopBind annotated_w_levels)
	    } ;

	dumpIfSet_dyn dflags Opt_D_verbose_core2core "Levels added:"
	          (vcat (map ppr annotated_w_levels));

	let { (tlets, ntlets, lams) = get_stats (sum_stats fss) };

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats "FloatOut stats:"
		(hcat [	int tlets,  ptext (sLit " Lets floated to top level; "),
			int ntlets, ptext (sLit " Lets floated elsewhere; from "),
			int lams,   ptext (sLit " Lambda groups")]);

	return (concat binds_s')
    }

floatTopBind :: LevelledBind -> (FloatStats, [CoreBind])
floatTopBind bind
  = case (floatBind bind) of { (fs, floats) ->
    (fs, bagToList (flattenFloats floats)) }
\end{code}

%************************************************************************
%*									*
\subsection[FloatOut-Bind]{Floating in a binding (the business end)}
%*									*
%************************************************************************

\begin{code}
floatBind :: LevelledBind -> (FloatStats, FloatBinds)
floatBind (NonRec (TB var level) rhs)
  = case (floatRhs level rhs) of { (fs, rhs_floats, rhs') ->

	-- A tiresome hack: 
	-- see Note [Bottoming floats: eta expansion] in SetLevels
    let rhs'' | isBottomingId var = etaExpand (idArity var) rhs'
	      | otherwise         = rhs'

    in (fs, rhs_floats `plusFloats` unitFloat level (NonRec var rhs'')) }

floatBind (Rec pairs)
  = case floatList do_pair pairs of { (fs, rhs_floats, new_pairs) ->
        -- NB: the rhs floats may contain references to the 
	-- bound things.  For example
	--	f = ...(let v = ...f... in b) ...
    if not (isTopLvl dest_lvl) then
	-- Find which bindings float out at least one lambda beyond this one
	-- These ones can't mention the binders, because they couldn't 
	-- be escaping a major level if so.
	-- The ones that are not going further can join the letrec;
	-- they may not be mutually recursive but the occurrence analyser will
	-- find that out. In our example we make a Rec thus:
	--	v = ...f...
	--	f = ... b ...
	case (partitionByMajorLevel dest_lvl rhs_floats) of { (floats', heres) ->
	(fs, floats' `plusFloats` unitFloat dest_lvl 
	         (Rec (floatsToBindPairs heres new_pairs))) }
    else
	-- For top level, no need to partition; just make them all recursive
	-- (And the partition wouldn't work because they'd all end up in floats')
	(fs, unitFloat dest_lvl
	         (Rec (floatsToBindPairs (flattenFloats rhs_floats) new_pairs)))  }
  where
    (((TB _ dest_lvl), _) : _) = pairs

    do_pair (TB name level, rhs)
      = case (floatRhs level rhs) of { (fs, rhs_floats, rhs') ->
	(fs, rhs_floats, (name, rhs')) }

---------------
floatList :: (a -> (FloatStats, FloatBinds, b)) -> [a] -> (FloatStats, FloatBinds, [b])
floatList _ [] = (zeroStats, emptyFloats, [])
floatList f (a:as) = case f a		 of { (fs_a,  binds_a,  b)  ->
		     case floatList f as of { (fs_as, binds_as, bs) ->
		     (fs_a `add_stats` fs_as, binds_a `plusFloats`  binds_as, b:bs) }}
\end{code}


%************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
%*									*
%************************************************************************

\begin{code}
floatExpr, floatRhs, floatCaseAlt
	 :: Level
	 -> LevelledExpr
	 -> (FloatStats, FloatBinds, CoreExpr)

floatCaseAlt lvl arg	-- Used rec rhss, and case-alternative rhss
  = case (floatExpr lvl arg) of { (fsa, floats, arg') ->
    case (partitionByMajorLevel lvl floats) of { (floats', heres) ->
	-- Dump bindings that aren't going to escape from a lambda;
	-- in particular, we must dump the ones that are bound by 
	-- the rec or case alternative
    (fsa, floats', install heres arg') }}

-----------------
floatRhs lvl arg	-- Used for nested non-rec rhss, and fn args
			-- See Note [Floating out of RHS]
  = floatExpr lvl arg

-----------------
floatExpr _ (Var v)   = (zeroStats, emptyFloats, Var v)
floatExpr _ (Type ty) = (zeroStats, emptyFloats, Type ty)
floatExpr _ (Coercion co) = (zeroStats, emptyFloats, Coercion co)
floatExpr _ (Lit lit) = (zeroStats, emptyFloats, Lit lit)
	  
floatExpr lvl (App e a)
  = case (floatExpr      lvl e) of { (fse, floats_e, e') ->
    case (floatRhs lvl a) 	of { (fsa, floats_a, a') ->
    (fse `add_stats` fsa, floats_e `plusFloats` floats_a, App e' a') }}

floatExpr _ lam@(Lam (TB _ lam_lvl) _)
  = let (bndrs_w_lvls, body) = collectBinders lam
	bndrs		     = [b | TB b _ <- bndrs_w_lvls]
	-- All the binders have the same level
	-- See SetLevels.lvlLamBndrs
    in
    case (floatExpr lam_lvl body) of { (fs, floats, body1) ->

        -- Dump anything that is captured by this lambda
	-- Eg  \x -> ...(\y -> let v = <blah> in ...)...
	-- We'll have the binding (v = <blah>) in the floats,
	-- but must dump it at the lambda-x
    case (partitionByLevel lam_lvl floats)	of { (floats1, heres) ->
    (add_to_stats fs floats1, floats1, mkLams bndrs (install heres body1))
    }}

floatExpr lvl (Note note@(SCC cc) expr)
  = case (floatExpr lvl expr)    of { (fs, floating_defns, expr') ->
    let
	-- Annotate bindings floated outwards past an scc expression
	-- with the cc.  We mark that cc as "duplicated", though.

	annotated_defns = wrapCostCentre (dupifyCC cc) floating_defns
    in
    (fs, annotated_defns, Note note expr') }

floatExpr lvl (Note note expr)	-- Other than SCCs
  = case (floatExpr lvl expr)    of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Note note expr') }

floatExpr lvl (Cast expr co)
  = case (floatExpr lvl expr)	of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Cast expr' co) }

floatExpr lvl (Let (NonRec (TB bndr bndr_lvl) rhs) body)
  | isUnLiftedType (idType bndr)  -- Treat unlifted lets just like a case
				  -- I.e. floatExpr for rhs, floatCaseAlt for body
  = case floatExpr lvl rhs	    of { (_, rhs_floats, rhs') ->
    case floatCaseAlt bndr_lvl body of { (fs, body_floats, body') ->
    (fs, rhs_floats `plusFloats` body_floats, Let (NonRec bndr rhs') body') }}

floatExpr lvl (Let bind body)
  = case (floatBind bind)     of { (fsb, bind_floats) ->
    case (floatExpr lvl body) of { (fse, body_floats, body') ->
    case partitionByMajorLevel lvl (bind_floats `plusFloats` body_floats) 
                              of { (floats, heres) ->
	-- See Note [Avoiding unnecessary floating]
    (add_stats fsb fse, floats, install heres body')  } } }

floatExpr lvl (Case scrut (TB case_bndr case_lvl) ty alts)
  = case floatExpr lvl scrut	of { (fse, fde, scrut') ->
    case floatList float_alt alts	of { (fsa, fda, alts')  ->
    (add_stats fse fsa, fda `plusFloats` fde, Case scrut' case_bndr ty alts')
    }}
  where
	-- Use floatCaseAlt for the alternatives, so that we
	-- don't gratuitiously float bindings out of the RHSs
    float_alt (con, bs, rhs)
	= case (floatCaseAlt case_lvl rhs)	of { (fs, rhs_floats, rhs') ->
	  (fs, rhs_floats, (con, [b | TB b _ <- bs], rhs')) }
\end{code}

Note [Avoiding unnecessary floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we want to avoid floating a let unnecessarily, because
it might worsen strictness:
    let 
       x = ...(let y = e in y+y)....
Here y is demanded.  If we float it outside the lazy 'x=..' then
we'd have to zap its demand info, and it may never be restored.

So at a 'let' we leave the binding right where the are unless
the binding will escape a value lambda.  That's what the 
partitionByMajorLevel does in the floatExpr (Let ...) case.

Notice, though, that we must take care to drop any bindings
from the body of the let that depend on the staying-put bindings.

We used instead to do the partitionByMajorLevel on the RHS of an '=',
in floatRhs.  But that was quite tiresome.  We needed to test for
values or trival rhss, because (in particular) we don't want to insert
new bindings between the "=" and the "\".  E.g.
	f = \x -> let <bind> in <body>
We do not want
	f = let <bind> in \x -> <body>
(a) The simplifier will immediately float it further out, so we may
	as well do so right now; in general, keeping rhss as manifest 
	values is good
(b) If a float-in pass follows immediately, it might add yet more
	bindings just after the '='.  And some of them might (correctly)
	be strict even though the 'let f' is lazy, because f, being a value,
	gets its demand-info zapped by the simplifier.
And even all that turned out to be very fragile, and broke
altogether when profiling got in the way.

So now we do the partition right at the (Let..) itself.

%************************************************************************
%*									*
\subsection{Utility bits for floating stats}
%*									*
%************************************************************************

I didn't implement this with unboxed numbers.  I don't want to be too
strict in this stuff, as it is rarely turned on.  (WDP 95/09)

\begin{code}
data FloatStats
  = FlS	Int  -- Number of top-floats * lambda groups they've been past
	Int  -- Number of non-top-floats * lambda groups they've been past
	Int  -- Number of lambda (groups) seen

get_stats :: FloatStats -> (Int, Int, Int)
get_stats (FlS a b c) = (a, b, c)

zeroStats :: FloatStats
zeroStats = FlS 0 0 0

sum_stats :: [FloatStats] -> FloatStats
sum_stats xs = foldr add_stats zeroStats xs

add_stats :: FloatStats -> FloatStats -> FloatStats
add_stats (FlS a1 b1 c1) (FlS a2 b2 c2)
  = FlS (a1 + a2) (b1 + b2) (c1 + c2)

add_to_stats :: FloatStats -> FloatBinds -> FloatStats
add_to_stats (FlS a b c) (FB tops others)
  = FlS (a + lengthBag tops) (b + lengthBag (flattenMajor others)) (c + 1)
\end{code}


%************************************************************************
%*									*
\subsection{Utility bits for floating}
%*									*
%************************************************************************

Note [Representation of FloatBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatBinds types is somewhat important.  We can get very large numbers
of floating bindings, often all destined for the top level.  A typical example
is     x = [4,2,5,2,5, .... ]
Then we get lots of small expressions like (fromInteger 4), which all get
lifted to top level.  

The trouble is that  
  (a) we partition these floating bindings *at every binding site* 
  (b) SetLevels introduces a new bindings site for every float
So we had better not look at each binding at each binding site!

That is why MajorEnv is represented as a finite map.

We keep the bindings destined for the *top* level separate, because
we float them out even if they don't escape a *value* lambda; see
partitionByMajorLevel.


\begin{code}
type FloatBind = CoreBind	-- INVARIANT: a FloatBind is always lifted

data FloatBinds  = FB !(Bag FloatBind)	   	-- Destined for top level
     		      !MajorEnv			-- Levels other than top
     -- See Note [Representation of FloatBinds]

instance Outputable FloatBinds where
  ppr (FB fbs env) = ptext (sLit "FB") <+> (braces $ vcat
                       [ ptext (sLit "binds =") <+> ppr fbs
                       , ptext (sLit "env =") <+> ppr env ])

type MajorEnv = M.IntMap MinorEnv			-- Keyed by major level
type MinorEnv = M.IntMap (Bag FloatBind)		-- Keyed by minor level

flattenFloats :: FloatBinds -> Bag FloatBind
flattenFloats (FB tops others) = tops `unionBags` flattenMajor others

flattenMajor :: MajorEnv -> Bag FloatBind
flattenMajor = M.fold (unionBags . flattenMinor) emptyBag

flattenMinor :: MinorEnv -> Bag FloatBind
flattenMinor = M.fold unionBags emptyBag

emptyFloats :: FloatBinds
emptyFloats = FB emptyBag M.empty

unitFloat :: Level -> FloatBind -> FloatBinds
unitFloat lvl@(Level major minor) b 
  | isTopLvl lvl = FB (unitBag b) M.empty
  | otherwise    = FB emptyBag (M.singleton major (M.singleton minor (unitBag b)))

plusFloats :: FloatBinds -> FloatBinds -> FloatBinds
plusFloats (FB t1 b1) (FB t2 b2) = FB (t1 `unionBags` t2) (b1 `plusMajor` b2)

plusMajor :: MajorEnv -> MajorEnv -> MajorEnv
plusMajor = M.unionWith plusMinor

plusMinor :: MinorEnv -> MinorEnv -> MinorEnv
plusMinor = M.unionWith unionBags

floatsToBindPairs :: Bag FloatBind -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
floatsToBindPairs floats binds = foldrBag add binds floats
  where
   add (Rec pairs)         binds = pairs ++ binds
   add (NonRec binder rhs) binds = (binder,rhs) : binds

install :: Bag FloatBind -> CoreExpr -> CoreExpr
install defn_groups expr
  = foldrBag install_group expr defn_groups
  where
    install_group defns body = Let defns body

partitionByMajorLevel, partitionByLevel
	:: Level		-- Partitioning level
	-> FloatBinds   	-- Defns to be divided into 2 piles...
	-> (FloatBinds,		-- Defns  with level strictly < partition level,
	    Bag FloatBind)	-- The rest

-- 	 ---- partitionByMajorLevel ----
-- Float it if we escape a value lambda, *or* if we get to the top level
-- If we can get to the top level, say "yes" anyway. This means that 
--	x = f e
-- transforms to 
--    lvl = e
--    x = f lvl
-- which is as it should be

partitionByMajorLevel (Level major _) (FB tops defns)
  = (FB tops outer, heres `unionBags` flattenMajor inner)
  where
    (outer, mb_heres, inner) = M.splitLookup major defns
    heres = case mb_heres of 
               Nothing -> emptyBag
               Just h  -> flattenMinor h

partitionByLevel (Level major minor) (FB tops defns)
  = (FB tops (outer_maj `plusMajor` M.singleton major outer_min),
     here_min `unionBags` flattenMinor inner_min 
              `unionBags` flattenMajor inner_maj)

  where
    (outer_maj, mb_here_maj, inner_maj) = M.splitLookup major defns
    (outer_min, mb_here_min, inner_min) = case mb_here_maj of
                                            Nothing -> (M.empty, Nothing, M.empty)
                                            Just min_defns -> M.splitLookup minor min_defns
    here_min = mb_here_min `orElse` emptyBag

wrapCostCentre :: CostCentre -> FloatBinds -> FloatBinds
wrapCostCentre cc (FB tops defns)
  = FB (wrap_defns tops) (M.map (M.map wrap_defns) defns)
  where
    wrap_defns = mapBag wrap_one 
    wrap_one (NonRec binder rhs) = NonRec binder (mkSCC cc rhs)
    wrap_one (Rec pairs)         = Rec (mapSnd (mkSCC cc) pairs)
\end{code}
