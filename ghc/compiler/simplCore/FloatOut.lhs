%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
module FloatOut ( floatOutwards ) where

#include "HsVersions.h"

import CoreSyn

import CmdLineOpts	( opt_D_verbose_core2core, opt_D_simplifier_stats )
import ErrUtils		( dumpIfSet )
import CostCentre	( dupifyCC, CostCentre )
import Id		( Id )
import Const		( isWHNFCon )
import VarEnv
import CoreLint		( beginPass, endPass )
import PprCore
import SetLevels	( setLevels,
		 	  Level(..), tOP_LEVEL, ltMajLvl, ltLvl, isTopLvl
			)
import BasicTypes	( Unused )
import Var		( TyVar )
import UniqSupply       ( UniqSupply )
import List		( partition )
import Outputable
\end{code}

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

\begin{code}
type LevelledExpr  = TaggedExpr Level
type LevelledBind  = TaggedBind Level
type FloatBind     = (Level, CoreBind)
type FloatBinds    = [FloatBind]
\end{code}

%************************************************************************
%*									*
\subsection[floatOutwards]{@floatOutwards@: let-floating interface function}
%*									*
%************************************************************************

\begin{code}
floatOutwards :: UniqSupply -> [CoreBind] -> IO [CoreBind]

floatOutwards us pgm
  = do {
	beginPass "Float out";

	let { annotated_w_levels = setLevels pgm us ;
	      (fss, binds_s')    = unzip (map floatTopBind annotated_w_levels)
	    } ;

	dumpIfSet opt_D_verbose_core2core "Levels added:"
	          (vcat (map ppr annotated_w_levels));

	let { (tlets, ntlets, lams) = get_stats (sum_stats fss) };

	dumpIfSet opt_D_simplifier_stats "FloatOut stats:"
		(hcat [	int tlets,  ptext SLIT(" Lets floated to top level; "),
			int ntlets, ptext SLIT(" Lets floated elsewhere; from "),
			int lams,   ptext SLIT(" Lambda groups")]);

	endPass "Float out" 
	 	opt_D_verbose_core2core		{- no specific flag for dumping float-out -} 
		(concat binds_s')
    }

floatTopBind bind@(NonRec _ _)
  = case (floatBind emptyVarEnv tOP_LEVEL bind) of { (fs, floats, bind', _) ->
    (fs, floatsToBinds floats ++ [bind'])
    }

floatTopBind bind@(Rec _)
  = case (floatBind emptyVarEnv tOP_LEVEL bind) of { (fs, floats, Rec pairs', _) ->
	-- Actually floats will be empty
    --false:ASSERT(null floats)
    (fs, [Rec (floatsToBindPairs floats ++ pairs')])
    }
\end{code}

%************************************************************************
%*									*
\subsection[FloatOut-Bind]{Floating in a binding (the business end)}
%*									*
%************************************************************************


\begin{code}
floatBind :: IdEnv Level
	  -> Level
	  -> LevelledBind
	  -> (FloatStats, FloatBinds, CoreBind, IdEnv Level)

floatBind env lvl (NonRec (name,level) rhs)
  = case (floatExpr env level rhs) of { (fs, rhs_floats, rhs') ->

	-- A good dumping point
    case (partitionByMajorLevel level rhs_floats) of { (rhs_floats', heres) ->

    (fs, rhs_floats',
     NonRec name (install heres rhs'),
     extendVarEnv env name level)
    }}

floatBind env lvl bind@(Rec pairs)
  = case (unzip3 (map do_pair pairs)) of { (fss, rhss_floats, new_pairs) ->

    if not (isTopLvl bind_level) then
	-- Standard case
	(sum_stats fss, concat rhss_floats, Rec new_pairs, new_env)
    else
	{- In a recursive binding, destined for the top level (only),
	   the rhs floats may contain
	   references to the bound things.  For example

		f = ...(let v = ...f... in b) ...

	   might get floated to

		v = ...f...
		f = ... b ...

	   and hence we must (pessimistically) make all the floats recursive
	   with the top binding.  Later dependency analysis will unravel it.
	-}

	(sum_stats fss,
	 [],
	 Rec (new_pairs ++ floatsToBindPairs (concat rhss_floats)),
	 new_env)

    }
  where
    new_env = extendVarEnvList env (map fst pairs)

    bind_level = getBindLevel bind

    do_pair ((name, level), rhs)
      = case (floatExpr new_env level rhs) of { (fs, rhs_floats, rhs') ->

		-- A good dumping point
	case (partitionByMajorLevel level rhs_floats) of { (rhs_floats', heres) ->

	(fs, rhs_floats', (name, install heres rhs'))
	}}
\end{code}

%************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
%*									*
%************************************************************************

\begin{code}
floatExpr :: IdEnv Level
	  -> Level
	  -> LevelledExpr
	  -> (FloatStats, FloatBinds, CoreExpr)

floatExpr env _ (Var v)	     = (zeroStats, [], Var v)
floatExpr env _ (Type ty)    = (zeroStats, [], Type ty)
floatExpr env lvl (Con con as) 
  = case floatList (floatExpr env lvl) as of { (stats, floats, as') ->
    (stats, floats, Con con as') }
	  
floatExpr env lvl (App e a)
  = case (floatExpr env lvl e) of { (fse, floats_e, e') ->
    case (floatExpr env lvl a) of { (fsa, floats_a, a') ->
    (fse `add_stats` fsa, floats_e ++ floats_a, App e' a') }}

floatExpr env lvl (Lam (tv,incd_lvl) e)
  | isTyVar tv
  = case (floatExpr env incd_lvl e) of { (fs, floats, e') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (fs, floats', Lam tv (install heres e'))
    }}

floatExpr env lvl (Lam (arg,incd_lvl) rhs)
  = ASSERT( isId arg )
    let
	new_env  = extendVarEnv env arg incd_lvl
    in
    case (floatExpr new_env incd_lvl rhs) of { (fs, floats, rhs') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (add_to_stats fs floats',
     floats',
     Lam arg (install heres rhs'))
    }}

floatExpr env lvl (Note note@(SCC cc) expr)
  = case (floatExpr env lvl expr)    of { (fs, floating_defns, expr') ->
    let
	-- Annotate bindings floated outwards past an scc expression
	-- with the cc.  We mark that cc as "duplicated", though.

	annotated_defns = annotate (dupifyCC cc) floating_defns
    in
    (fs, annotated_defns, Note note expr') }
  where
    annotate :: CostCentre -> FloatBinds -> FloatBinds

    annotate dupd_cc defn_groups
      = [ (level, ann_bind floater) | (level, floater) <- defn_groups ]
      where
	ann_bind (NonRec binder rhs)
	  = NonRec binder (ann_rhs rhs)

	ann_bind (Rec pairs)
	  = Rec [(binder, ann_rhs rhs) | (binder, rhs) <- pairs]

	ann_rhs (Lam arg e)     = Lam arg (ann_rhs e)
	ann_rhs rhs@(Con con _) | isWHNFCon con = rhs	-- no point in scc'ing WHNF data
	ann_rhs rhs	        = Note (SCC dupd_cc) rhs

	-- Note: Nested SCC's are preserved for the benefit of
	--       cost centre stack profiling (Durham)

floatExpr env lvl (Note note expr)	-- Other than SCCs
  = case (floatExpr env lvl expr)    of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Note note expr') }

floatExpr env lvl (Let bind body)
  = case (floatBind env     lvl bind) of { (fsb, rhs_floats, bind', new_env) ->
    case (floatExpr new_env lvl body) of { (fse, body_floats, body') ->
    (add_stats fsb fse,
     rhs_floats ++ [(bind_lvl, bind')] ++ body_floats,
     body')
    }}
  where
    bind_lvl = getBindLevel bind

floatExpr env lvl (Case scrut (case_bndr, case_lvl) alts)
  = case floatExpr env lvl scrut	of { (fse, fde, scrut') ->
    case floatList float_alt alts	of { (fsa, fda, alts')  ->
    (add_stats fse fsa, fda ++ fde, Case scrut' case_bndr alts')
    }}
  where
      alts_env = extendVarEnv env case_bndr case_lvl

      partition_fn = partitionByMajorLevel

      float_alt (con, bs, rhs)
	= let
	      bs' = map fst bs
	      new_env = extendVarEnvList alts_env bs
	  in
	  case (floatExpr new_env case_lvl rhs)		of { (fs, rhs_floats, rhs') ->
	  case (partition_fn case_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', (con, bs', install heres rhs')) }}


floatList :: (a -> (FloatStats, FloatBinds, b)) -> [a] -> (FloatStats, FloatBinds, [b])
floatList f [] = (zeroStats, [], [])
floatList f (a:as) = case f a		 of { (fs_a,  binds_a,  b)  ->
		     case floatList f as of { (fs_as, binds_as, bs) ->
		     (fs_a `add_stats` fs_as, binds_a ++ binds_as, b:bs) }}
\end{code}

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

get_stats (FlS a b c) = (a, b, c)

zeroStats = FlS 0 0 0

sum_stats xs = foldr add_stats zeroStats xs

add_stats (FlS a1 b1 c1) (FlS a2 b2 c2)
  = FlS (a1 + a2) (b1 + b2) (c1 + c2)

add_to_stats (FlS a b c) floats
  = FlS (a + length top_floats) (b + length other_floats) (c + 1)
  where
    (top_floats, other_floats) = partition to_very_top floats

    to_very_top (my_lvl, _) = isTopLvl my_lvl
\end{code}


%************************************************************************
%*									*
\subsection{Utility bits for floating}
%*									*
%************************************************************************

\begin{code}
getBindLevel (NonRec (_, lvl) _)      = lvl
getBindLevel (Rec (((_,lvl), _) : _)) = lvl
\end{code}

\begin{code}
partitionByMajorLevel, partitionByLevel
	:: Level		-- Partitioning level

	-> FloatBinds   	-- Defns to be divided into 2 piles...

	-> (FloatBinds,	-- Defns  with level strictly < partition level,
	    FloatBinds)	-- The rest


partitionByMajorLevel ctxt_lvl defns
  = partition float_further defns
  where
    float_further (my_lvl, _) = my_lvl `ltMajLvl` ctxt_lvl ||
				isTopLvl my_lvl

partitionByLevel ctxt_lvl defns
  = partition float_further defns
  where
    float_further (my_lvl, _) = my_lvl `ltLvl` ctxt_lvl
\end{code}

\begin{code}
floatsToBinds :: FloatBinds -> [CoreBind]
floatsToBinds floats = map snd floats

floatsToBindPairs :: FloatBinds -> [(Id,CoreExpr)]

floatsToBindPairs floats = concat (map mk_pairs floats)
  where
   mk_pairs (_, Rec pairs)         = pairs
   mk_pairs (_, NonRec binder rhs) = [(binder,rhs)]

install :: FloatBinds -> CoreExpr -> CoreExpr

install defn_groups expr
  = foldr install_group expr defn_groups
  where
    install_group (_, defns) body = Let defns body
\end{code}
