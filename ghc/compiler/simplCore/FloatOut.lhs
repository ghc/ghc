%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
#include "HsVersions.h"

module FloatOut ( floatOutwards ) where

import Ubiq{-uitous-}

import CoreSyn

import CmdLineOpts	( opt_D_verbose_core2core, opt_D_simplifier_stats )
import CostCentre	( dupifyCC )
import Id		( nullIdEnv, addOneToIdEnv, growIdEnvList, IdEnv(..),
			  GenId{-instance Outputable-}
			)
import Outputable	( Outputable(..){-instance (,)-} )
import PprCore		( GenCoreBinding{-instance-} )
import PprStyle		( PprStyle(..) )
import PprType		-- too lazy to type in all the instances
import Pretty		( ppInt, ppStr, ppBesides, ppAboves )
import SetLevels	-- all of it
import TyVar		( GenTyVar{-instance Eq-} )
import Unique		( Unique{-instance Eq-} )
import Usage		( UVar(..) )
import Util		( pprTrace, panic )
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
which might usefully be separated to
@
	\deq -> let eq = eqFromEqDict deq in \xy -> ...
@
Well, maybe.  We don't do this at the moment.

\begin{code}
type LevelledExpr  = GenCoreExpr    (Id, Level) Id TyVar UVar
type LevelledBind  = GenCoreBinding (Id, Level) Id TyVar UVar
type FloatingBind  = (Level, Floater)
type FloatingBinds = [FloatingBind]

data Floater
  = LetFloater	CoreBinding
  | CaseFloater	(CoreExpr -> CoreExpr)
		-- A CoreExpr with a hole in it:
		-- "Give me a right-hand side of the
		-- (usually single) alternative, and
		-- I'll build the case..."
\end{code}

%************************************************************************
%*									*
\subsection[floatOutwards]{@floatOutwards@: let-floating interface function}
%*									*
%************************************************************************

\begin{code}
floatOutwards :: UniqSupply -> [CoreBinding] -> [CoreBinding]

floatOutwards us pgm
  = case (setLevels pgm us) of { annotated_w_levels ->

    case (unzip (map floatTopBind annotated_w_levels))
		of { (fss, final_toplev_binds_s) ->

    (if opt_D_verbose_core2core
     then pprTrace "Levels added:\n"
		   (ppAboves (map (ppr PprDebug) annotated_w_levels))
     else id
    )
    ( if not (opt_D_simplifier_stats) then
	 id
      else
	 let
	    (tlets, ntlets, lams) = get_stats (sum_stats fss)
	 in
	 pprTrace "FloatOut stats: " (ppBesides [
		ppInt tlets,  ppStr " Lets floated to top level; ",
		ppInt ntlets, ppStr " Lets floated elsewhere; from ",
		ppInt lams,   ppStr " Lambda groups"])
    )
    concat final_toplev_binds_s
    }}

floatTopBind bind@(NonRec _ _)
  = case (floatBind nullIdEnv tOP_LEVEL bind) of { (fs, floats, bind', _) ->
    (fs, floatsToBinds floats ++ [bind'])
    }

floatTopBind bind@(Rec _)
  = case (floatBind nullIdEnv tOP_LEVEL bind) of { (fs, floats, Rec pairs', _) ->
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
	  -> (FloatStats, FloatingBinds, CoreBinding, IdEnv Level)

floatBind env lvl (NonRec (name,level) rhs)
  = case (floatExpr env level rhs) of { (fs, rhs_floats, rhs') ->

	-- A good dumping point
    case (partitionByMajorLevel level rhs_floats) of { (rhs_floats', heres) ->

    (fs, rhs_floats',
     NonRec name (install heres rhs'),
     addOneToIdEnv env name level)
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
    new_env = growIdEnvList env (map fst pairs)

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
	  -> (FloatStats, FloatingBinds, CoreExpr)

floatExpr env _ (Var v)	     = (zero_stats, [], Var v)
floatExpr env _ (Lit l)      = (zero_stats, [], Lit l)
floatExpr env _ (Prim op as) = (zero_stats, [], Prim op as)
floatExpr env _ (Con con as) = (zero_stats, [], Con con as)
	  
floatExpr env lvl (App e a)
  = case (floatExpr env lvl e) of { (fs, floating_defns, e') ->
    (fs, floating_defns, App e' a) }

floatExpr env lvl (Lam (UsageBinder _) e)
  = panic "FloatOut.floatExpr: Lam UsageBinder"

floatExpr env lvl (Lam (TyBinder tv) e)
  = let
	incd_lvl = incMinorLvl lvl
    in
    case (floatExpr env incd_lvl e) of { (fs, floats, e') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (fs, floats', Lam (TyBinder tv) (install heres e'))
    }}

floatExpr env lvl (Lam (ValBinder (arg,incd_lvl)) rhs)
  = let
	new_env  = addOneToIdEnv env arg incd_lvl
    in
    case (floatExpr new_env incd_lvl rhs) of { (fs, floats, rhs') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (add_to_stats fs floats',
     floats',
     Lam (ValBinder arg) (install heres rhs'))
    }}

floatExpr env lvl (SCC cc expr)
  = case (floatExpr env lvl expr)    of { (fs, floating_defns, expr') ->
    let
	-- annotate bindings floated outwards past an scc expression
	-- with the cc.  We mark that cc as "duplicated", though.

	annotated_defns = annotate (dupifyCC cc) floating_defns
    in
    (fs, annotated_defns, SCC cc expr') }
  where
    annotate :: CostCentre -> FloatingBinds -> FloatingBinds

    annotate dupd_cc defn_groups
      = [ (level, ann_bind floater) | (level, floater) <- defn_groups ]
      where
	ann_bind (LetFloater (NonRec binder rhs))
	  = LetFloater (NonRec binder (ann_rhs rhs))

	ann_bind (LetFloater (Rec pairs))
	  = LetFloater (Rec [(binder, ann_rhs rhs) | (binder, rhs) <- pairs])

	ann_bind (CaseFloater fn) = CaseFloater ( \ rhs -> SCC dupd_cc (fn rhs) )

	ann_rhs (Lam arg e)   = Lam arg (ann_rhs e)
	ann_rhs rhs@(Con _ _) = rhs	-- no point in scc'ing WHNF data
	ann_rhs rhs	      = SCC dupd_cc rhs

	-- Note: Nested SCC's are preserved for the benefit of
	--       cost centre stack profiling (Durham)

floatExpr env lvl (Coerce c ty expr)
  = case (floatExpr env lvl expr)    of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Coerce c ty expr') }

floatExpr env lvl (Let bind body)
  = case (floatBind env     lvl bind) of { (fsb, rhs_floats, bind', new_env) ->
    case (floatExpr new_env lvl body) of { (fse, body_floats, body') ->
    (add_stats fsb fse,
     rhs_floats ++ [(bind_lvl, LetFloater bind')] ++ body_floats,
     body')
    }}
  where
    bind_lvl = getBindLevel bind

floatExpr env lvl (Case scrut alts)
  = case (floatExpr env lvl scrut) of { (fse, fde, scrut') ->

    case (scrut', float_alts alts) of
	(_, (fsa, fda, alts')) ->
		(add_stats fse fsa, fda ++ fde, Case scrut' alts')
    }
    {-	OLD CASE-FLOATING CODE: DROPPED FOR NOW.  (SLPJ 7/2/94)

	(Var scrut_var, (fda, AlgAlts [(con,bs,rhs')] NoDefault))
	 	| scrut_var_lvl `ltMajLvl` lvl ->

		-- Candidate for case floater; scrutinising a variable; it can
		-- escape outside a lambda; there's only one alternative.
		(fda ++ fde ++ [case_floater], rhs')

		where
		case_floater = (scrut_var_lvl, CaseFloater fn)
		fn body = Case scrut' (AlgAlts [(con,bs,body)] NoDefault)
		scrut_var_lvl = case lookupIdEnv env scrut_var of
				  Nothing  -> Level 0 0
				  Just lvl -> unTopify lvl

    END OF CASE FLOATING DROPPED -}
  where
      incd_lvl = incMinorLvl lvl

      partition_fn = partitionByMajorLevel

{-	OMITTED
	We don't want to be too keen about floating lets out of case alternatives
	because they may benefit from seeing the evaluation done by the case.

	The main reason for doing this is to allocate in fewer larger blocks
	but that's really an STG-level issue.

			case alts of
				-- Just one alternative, then dump only
				-- what *has* to be dumped
			AlgAlts  [_] NoDefault	   -> partitionByLevel
			AlgAlts  []  (BindDefault _ _) -> partitionByLevel
			PrimAlts [_] NoDefault	   -> partitionByLevel
			PrimAlts []  (BindDefault _ _) -> partitionByLevel

				-- If there's more than one alternative, then
				-- this is a dumping point
			other				   -> partitionByMajorLevel
-}

      float_alts (AlgAlts alts deflt)
	= case (float_deflt  deflt)		 of { (fsd,  fdd,  deflt') ->
	  case (unzip3 (map float_alg_alt alts)) of { (fsas, fdas, alts') ->
	  (foldr add_stats fsd fsas,
	   concat fdas ++ fdd,
	   AlgAlts alts' deflt') }}

      float_alts (PrimAlts alts deflt)
	= case (float_deflt deflt)		  of { (fsd,   fdd, deflt') ->
	  case (unzip3 (map float_prim_alt alts)) of { (fsas, fdas, alts') ->
	  (foldr add_stats fsd fsas,
	   concat fdas ++ fdd,
	   PrimAlts alts' deflt') }}

      -------------
      float_alg_alt (con, bs, rhs)
	= let
	      bs' = map fst bs
	      new_env = growIdEnvList env bs
	  in
	  case (floatExpr new_env incd_lvl rhs)	of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', (con, bs', install heres rhs')) }}

      --------------
      float_prim_alt (lit, rhs)
	= case (floatExpr env incd_lvl rhs)		of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', (lit, install heres rhs')) }}

      --------------
      float_deflt NoDefault = (zero_stats, [], NoDefault)

      float_deflt (BindDefault (b,lvl) rhs)
	= case (floatExpr new_env lvl rhs)		of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', BindDefault b (install heres rhs')) }}
	where
	  new_env = addOneToIdEnv env b lvl
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

zero_stats = FlS 0 0 0

sum_stats xs = foldr add_stats zero_stats xs

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

	-> FloatingBinds   	-- Defns to be divided into 2 piles...

	-> (FloatingBinds,	-- Defns  with level strictly < partition level,
	    FloatingBinds)	-- The rest


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
floatsToBinds :: FloatingBinds -> [CoreBinding]
floatsToBinds floats = map get_bind floats
		     where
		       get_bind (_, LetFloater bind) = bind
		       get_bind (_, CaseFloater _)   = panic "floatsToBinds"

floatsToBindPairs :: FloatingBinds -> [(Id,CoreExpr)]

floatsToBindPairs floats = concat (map mk_pairs floats)
  where
   mk_pairs (_, LetFloater (Rec pairs))         = pairs
   mk_pairs (_, LetFloater (NonRec binder rhs)) = [(binder,rhs)]
   mk_pairs (_, CaseFloater _) 			  = panic "floatsToBindPairs"

install :: FloatingBinds -> CoreExpr -> CoreExpr

install defn_groups expr
  = foldr install_group expr defn_groups
  where
    install_group (_, LetFloater defns) body = Let defns body
    install_group (_, CaseFloater fn)   body = fn body
\end{code}
