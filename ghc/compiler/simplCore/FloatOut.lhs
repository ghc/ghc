%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
#include "HsVersions.h"

module FloatOut ( floatOutwards ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty
import Outputable

import PlainCore

import BasicLit		( BasicLit(..), PrimKind )
import CmdLineOpts	( GlobalSwitch(..) )
import CostCentre	( dupifyCC, CostCentre )
import SetLevels
import Id		( eqId )
import IdEnv
import Maybes		( Maybe(..), catMaybes, maybeToBool )
import SplitUniq
import Util
\end{code}

Random comments
~~~~~~~~~~~~~~~
At the moment we never float a binding out to between two adjacent lambdas.  For
example:
@
	\x y -> let t = x+x in ...
===>
	\x -> let t = x+x in \y -> ...
@
Reason: this is less efficient in the case where the original lambda is
never partially applied.

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
type LevelledExpr  = CoreExpr	 (Id, Level) Id
type LevelledBind  = CoreBinding (Id, Level) Id
type FloatingBind  = (Level, Floater)
type FloatingBinds = [FloatingBind]

data Floater = LetFloater     PlainCoreBinding

	     | CaseFloater   (PlainCoreExpr -> PlainCoreExpr)
				-- Give me a right-hand side of the
				-- (usually single) alternative, and
				-- I'll build the case
\end{code}

%************************************************************************
%*									*
\subsection[floatOutwards]{@floatOutwards@: let-floating interface function}
%*									*
%************************************************************************

\begin{code}
floatOutwards :: (GlobalSwitch -> Bool)	 -- access to all global cmd-line opts
	      -> SplitUniqSupply
	      -> PlainCoreProgram 
	      -> PlainCoreProgram

floatOutwards sw_chker us pgm
  = case (setLevels pgm sw_chker us) of { annotated_w_levels ->

    case unzip (map (floatTopBind sw_chker) annotated_w_levels)
		of { (fss, final_toplev_binds_s) ->

    (if sw_chker D_verbose_core2core
     then pprTrace "Levels added:\n" (ppr PprDebug annotated_w_levels)
     else id
    )
    ( if not (sw_chker D_simplifier_stats) then
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

floatTopBind sw bind@(CoNonRec _ _)
  = case (floatBind sw nullIdEnv tOP_LEVEL bind) of { (fs, floats, bind', _) ->
    (fs, floatsToBinds floats ++ [bind'])
    }

floatTopBind sw bind@(CoRec _)
  = case (floatBind sw nullIdEnv tOP_LEVEL bind) of { (fs, floats, CoRec pairs', _) ->
	-- Actually floats will be empty
    --false:ASSERT(null floats)
    (fs, [CoRec (floatsToBindPairs floats ++ pairs')])
    }
\end{code}

%************************************************************************
%*									*
\subsection[FloatOut-Bind]{Floating in a binding (the business end)}
%*									*
%************************************************************************


\begin{code}
floatBind :: (GlobalSwitch -> Bool) 
	  -> IdEnv Level
	  -> Level
	  -> LevelledBind
	  -> (FloatStats, FloatingBinds, PlainCoreBinding, IdEnv Level)

floatBind sw env lvl (CoNonRec (name,level) rhs)
  = case (floatExpr sw env level rhs) of { (fs, rhs_floats, rhs') ->

	-- A good dumping point
    case (partitionByMajorLevel level rhs_floats)	of { (rhs_floats', heres) ->

    (fs, rhs_floats',CoNonRec name (install heres rhs'), addOneToIdEnv env name level)
    }}
    
floatBind sw env lvl bind@(CoRec pairs)
  = case (unzip3 (map do_pair pairs)) of { (fss, rhss_floats, new_pairs) ->

    if not (isTopLvl bind_level) then
	-- Standard case
	(sum_stats fss, concat rhss_floats, CoRec new_pairs, new_env)
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
	 CoRec (new_pairs ++ floatsToBindPairs (concat rhss_floats)),
	 new_env)

    }
  where
    new_env = growIdEnvList env (map fst pairs)

    bind_level = getBindLevel bind

    do_pair ((name, level), rhs)
      = case (floatExpr sw new_env level rhs) of { (fs, rhs_floats, rhs') ->

		-- A good dumping point
	case (partitionByMajorLevel level rhs_floats)	of { (rhs_floats', heres) ->

	(fs, rhs_floats', (name, install heres rhs'))
	}}
\end{code}

%************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
%*									*
%************************************************************************

\begin{code}
floatExpr :: (GlobalSwitch -> Bool) 
	  -> IdEnv Level
	  -> Level 
	  -> LevelledExpr
	  -> (FloatStats, FloatingBinds, PlainCoreExpr)

floatExpr sw env _ (CoVar v)	     = (zero_stats, [], CoVar v)

floatExpr sw env _ (CoLit l)     = (zero_stats, [], CoLit l)

floatExpr sw env _ (CoPrim op ty as) = (zero_stats, [], CoPrim op ty as)
floatExpr sw env _ (CoCon con ty as) = (zero_stats, [], CoCon con ty as)

floatExpr sw env lvl (CoApp e a)
  = case (floatExpr sw env lvl e) of { (fs, floating_defns, e') ->
    (fs, floating_defns, CoApp e' a) }
    
floatExpr sw env lvl (CoTyApp e ty)
  = case (floatExpr sw env lvl e) of { (fs, floating_defns, e') ->
    (fs, floating_defns, CoTyApp e' ty) }

floatExpr sw env lvl (CoTyLam tv e)
  = let
	incd_lvl = incMinorLvl lvl
    in
    case (floatExpr sw env incd_lvl e) of { (fs, floats, e') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (fs, floats', CoTyLam tv (install heres e'))
    }}

floatExpr sw env lvl (CoLam args@((_,incd_lvl):_) rhs)
  = let
	args'	 = map fst args
	new_env  = growIdEnvList env args
    in
    case (floatExpr sw new_env incd_lvl rhs) of { (fs, floats, rhs') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partitionByLevel incd_lvl floats)	of { (floats', heres) ->

    (add_to_stats fs floats',
     floats',
     mkCoLam args' (install heres rhs'))
    }}

floatExpr sw env lvl (CoSCC cc expr)
  = case (floatExpr sw env lvl expr)    of { (fs, floating_defns, expr') ->
    let
	-- annotate bindings floated outwards past an scc expression
	-- with the cc.  We mark that cc as "duplicated", though.

	annotated_defns = annotate (dupifyCC cc) floating_defns
    in
    (fs, annotated_defns, CoSCC cc expr') }
  where
    annotate :: CostCentre -> FloatingBinds -> FloatingBinds

    annotate dupd_cc defn_groups
      = [ (level, ann_bind floater) | (level, floater) <- defn_groups ]
      where
	ann_bind (LetFloater (CoNonRec binder rhs)) 
	  = LetFloater (CoNonRec binder (ann_rhs rhs))

	ann_bind (LetFloater (CoRec pairs))
	  = LetFloater (CoRec [(binder, ann_rhs rhs) | (binder, rhs) <- pairs])

	ann_bind (CaseFloater fn) = CaseFloater ( \ rhs -> CoSCC dupd_cc (fn rhs) )

	ann_rhs (CoLam	 args e) = CoLam   args (ann_rhs e)
	ann_rhs (CoTyLam tv   e) = CoTyLam tv	(ann_rhs e)
	ann_rhs rhs@(CoCon _ _ _)= rhs	-- no point in scc'ing WHNF data
	ann_rhs rhs		 = CoSCC dupd_cc rhs

	-- Note: Nested SCC's are preserved for the benefit of
	--       cost centre stack profiling (Durham)

floatExpr sw env lvl (CoLet bind body)
  = case (floatBind sw env     lvl bind) of { (fsb, rhs_floats, bind', new_env) ->
    case (floatExpr sw new_env lvl body) of { (fse, body_floats, body') ->
    (add_stats fsb fse,
     rhs_floats ++ [(bind_lvl, LetFloater bind')] ++ body_floats,
     body')
    }}
  where
    bind_lvl = getBindLevel bind

floatExpr sw env lvl (CoCase scrut alts)
  = case (floatExpr sw env lvl scrut) of { (fse, fde, scrut') ->

    case (scrut', float_alts alts) of 

{-	CASE-FLOATING DROPPED FOR NOW.  (SLPJ 7/2/94)

	(CoVar scrut_var, (fda, CoAlgAlts [(con,bs,rhs')] CoNoDefault)) 
	 	| scrut_var_lvl `ltMajLvl` lvl ->

		-- Candidate for case floater; scrutinising a variable; it can
		-- escape outside a lambda; there's only one alternative.
		(fda ++ fde ++ [case_floater], rhs')

		where
		case_floater = (scrut_var_lvl, CaseFloater fn)
		fn body = CoCase scrut' (CoAlgAlts [(con,bs,body)] CoNoDefault)
		scrut_var_lvl = case lookupIdEnv env scrut_var of
				  Nothing  -> Level 0 0
				  Just lvl -> unTopify lvl

 END OF CASE FLOATING DROPPED  	-}

	(_, (fsa, fda, alts')) -> 

		(add_stats fse fsa, fda ++ fde, CoCase scrut' alts') 
    }
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
			CoAlgAlts  [_] CoNoDefault	   -> partitionByLevel
			CoAlgAlts  []  (CoBindDefault _ _) -> partitionByLevel
			CoPrimAlts [_] CoNoDefault	   -> partitionByLevel
			CoPrimAlts []  (CoBindDefault _ _) -> partitionByLevel

				-- If there's more than one alternative, then
				-- this is a dumping point
			other				   -> partitionByMajorLevel
-}

      float_alts (CoAlgAlts alts deflt)
	= case (float_deflt  deflt)		 of { (fsd,  fdd,  deflt') ->
	  case (unzip3 (map float_alg_alt alts)) of { (fsas, fdas, alts') ->
	  (foldr add_stats fsd fsas,
	   concat fdas ++ fdd,
	   CoAlgAlts alts' deflt') }}

      float_alts (CoPrimAlts alts deflt)
	= case (float_deflt deflt)		  of { (fsd,   fdd, deflt') ->
	  case (unzip3 (map float_prim_alt alts)) of { (fsas, fdas, alts') ->
	  (foldr add_stats fsd fsas,
	   concat fdas ++ fdd,
	   CoPrimAlts alts' deflt') }}

      -------------
      float_alg_alt (con, bs, rhs)
	= let
	      bs' = map fst bs
	      new_env = growIdEnvList env bs
	  in
	  case (floatExpr sw new_env incd_lvl rhs)	of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', (con, bs', install heres rhs')) }}

      --------------
      float_prim_alt (lit, rhs)
	= case (floatExpr sw env incd_lvl rhs)		of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', (lit, install heres rhs')) }}

      --------------
      float_deflt CoNoDefault = (zero_stats, [], CoNoDefault)

      float_deflt (CoBindDefault (b,lvl) rhs)
	= case (floatExpr sw new_env lvl rhs)		of { (fs, rhs_floats, rhs') ->
	  case (partition_fn incd_lvl rhs_floats)	of { (rhs_floats', heres) ->
	  (fs, rhs_floats', CoBindDefault b (install heres rhs')) }}
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
getBindLevel (CoNonRec (_, lvl) _)      = lvl
getBindLevel (CoRec (((_,lvl), _) : _)) = lvl
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
floatsToBinds :: FloatingBinds -> [PlainCoreBinding]
floatsToBinds floats = map get_bind floats
		     where
		       get_bind (_, LetFloater bind) = bind
		       get_bind (_, CaseFloater _)   = panic "floatsToBinds"

floatsToBindPairs :: FloatingBinds -> [(Id,PlainCoreExpr)]

floatsToBindPairs floats = concat (map mk_pairs floats)
  where
   mk_pairs (_, LetFloater (CoRec pairs))         = pairs
   mk_pairs (_, LetFloater (CoNonRec binder rhs)) = [(binder,rhs)]
   mk_pairs (_, CaseFloater _) 			  = panic "floatsToBindPairs"

install :: FloatingBinds -> PlainCoreExpr -> PlainCoreExpr

install defn_groups expr
  = foldr install_group expr defn_groups
  where
    install_group (_, LetFloater defns) body = CoLet defns body
    install_group (_, CaseFloater fn)   body = fn body
\end{code}
