%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[FloatOut]{Float bindings outwards (towards the top level)}

``Long-distance'' floating of bindings towards the top level.

\begin{code}
module FloatOut ( floatOutwards ) where

#include "HsVersions.h"

import CoreSyn
import CoreUtils	( mkSCC, exprIsHNF, exprIsTrivial )

import DynFlags	( DynFlags, DynFlag(..), FloatOutSwitches(..) )
import ErrUtils		( dumpIfSet_dyn )
import CostCentre	( dupifyCC, CostCentre )
import Id		( Id, idType )
import Type		( isUnLiftedType )
import CoreLint		( showPass, endPass )
import SetLevels	( Level(..), LevelledExpr, LevelledBind,
			  setLevels, ltMajLvl, ltLvl, isTopLvl )
import UniqSupply       ( UniqSupply )
import List		( partition )
import Outputable
import Util             ( notNull )
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

\begin{code}
type FloatBind     = (Level, CoreBind)	-- INVARIANT: a FloatBind is always lifted
type FloatBinds    = [FloatBind]	
\end{code}

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
	showPass dflags float_msg ;

	let { annotated_w_levels = setLevels float_sws pgm us ;
	      (fss, binds_s')    = unzip (map floatTopBind annotated_w_levels)
	    } ;

	dumpIfSet_dyn dflags Opt_D_verbose_core2core "Levels added:"
	          (vcat (map ppr annotated_w_levels));

	let { (tlets, ntlets, lams) = get_stats (sum_stats fss) };

	dumpIfSet_dyn dflags Opt_D_dump_simpl_stats "FloatOut stats:"
		(hcat [	int tlets,  ptext SLIT(" Lets floated to top level; "),
			int ntlets, ptext SLIT(" Lets floated elsewhere; from "),
			int lams,   ptext SLIT(" Lambda groups")]);

	endPass dflags float_msg  Opt_D_verbose_core2core (concat binds_s')
			{- no specific flag for dumping float-out -} 
    }
  where
    float_msg = showSDoc (text "Float out" <+> parens (sws float_sws))
    sws (FloatOutSw lam const) = pp_not lam   <+> text "lambdas" <> comma <+>
				 pp_not const <+> text "constants"
    pp_not True  = empty
    pp_not False = text "not"

floatTopBind bind@(NonRec _ _)
  = case (floatBind bind) of { (fs, floats, bind') ->
    (fs, floatsToBinds floats ++ [bind'])
    }

floatTopBind bind@(Rec _)
  = case (floatBind bind) of { (fs, floats, Rec pairs') ->
    WARN( notNull floats, ppr bind $$ ppr floats )
    (fs, [Rec (floatsToBindPairs floats ++ pairs')]) }
\end{code}

%************************************************************************
%*									*
\subsection[FloatOut-Bind]{Floating in a binding (the business end)}
%*									*
%************************************************************************


\begin{code}
floatBind :: LevelledBind
	  -> (FloatStats, FloatBinds, CoreBind)

floatBind (NonRec (TB name level) rhs)
  = case (floatNonRecRhs level rhs) of { (fs, rhs_floats, rhs') ->
    (fs, rhs_floats, NonRec name rhs') }

floatBind bind@(Rec pairs)
  = case (unzip3 (map do_pair pairs)) of { (fss, rhss_floats, new_pairs) ->

    if not (isTopLvl bind_dest_level) then
	-- Standard case; the floated bindings can't mention the
	-- binders, because they couldn't be escaping a major level
	-- if so.
	(sum_stats fss, concat rhss_floats, Rec new_pairs)
    else
	-- In a recursive binding, *destined for* the top level
	-- (only), the rhs floats may contain references to the 
	-- bound things.  For example
	--	f = ...(let v = ...f... in b) ...
	--  might get floated to
	--	v = ...f...
	--	f = ... b ...
	-- and hence we must (pessimistically) make all the floats recursive
	-- with the top binding.  Later dependency analysis will unravel it.
	--
	-- This can only happen for bindings destined for the top level,
	-- because only then will partitionByMajorLevel allow through a binding
	-- that only differs in its minor level
	(sum_stats fss, [],
	 Rec (new_pairs ++ floatsToBindPairs (concat rhss_floats)))
    }
  where
    bind_dest_level = getBindLevel bind

    do_pair (TB name level, rhs)
      = case (floatRhs level rhs) of { (fs, rhs_floats, rhs') ->
	(fs, rhs_floats, (name, rhs'))
	}
\end{code}

%************************************************************************

\subsection[FloatOut-Expr]{Floating in expressions}
%*									*
%************************************************************************

\begin{code}
floatExpr, floatRhs, floatNonRecRhs
	 :: Level
	 -> LevelledExpr
	 -> (FloatStats, FloatBinds, CoreExpr)

floatRhs lvl arg	-- Used rec rhss, and case-alternative rhss
  = case (floatExpr lvl arg) of { (fsa, floats, arg') ->
    case (partitionByMajorLevel lvl floats) of { (floats', heres) ->
	-- Dump bindings that aren't going to escape from a lambda;
	-- in particular, we must dump the ones that are bound by 
	-- the rec or case alternative
    (fsa, floats', install heres arg') }}

floatNonRecRhs lvl arg	-- Used for nested non-rec rhss, and fn args
  = case (floatExpr lvl arg) of { (fsa, floats, arg') ->
	-- Dump bindings that aren't going to escape from a lambda
	-- This isn't a scoping issue (the binder isn't in scope in the RHS of a non-rec binding)
	-- Rather, it is to avoid floating the x binding out of
	--	f (let x = e in b)
	-- unnecessarily.  But we first test for values or trival rhss,
	-- because (in particular) we don't want to insert new bindings between
	-- the "=" and the "\".  E.g.
	--	f = \x -> let <bind> in <body>
	-- We do not want
	--	f = let <bind> in \x -> <body>
	-- (a) The simplifier will immediately float it further out, so we may
	--	as well do so right now; in general, keeping rhss as manifest 
	--	values is good
	-- (b) If a float-in pass follows immediately, it might add yet more
	--	bindings just after the '='.  And some of them might (correctly)
	--	be strict even though the 'let f' is lazy, because f, being a value,
	--	gets its demand-info zapped by the simplifier.
    if exprIsHNF arg' || exprIsTrivial arg' then
	(fsa, floats, arg')
    else
    case (partitionByMajorLevel lvl floats) of { (floats', heres) ->
    (fsa, floats', install heres arg') }}

floatExpr _ (Var v)   = (zeroStats, [], Var v)
floatExpr _ (Type ty) = (zeroStats, [], Type ty)
floatExpr _ (Lit lit) = (zeroStats, [], Lit lit)
	  
floatExpr lvl (App e a)
  = case (floatExpr      lvl e) of { (fse, floats_e, e') ->
    case (floatNonRecRhs lvl a) of { (fsa, floats_a, a') ->
    (fse `add_stats` fsa, floats_e ++ floats_a, App e' a') }}

floatExpr lvl lam@(Lam _ _)
  = let
	(bndrs_w_lvls, body) = collectBinders lam
	bndrs		     = [b | TB b _ <- bndrs_w_lvls]
	lvls		     = [l | TB b l <- bndrs_w_lvls]

	-- For the all-tyvar case we are prepared to pull 
	-- the lets out, to implement the float-out-of-big-lambda
	-- transform; but otherwise we only float bindings that are
	-- going to escape a value lambda.
	-- In particular, for one-shot lambdas we don't float things
	-- out; we get no saving by so doing.
	partition_fn | all isTyVar bndrs = partitionByLevel
		     | otherwise	 = partitionByMajorLevel
    in
    case (floatExpr (last lvls) body) of { (fs, floats, body') ->

	-- Dump any bindings which absolutely cannot go any further
    case (partition_fn (head lvls) floats)	of { (floats', heres) ->

    (add_to_stats fs floats', floats', mkLams bndrs (install heres body'))
    }}

floatExpr lvl (Note note@(SCC cc) expr)
  = case (floatExpr lvl expr)    of { (fs, floating_defns, expr') ->
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
	  = NonRec binder (mkSCC dupd_cc rhs)

	ann_bind (Rec pairs)
	  = Rec [(binder, mkSCC dupd_cc rhs) | (binder, rhs) <- pairs]

floatExpr lvl (Note InlineMe expr)	-- Other than SCCs
  = case floatExpr InlineCtxt expr of { (fs, floating_defns, expr') ->
	-- There can be some floating_defns, arising from
	-- ordinary lets that were there all the time.  It seems
	-- more efficient to test once here than to avoid putting
	-- them into floating_defns (which would mean testing for
	-- inlineCtxt  at every let)
    (fs, [], Note InlineMe (install floating_defns expr')) }	-- See notes in SetLevels

floatExpr lvl (Note note expr)	-- Other than SCCs
  = case (floatExpr lvl expr)    of { (fs, floating_defns, expr') ->
    (fs, floating_defns, Note note expr') }

floatExpr lvl (Let (NonRec (TB bndr bndr_lvl) rhs) body)
  | isUnLiftedType (idType bndr)	-- Treat unlifted lets just like a case
  = case floatExpr lvl rhs	of { (fs, rhs_floats, rhs') ->
    case floatRhs bndr_lvl body of { (fs, body_floats, body') ->
    (fs, rhs_floats ++ body_floats, Let (NonRec bndr rhs') body') }}

floatExpr lvl (Let bind body)
  = case (floatBind bind)     of { (fsb, rhs_floats,  bind') ->
    case (floatExpr lvl body) of { (fse, body_floats, body') ->
    (add_stats fsb fse,
     rhs_floats ++ [(bind_lvl, bind')] ++ body_floats,
     body')  }}
  where
    bind_lvl = getBindLevel bind

floatExpr lvl (Case scrut (TB case_bndr case_lvl) ty alts)
  = case floatExpr lvl scrut	of { (fse, fde, scrut') ->
    case floatList float_alt alts	of { (fsa, fda, alts')  ->
    (add_stats fse fsa, fda ++ fde, Case scrut' case_bndr ty alts')
    }}
  where
	-- Use floatRhs for the alternatives, so that we
	-- don't gratuitiously float bindings out of the RHSs
    float_alt (con, bs, rhs)
	= case (floatRhs case_lvl rhs)	of { (fs, rhs_floats, rhs') ->
	  (fs, rhs_floats, (con, [b | TB b _ <- bs], rhs')) }


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
getBindLevel (NonRec (TB _ lvl) _)      = lvl
getBindLevel (Rec (((TB _ lvl), _) : _)) = lvl
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
	-- Float it if we escape a value lambda, or if we get to the top level
    float_further (my_lvl, bind) = my_lvl `ltMajLvl` ctxt_lvl || isTopLvl my_lvl
	-- The isTopLvl part says that if we can get to the top level, say "yes" anyway
	-- This means that 
	--	x = f e
	-- transforms to 
	--    lvl = e
	--    x = f lvl
	-- which is as it should be

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
