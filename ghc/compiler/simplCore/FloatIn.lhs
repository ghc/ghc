%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[FloatIn]{Floating Inwards pass}
%*									*
%************************************************************************

The main purpose of @floatInwards@ is floating into branches of a
case, so that we don't allocate things, save them on the stack, and
then discover that they aren't needed in the chosen branch.

\begin{code}
module FloatIn ( floatInwards ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_verbose_core2core )
import CoreSyn
import CoreLint		( beginPass, endPass )
import FreeVars		( CoreExprWithFVs, freeVars, freeVarsOf )
import Var		( Id )
import VarSet
import Util		( zipEqual )
import Outputable
\end{code}

Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.

\begin{code}
floatInwards :: [CoreBind] -> IO [CoreBind]

floatInwards binds
  = do {
	beginPass "Float inwards";
	let { binds' = map fi_top_bind binds };
	endPass "Float inwards" 
	 	opt_D_verbose_core2core		{- no specific flag for dumping float-in -} 
		binds'	
    }
			  
  where
    fi_top_bind (NonRec binder rhs)
      = NonRec binder (fiExpr [] (freeVars rhs))
    fi_top_bind (Rec pairs)
      = Rec [ (b, fiExpr [] (freeVars rhs)) | (b, rhs) <- pairs ]
\end{code}

%************************************************************************
%*									*
\subsection{Mail from Andr\'e [edited]}
%*									*
%************************************************************************

{\em Will wrote: What??? I thought the idea was to float as far
inwards as possible, no matter what.  This is dropping all bindings
every time it sees a lambda of any kind.  Help! }

You are assuming we DO DO full laziness AFTER floating inwards!  We
have to [not float inside lambdas] if we don't.

If we indeed do full laziness after the floating inwards (we could
check the compilation flags for that) then I agree we could be more
aggressive and do float inwards past lambdas.

Actually we are not doing a proper full laziness (see below), which
was another reason for not floating inwards past a lambda.

This can easily be fixed.  The problem is that we float lets outwards,
but there are a few expressions which are not let bound, like case
scrutinees and case alternatives.  After floating inwards the
simplifier could decide to inline the let and the laziness would be
lost, e.g.

\begin{verbatim}
let a = expensive             ==> \b -> case expensive of ...
in \ b -> case a of ...
\end{verbatim}
The fix is
\begin{enumerate}
\item
to let bind the algebraic case scrutinees (done, I think) and
the case alternatives (except the ones with an
unboxed type)(not done, I think). This is best done in the
SetLevels.lhs module, which tags things with their level numbers.
\item
do the full laziness pass (floating lets outwards).
\item
simplify. The simplifier inlines the (trivial) lets that were
 created but were not floated outwards.
\end{enumerate}

With the fix I think Will's suggestion that we can gain even more from
strictness by floating inwards past lambdas makes sense.

We still gain even without going past lambdas, as things may be
strict in the (new) context of a branch (where it was floated to) or
of a let rhs, e.g.
\begin{verbatim}
let a = something            case x of
in case x of                   alt1 -> case something of a -> a + a
     alt1 -> a + a      ==>    alt2 -> b
     alt2 -> b

let a = something           let b = case something of a -> a + a
in let b = a + a        ==> in (b,b)
in (b,b)
\end{verbatim}
Also, even if a is not found to be strict in the new context and is
still left as a let, if the branch is not taken (or b is not entered)
the closure for a is not built.

%************************************************************************
%*									*
\subsection{Main floating-inwards code}
%*									*
%************************************************************************

\begin{code}
type FreeVarsSet   = IdSet

type FloatingBinds = [(CoreBind, FreeVarsSet)]
	-- In reverse dependency order (innermost bindiner first)

	-- The FreeVarsSet is the free variables of the binding.  In the case
	-- of recursive bindings, the set doesn't include the bound
	-- variables.

fiExpr :: FloatingBinds		-- Binds we're trying to drop
				-- as far "inwards" as possible
       -> CoreExprWithFVs	-- Input expr
       -> CoreExpr		-- Result

fiExpr to_drop (_, AnnVar v) = mkCoLets' to_drop (Var v)

fiExpr to_drop (_, AnnType ty) = ASSERT( null to_drop )
				 Type ty

fiExpr to_drop (_, AnnCon c args)
   = mkCoLets' drop_here (Con c args')
   where
     (drop_here : arg_drops) = sepBindsByDropPoint (map freeVarsOf args) to_drop
     args'	 	     = zipWith fiExpr arg_drops args
\end{code}

Applications: we do float inside applications, mainly because we
need to get at all the arguments.  The next simplifier run will
pull out any silly ones.

\begin{code}
fiExpr to_drop (_,AnnApp fun arg)
  = mkCoLets' drop_here (App (fiExpr fun_drop fun) (fiExpr arg_drop arg))
  where
    [drop_here, fun_drop, arg_drop] = sepBindsByDropPoint [freeVarsOf fun, freeVarsOf arg] to_drop
\end{code}

We are careful about lambdas:

* We never float inside a value lambda.  That risks losing laziness.
  The float-out pass might rescue us, but then again it might not.

* We don't float inside type lambdas either.  At one time we did, and
  there is no risk of duplicating work thereby, but we do need to be
  careful.  In particular, here is a bad case (it happened in the
  cichelli benchmark:
   	let v = ...
   	in let f = /\t -> \a -> ...
   	   ==>
   	let f = /\t -> let v = ... in \a -> ...
  This is bad as now f is an updatable closure (update PAP)
  and has arity 0.

So the simple thing is never to float inside big lambda either.
Maybe we'll find cases when that loses something important; if
so we can modify the decision.

\begin{code}
fiExpr to_drop (_, AnnLam b body)
  = mkCoLets' to_drop (Lam b (fiExpr [] body))
\end{code}

We don't float lets inwards past an SCC.
	ToDo: keep info on current cc, and when passing
	one, if it is not the same, annotate all lets in binds with current
	cc, change current cc to the new one and float binds into expr.

\begin{code}
fiExpr to_drop (_, AnnNote note@(SCC cc) expr)
  = 	-- Wimp out for now
    mkCoLets' to_drop (Note note (fiExpr [] expr))

fiExpr to_drop (_, AnnNote InlineCall expr)
  = 	-- Wimp out for InlineCall; keep it close
	-- the the call it annotates
    mkCoLets' to_drop (Note InlineCall (fiExpr [] expr))

fiExpr to_drop (_, AnnNote note@(Coerce _ _) expr)
  = 	-- Just float in past coercion
    Note note (fiExpr to_drop expr)
\end{code}

For @Lets@, the possible ``drop points'' for the \tr{to_drop}
bindings are: (a)~in the body, (b1)~in the RHS of a NonRec binding,
or~(b2), in each of the RHSs of the pairs of a @Rec@.

Note that we do {\em weird things} with this let's binding.  Consider:
\begin{verbatim}
let
    w = ...
in {
    let v = ... w ...
    in ... w ...
}
\end{verbatim}
Look at the inner \tr{let}.  As \tr{w} is used in both the bind and
body of the inner let, we could panic and leave \tr{w}'s binding where
it is.  But \tr{v} is floatable into the body of the inner let, and
{\em then} \tr{w} will also be only in the body of that inner let.

So: rather than drop \tr{w}'s binding here, we add it onto the list of
things to drop in the outer let's body, and let nature take its
course.

\begin{code}
fiExpr to_drop (_,AnnLet (AnnNonRec id rhs) body)
  = fiExpr new_to_drop body
  where
    rhs_fvs  = freeVarsOf rhs
    body_fvs = freeVarsOf body

    [shared_binds, rhs_binds, body_binds] = sepBindsByDropPoint [rhs_fvs, body_fvs] to_drop

    new_to_drop = body_binds ++				-- the bindings used only in the body
		  [(NonRec id rhs', rhs_fvs')] ++ 	-- the new binding itself
		  shared_binds  			-- the bindings used both in rhs and body

	-- Push rhs_binds into the right hand side of the binding
    rhs'     = fiExpr rhs_binds rhs
    rhs_fvs' = rhs_fvs `unionVarSet` floatedBindsFVs rhs_binds

fiExpr to_drop (_,AnnLet (AnnRec bindings) body)
  = fiExpr new_to_drop body
  where
    (binders, rhss) = unzip bindings

    rhss_fvs = map freeVarsOf rhss
    body_fvs = freeVarsOf body

    (shared_binds:body_binds:rhss_binds) = sepBindsByDropPoint (body_fvs:rhss_fvs) to_drop

    new_to_drop = -- the bindings used only in the body
		  body_binds ++
		  -- the new binding itself
		  [(Rec (fi_bind rhss_binds bindings), rhs_fvs')] ++
		  -- the bindings used both in rhs and body or in more than one rhs
		  shared_binds

    rhs_fvs' = unionVarSet (unionVarSets rhss_fvs)
			   (unionVarSets (map floatedBindsFVs rhss_binds))

    -- Push rhs_binds into the right hand side of the binding
    fi_bind :: [FloatingBinds]	    -- one per "drop pt" conjured w/ fvs_of_rhss
	    -> [(Id, CoreExprWithFVs)]
	    -> [(Id, CoreExpr)]

    fi_bind to_drops pairs
      = [ (binder, fiExpr to_drop rhs) 
	| ((binder, rhs), to_drop) <- zipEqual "fi_bind" pairs to_drops ]
\end{code}

For @Case@, the possible ``drop points'' for the \tr{to_drop}
bindings are: (a)~inside the scrutinee, (b)~inside one of the
alternatives/default [default FVs always {\em first}!].

\begin{code}
fiExpr to_drop (_, AnnCase scrut case_bndr alts)
  = mkCoLets' drop_here (Case (fiExpr scrut_drops scrut) case_bndr
			      (zipWith fi_alt alts_drops alts))
  where
    (drop_here : scrut_drops : alts_drops) = sepBindsByDropPoint (scrut_fvs : alts_fvs) to_drop
    scrut_fvs = freeVarsOf scrut
    alts_fvs  = map alt_fvs alts
    alt_fvs (con, args, rhs) = foldl delVarSet (freeVarsOf rhs) (case_bndr:args)
				-- Delete case_bndr and args from free vars of rhs 
				-- to get free vars of alt

    fi_alt to_drop (con, args, rhs) = (con, args, fiExpr to_drop rhs)
\end{code}


%************************************************************************
%*									*
\subsection{@sepBindsByDropPoint@}
%*									*
%************************************************************************

This is the crucial function.  The idea is: We have a wad of bindings
that we'd like to distribute inside a collection of {\em drop points};
insides the alternatives of a \tr{case} would be one example of some
drop points; the RHS and body of a non-recursive \tr{let} binding
would be another (2-element) collection.

So: We're given a list of sets-of-free-variables, one per drop point,
and a list of floating-inwards bindings.  If a binding can go into
only one drop point (without suddenly making something out-of-scope),
in it goes.  If a binding is used inside {\em multiple} drop points,
then it has to go in a you-must-drop-it-above-all-these-drop-points
point.

We have to maintain the order on these drop-point-related lists.

\begin{code}
sepBindsByDropPoint
    :: [FreeVarsSet]	    -- One set of FVs per drop point
    -> FloatingBinds 	    -- Candidate floaters
    -> [FloatingBinds]      -- FIRST one is bindings which must not be floated
			    -- inside any drop point; the rest correspond
			    -- one-to-one with the input list of FV sets

-- Every input floater is returned somewhere in the result;
-- none are dropped, not even ones which don't seem to be
-- free in *any* of the drop-point fvs.  Why?  Because, for example,
-- a binding (let x = E in B) might have a specialised version of
-- x (say x') stored inside x, but x' isn't free in E or B.

sepBindsByDropPoint drop_pts []
  = [] : [[] | p <- drop_pts]	-- cut to the chase scene; it happens

sepBindsByDropPoint drop_pts floaters
  = go floaters (map (\fvs -> (fvs, [])) (emptyVarSet : drop_pts))
  where
    go :: FloatingBinds -> [(FreeVarsSet, FloatingBinds)] -> [FloatingBinds]
	-- The *first* one in the argument list is the drop_here set
	-- The FloatingBinds in the lists are in the reverse of
	-- the normal FloatingBinds order; that is, they are the right way round!

    go [] drop_boxes = map (reverse . snd) drop_boxes

    go (bind_w_fvs@(bind, bind_fvs) : binds) drop_boxes
	= go binds (insert drop_boxes (drop_here : used_in_flags))
		-- insert puts the find in box whose True flag comes first
	where
	  (used_here : used_in_flags) = [ any (`elemVarSet` fvs) (bindersOf bind)
					| (fvs, drops) <- drop_boxes]

	  drop_here = used_here || not (exactlyOneTrue used_in_flags)

	  insert ((fvs,drops) : drop_boxes) (True : _)
		= ((fvs `unionVarSet` bind_fvs, bind_w_fvs:drops) : drop_boxes)
	  insert (drop_box : drop_boxes) (False : others)
		= drop_box : insert drop_boxes others
	  insert _ _ = panic "sepBindsByDropPoint"	-- Should never happen

exactlyOneTrue :: [Bool] -> Bool
exactlyOneTrue flags = case [() | True <- flags] of
			[_]   -> True
			other -> False

floatedBindsFVs :: FloatingBinds -> FreeVarsSet
floatedBindsFVs binds = unionVarSets (map snd binds)

mkCoLets' :: FloatingBinds -> CoreExpr -> CoreExpr
mkCoLets' to_drop e = foldl (flip (Let . fst)) e to_drop
	-- Remember to_drop is in *reverse* dependency order
\end{code}
