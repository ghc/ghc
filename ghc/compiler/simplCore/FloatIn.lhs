%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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
#include "HsVersions.h"

module FloatIn (
	floatInwards,

	-- and to make the interface self-sufficient...
	CoreExpr, CoreBinding, Id, 
	PlainCoreProgram(..), PlainCoreExpr(..)
    ) where

import Pretty		-- ToDo: debugging only

import PlainCore
import AnnCoreSyn

import FreeVars
import UniqSet
import Util
\end{code}

Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.

\begin{code}
floatInwards :: [PlainCoreBinding] -> [PlainCoreBinding]

floatInwards binds 
  = map fi_top_bind binds
  where
    fi_top_bind (CoNonRec binder rhs) 
      = CoNonRec binder (fiExpr [] (freeVars rhs))
    fi_top_bind (CoRec pairs)
      = CoRec [ (b, fiExpr [] (freeVars rhs)) | (b, rhs) <- pairs ]
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

This can easily be fixed.
The problem is that we float lets outwards,
but there are a few expressions which are not
let bound, like case scrutinees and case alternatives.
After floating inwards the simplifier could decide to inline
the let and the laziness would be lost, e.g.
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
type FreeVarsSet   = UniqSet Id

type FloatingBinds = [(PlainCoreBinding, FreeVarsSet)]
	-- In dependency order (outermost first)

	-- The FreeVarsSet is the free variables of the binding.  In the case
	-- of recursive bindings, the set doesn't include the bound
	-- variables.

fiExpr :: FloatingBinds		-- binds we're trying to drop
				-- as far "inwards" as possible
       -> CoreExprWithFVs	-- input expr
       -> PlainCoreExpr		-- result

fiExpr to_drop (_,AnnCoVar v) = mkCoLets' to_drop (CoVar v)

fiExpr to_drop (_,AnnCoLit k) = mkCoLets' to_drop (CoLit k)

fiExpr to_drop (_,AnnCoCon c tys atoms)
  = mkCoLets' to_drop (CoCon c tys atoms)

fiExpr to_drop (_,AnnCoPrim c tys atoms)
  = mkCoLets' to_drop (CoPrim c tys atoms)
\end{code}

Here we are not floating inside lambda (type lambdas are OK):
\begin{code}
fiExpr to_drop (_,AnnCoLam binders body)
  = mkCoLets' to_drop (mkCoLam binders (fiExpr [] body))

fiExpr to_drop (_,AnnCoTyLam tyvar body)
  | whnf body
  -- we do not float into type lambdas if they are followed by 
  -- a whnf (actually we check for lambdas and constructors). 
  -- The reason is that a let binding will get stuck
  -- in between the type lambda and the whnf and the simplifier
  -- does not know how to pull it back out from a type lambda. 
  -- Ex:
  -- 	let v = ...
  -- 	in let f = /\t -> \a -> ...
  -- 	   ==>
  -- 	let f = /\t -> let v = ... in \a -> ...
  -- which is bad as now f is an updatable closure (update PAP)
  -- and has arity 0. This example comes from cichelli.
  = mkCoLets' to_drop (CoTyLam tyvar (fiExpr [] body))
  | otherwise
  = CoTyLam tyvar (fiExpr to_drop body)
  where 
    whnf :: CoreExprWithFVs -> Bool
    whnf (_,AnnCoLit _)     = True
    whnf (_,AnnCoCon _ _ _) = True
    whnf (_,AnnCoLam _ _)   = True
    whnf (_,AnnCoTyLam _ e) = whnf e
    whnf (_,AnnCoSCC _ e)   = whnf e
    whnf _                  = False

\end{code}

Applications: we could float inside applications, but it's probably
not worth it (a purely practical choice, hunch- [not experience-]
based).
\begin{code}
fiExpr to_drop (_,AnnCoApp fun atom)
  = mkCoLets' to_drop (CoApp (fiExpr [] fun) atom)

fiExpr to_drop (_,AnnCoTyApp expr ty)
  = CoTyApp (fiExpr to_drop expr) ty
\end{code}

We don't float lets inwards past an SCC.

ToDo: CoSCC: {\em should} keep info on current cc, and when passing
one, if it is not the same, annotate all lets in binds with current
cc, change current cc to the new one and float binds into expr.
\begin{code}
fiExpr to_drop (_, AnnCoSCC cc expr)
  = mkCoLets' to_drop (CoSCC cc (fiExpr [] expr))
\end{code}

For @CoLets@, the possible ``drop points'' for the \tr{to_drop}
bindings are: (a)~in the body, (b1)~in the RHS of a CoNonRec binding,
or~(b2), in each of the RHSs of the pairs of a @CoRec@.

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
fiExpr to_drop (_,AnnCoLet (AnnCoNonRec id rhs) body)
  = fiExpr new_to_drop body
  where
    rhs_fvs  = freeVarsOf rhs
    body_fvs = freeVarsOf body

    ([rhs_binds, body_binds], shared_binds) = sepBindsByDropPoint [rhs_fvs, body_fvs] to_drop 

    new_to_drop = body_binds ++				-- the bindings used only in the body
		  [(CoNonRec id rhs', rhs_fvs')] ++ 	-- the new binding itself
                  shared_binds  			-- the bindings used both in rhs and body

	-- Push rhs_binds into the right hand side of the binding
    rhs'     = fiExpr rhs_binds rhs
    rhs_fvs' = rhs_fvs `unionUniqSets` (floatedBindsFVs rhs_binds)

fiExpr to_drop (_,AnnCoLet (AnnCoRec bindings) body)
  = fiExpr new_to_drop body
  where
    (binders, rhss) = unzip bindings

    rhss_fvs = map freeVarsOf rhss
    body_fvs = freeVarsOf body

    (body_binds:rhss_binds, shared_binds) 
      = sepBindsByDropPoint (body_fvs:rhss_fvs) to_drop 

    new_to_drop = -- the bindings used only in the body
                  body_binds ++
                  -- the new binding itself
                  [(CoRec (fi_bind rhss_binds bindings), rhs_fvs')] ++ 
                  -- the bindings used both in rhs and body or in more than one rhs
                  shared_binds

    rhs_fvs' = unionUniqSets (unionManyUniqSets rhss_fvs) 
                     (unionManyUniqSets (map floatedBindsFVs rhss_binds))

    -- Push rhs_binds into the right hand side of the binding
    fi_bind :: [FloatingBinds]	    -- one per "drop pt" conjured w/ fvs_of_rhss
	    -> [(Id, CoreExprWithFVs)]
	    -> [(Id, PlainCoreExpr)]

    fi_bind to_drops pairs
      = [ (binder, fiExpr to_drop rhs) | ((binder, rhs), to_drop) <- zip pairs to_drops ]
\end{code}

For @CoCase@, the possible ``drop points'' for the \tr{to_drop}
bindings are: (a)~inside the scrutinee, (b)~inside one of the
alternatives/default [default FVs always {\em first}!].

\begin{code}
fiExpr to_drop (_, AnnCoCase scrut alts)
  = let
	fvs_scrut    = freeVarsOf scrut
	drop_pts_fvs = fvs_scrut : (get_fvs_from_deflt_and_alts alts)
    in
    case (sepBindsByDropPoint drop_pts_fvs to_drop)
		of (scrut_drops : deflt_drops : alts_drops, drop_here) ->
                     mkCoLets' drop_here (CoCase (fiExpr scrut_drops scrut)
		                                (fi_alts deflt_drops alts_drops alts))
    
  where
    ----------------------------
    -- pin default FVs on first!
    --
    get_fvs_from_deflt_and_alts (AnnCoAlgAlts alts deflt)
      = get_deflt_fvs deflt : [ freeVarsOf rhs | (_, _, rhs) <- alts ]

    get_fvs_from_deflt_and_alts (AnnCoPrimAlts alts deflt)
      = get_deflt_fvs deflt : [ freeVarsOf rhs | (_, rhs) <- alts]

    get_deflt_fvs AnnCoNoDefault	   = emptyUniqSet
    get_deflt_fvs (AnnCoBindDefault b rhs) = freeVarsOf rhs

    ----------------------------
    fi_alts to_drop_deflt to_drop_alts (AnnCoAlgAlts alts deflt)
      = CoAlgAlts
	    [ (con, params, fiExpr to_drop rhs)
	    | ((con, params, rhs), to_drop) <- alts `zip` to_drop_alts ]
	    (fi_default to_drop_deflt deflt)

    fi_alts to_drop_deflt to_drop_alts (AnnCoPrimAlts alts deflt)
      = CoPrimAlts
	    [ (lit, fiExpr to_drop rhs)
	    | ((lit, rhs), to_drop) <- alts `zip` to_drop_alts ]
	    (fi_default to_drop_deflt deflt)

    fi_default to_drop AnnCoNoDefault	      = CoNoDefault
    fi_default to_drop (AnnCoBindDefault b e) = CoBindDefault b (fiExpr to_drop e)
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
    :: [FreeVarsSet]	    -- one set of FVs per drop point
    -> FloatingBinds 	    -- candidate floaters
    -> ([FloatingBinds],    -- floaters that *can* be floated into
			    -- the corresponding drop point
        FloatingBinds)	    -- everything else, bindings which must
			    -- not be floated inside any drop point

sepBindsByDropPoint drop_pts []
  = ([[] | p <- drop_pts], []) -- cut to the chase scene; it happens

sepBindsByDropPoint drop_pts floaters
  = let
	(per_drop_pt, must_stay_here, _)
	    --= sep drop_pts emptyUniqSet{-fvs of prev drop_pts-} floaters
            = split' drop_pts floaters [] empty_boxes
        empty_boxes = take (length drop_pts) (repeat [])
	
    in
    (map reverse per_drop_pt, reverse must_stay_here)
  where
    split' drop_pts_fvs [] mult_branch drop_boxes
      = (drop_boxes, mult_branch, drop_pts_fvs)

    -- only in a or unused
    split' (a:as) (bind:binds) mult_branch (drop_box_a:drop_boxes)
      | all (\b -> {-b `elementOfUniqSet` a &&-}
                   not (b `elementOfUniqSet` (unionManyUniqSets as)))
            (bindersOf (fst bind))
      = split' (a':as) binds mult_branch ((bind:drop_box_a):drop_boxes)
      where
        a' = a `unionUniqSets` fvsOfBind bind

    -- not in a 
    split' (a:as) (bind:binds) mult_branch (drop_box_a:drop_boxes)
      | all (\b -> not (b `elementOfUniqSet` a)) (bindersOf (fst bind))
      = split' (a:as') binds mult_branch' (drop_box_a:drop_boxes')
      where
        (drop_boxes',mult_branch',as') = split' as [bind] mult_branch drop_boxes

    -- in a and in as
    split' aas@(a:as) (bind:binds) mult_branch drop_boxes
      = split' aas' binds (bind : mult_branch) drop_boxes
      where 
        aas' = map (unionUniqSets (fvsOfBind bind)) aas 

    -------------------------
    fvsOfBind (_,fvs)	= fvs

--floatedBindsFVs :: 
floatedBindsFVs binds = foldr unionUniqSets emptyUniqSet (map snd binds)

--mkCoLets' :: [FloatingBinds] -> PlainCoreExpr -> PlainCoreExpr
mkCoLets' to_drop e = mkCoLetsNoUnboxed (reverse (map fst to_drop)) e
\end{code}
