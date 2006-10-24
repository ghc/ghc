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

import DynFlags	( DynFlags, DynFlag(..) )
import CoreSyn
import CoreUtils	( exprIsHNF, exprIsDupable )
import CoreLint		( showPass, endPass )
import CoreFVs		( CoreExprWithFVs, freeVars, freeVarsOf )
import Id		( isOneShotBndr )
import Var		( Id, idType )
import Type		( isUnLiftedType )
import VarSet
import Util		( zipEqual, zipWithEqual, count )
import Outputable
\end{code}

Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.

\begin{code}
floatInwards :: DynFlags -> [CoreBind] -> IO [CoreBind]

floatInwards dflags binds
  = do {
	showPass dflags "Float inwards";
	let { binds' = map fi_top_bind binds };
	endPass dflags "Float inwards" Opt_D_verbose_core2core binds'	
				{- no specific flag for dumping float-in -} 
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
fiExpr to_drop (_, AnnCast expr co)
  = Cast (fiExpr to_drop expr) co	-- Just float in past coercion

fiExpr to_drop (_, AnnLit lit) = Lit lit
\end{code}

Applications: we do float inside applications, mainly because we
need to get at all the arguments.  The next simplifier run will
pull out any silly ones.

\begin{code}
fiExpr to_drop (_,AnnApp fun arg)
  = mkCoLets' drop_here (App (fiExpr fun_drop fun) (fiExpr arg_drop arg))
  where
    [drop_here, fun_drop, arg_drop] = sepBindsByDropPoint False [freeVarsOf fun, freeVarsOf arg] to_drop
\end{code}

We are careful about lambdas: 

* We must be careful about floating inside inside a value lambda.  
  That risks losing laziness.
  The float-out pass might rescue us, but then again it might not.

* We must be careful about type lambdas too.  At one time we did, and
  there is no risk of duplicating work thereby, but we do need to be
  careful.  In particular, here is a bad case (it happened in the
  cichelli benchmark:
   	let v = ...
   	in let f = /\t -> \a -> ...
   	   ==>
   	let f = /\t -> let v = ... in \a -> ...
  This is bad as now f is an updatable closure (update PAP)
  and has arity 0.

So we treat lambda in groups, using the following rule:

	Float inside a group of lambdas only if
	they are all either type lambdas or one-shot lambdas.

	Otherwise drop all the bindings outside the group.

\begin{code}
	-- Hack alert!  We only float in through one-shot lambdas, 
	-- not (as you might guess) through big lambdas.  
	-- Reason: we float *out* past big lambdas (see the test in the Lam
	-- case of FloatOut.floatExpr) and we don't want to float straight
	-- back in again.
	--
	-- It *is* important to float into one-shot lambdas, however;
	-- see the remarks with noFloatIntoRhs.
fiExpr to_drop lam@(_, AnnLam _ _)
  | all is_one_shot bndrs 	-- Float in
  = mkLams bndrs (fiExpr to_drop body)

  | otherwise	 	-- Dump it all here
  = mkCoLets' to_drop (mkLams bndrs (fiExpr [] body))

  where
    (bndrs, body) = collectAnnBndrs lam
\end{code}

We don't float lets inwards past an SCC.
	ToDo: keep info on current cc, and when passing
	one, if it is not the same, annotate all lets in binds with current
	cc, change current cc to the new one and float binds into expr.

\begin{code}
fiExpr to_drop (_, AnnNote note@(SCC cc) expr)
  = 	-- Wimp out for now
    mkCoLets' to_drop (Note note (fiExpr [] expr))

fiExpr to_drop (_, AnnNote InlineMe expr)
  = 	-- Ditto... don't float anything into an INLINE expression
    mkCoLets' to_drop (Note InlineMe (fiExpr [] expr))

fiExpr to_drop (_, AnnNote note@(TickBox {}) expr)
  =    -- Wimp out for now
    mkCoLets' to_drop (Note note (fiExpr [] expr))
fiExpr to_drop (_, AnnNote note@(BinaryTickBox {}) expr)
  =    -- Wimp out for now
    mkCoLets' to_drop (Note note (fiExpr [] expr))

fiExpr to_drop (_, AnnNote note@(CoreNote _) expr)
  = Note note (fiExpr to_drop expr)
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
    in ... v .. w ...
}
\end{verbatim}
Look at the inner \tr{let}.  As \tr{w} is used in both the bind and
body of the inner let, we could panic and leave \tr{w}'s binding where
it is.  But \tr{v} is floatable further into the body of the inner let, and
{\em then} \tr{w} will also be only in the body of that inner let.

So: rather than drop \tr{w}'s binding here, we add it onto the list of
things to drop in the outer let's body, and let nature take its
course.

\begin{code}
fiExpr to_drop (_,AnnLet (AnnNonRec id rhs@(rhs_fvs, ann_rhs)) body)
  = fiExpr new_to_drop body
  where
    body_fvs = freeVarsOf body

    final_body_fvs | noFloatIntoRhs ann_rhs
		   || isUnLiftedType (idType id) = body_fvs `unionVarSet` rhs_fvs
		   | otherwise		         = body_fvs
	-- See commments with letrec below
	-- No point in floating in only to float straight out again
	-- Ditto ok-for-speculation unlifted RHSs

    [shared_binds, rhs_binds, body_binds] = sepBindsByDropPoint False [rhs_fvs, final_body_fvs] to_drop

    new_to_drop = body_binds ++				-- the bindings used only in the body
		  [(NonRec id rhs', rhs_fvs')] ++ 	-- the new binding itself
		  shared_binds  			-- the bindings used both in rhs and body

	-- Push rhs_binds into the right hand side of the binding
    rhs'     = fiExpr rhs_binds rhs
    rhs_fvs' = rhs_fvs `unionVarSet` floatedBindsFVs rhs_binds

fiExpr to_drop (_,AnnLet (AnnRec bindings) body)
  = fiExpr new_to_drop body
  where
    rhss = map snd bindings

    rhss_fvs = map freeVarsOf rhss
    body_fvs = freeVarsOf body

	-- Add to body_fvs the free vars of any RHS that has
	-- a lambda at the top.  This has the effect of making it seem
	-- that such things are used in the body as well, and hence prevents
	-- them getting floated in.  The big idea is to avoid turning:
	--	let x# = y# +# 1#
	--	in
	--	letrec f = \z. ...x#...f...
	--	in ...
	-- into
	--	letrec f = let x# = y# +# 1# in \z. ...x#...f... in ...
	-- 
	-- Because now we can't float the let out again, because a letrec
	-- can't have unboxed bindings.

    final_body_fvs = foldr (unionVarSet . get_extras) body_fvs rhss
    get_extras (rhs_fvs, rhs) | noFloatIntoRhs rhs = rhs_fvs
			      | otherwise	   = emptyVarSet

    (shared_binds:body_binds:rhss_binds) = sepBindsByDropPoint False (final_body_fvs:rhss_fvs) to_drop

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
fiExpr to_drop (_, AnnCase scrut case_bndr ty alts)
  = mkCoLets' drop_here1 $
    mkCoLets' drop_here2 $
    Case (fiExpr scrut_drops scrut) case_bndr ty
	 (zipWith fi_alt alts_drops_s alts)
  where
	-- Float into the scrut and alts-considered-together just like App
    [drop_here1, scrut_drops, alts_drops] = sepBindsByDropPoint False [scrut_fvs, all_alts_fvs] to_drop

	-- Float into the alts with the is_case flag set
    (drop_here2 : alts_drops_s)           = sepBindsByDropPoint True alts_fvs alts_drops

    scrut_fvs    = freeVarsOf scrut
    alts_fvs     = map alt_fvs alts
    all_alts_fvs = unionVarSets alts_fvs
    alt_fvs (con, args, rhs) = foldl delVarSet (freeVarsOf rhs) (case_bndr:args)
				-- Delete case_bndr and args from free vars of rhs 
				-- to get free vars of alt

    fi_alt to_drop (con, args, rhs) = (con, args, fiExpr to_drop rhs)

noFloatIntoRhs (AnnNote InlineMe _) = True
noFloatIntoRhs (AnnLam b _)   	    = not (is_one_shot b)
	-- IMPORTANT: don't say 'True' for a RHS with a one-shot lambda at the top.
	-- This makes a big difference for things like
	--	f x# = let x = I# x#
	--	       in let j = \() -> ...x...
	--		  in if <condition> then normal-path else j ()
	-- If x is used only in the error case join point, j, we must float the
	-- boxing constructor into it, else we box it every time which is very bad
	-- news indeed.

noFloatIntoRhs rhs = exprIsHNF (deAnnotate' rhs)	-- We'd just float right back out again...

is_one_shot b = isId b && isOneShotBndr b
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
    :: Bool		    -- True <=> is case expression
    -> [FreeVarsSet]	    -- One set of FVs per drop point
    -> FloatingBinds 	    -- Candidate floaters
    -> [FloatingBinds]      -- FIRST one is bindings which must not be floated
			    -- inside any drop point; the rest correspond
			    -- one-to-one with the input list of FV sets

-- Every input floater is returned somewhere in the result;
-- none are dropped, not even ones which don't seem to be
-- free in *any* of the drop-point fvs.  Why?  Because, for example,
-- a binding (let x = E in B) might have a specialised version of
-- x (say x') stored inside x, but x' isn't free in E or B.

type DropBox = (FreeVarsSet, FloatingBinds)

sepBindsByDropPoint is_case drop_pts []
  = [] : [[] | p <- drop_pts]	-- cut to the chase scene; it happens

sepBindsByDropPoint is_case drop_pts floaters
  = go floaters (map (\fvs -> (fvs, [])) (emptyVarSet : drop_pts))
  where
    go :: FloatingBinds -> [DropBox] -> [FloatingBinds]
	-- The *first* one in the argument list is the drop_here set
	-- The FloatingBinds in the lists are in the reverse of
	-- the normal FloatingBinds order; that is, they are the right way round!

    go [] drop_boxes = map (reverse . snd) drop_boxes

    go (bind_w_fvs@(bind, bind_fvs) : binds) drop_boxes@(here_box : fork_boxes)
	= go binds new_boxes
	where
	  -- "here" means the group of bindings dropped at the top of the fork

	  (used_here : used_in_flags) = [ any (`elemVarSet` fvs) (bindersOf bind)
					| (fvs, drops) <- drop_boxes]

	  drop_here = used_here || not can_push

		-- For case expressions we duplicate the binding if it is
		-- reasonably small, and if it is not used in all the RHSs
		-- This is good for situations like
		--	let x = I# y in
		--	case e of
		--	  C -> error x
		-- 	  D -> error x
		--	  E -> ...not mentioning x...

	  n_alts      = length used_in_flags
	  n_used_alts = count id used_in_flags -- returns number of Trues in list.

	  can_push = n_used_alts == 1		-- Used in just one branch
		   || (is_case && 		-- We are looking at case alternatives
		       n_used_alts > 1 && 	-- It's used in more than one
		       n_used_alts < n_alts &&	-- ...but not all
		       bindIsDupable bind)	-- and we can duplicate the binding

	  new_boxes | drop_here = (insert here_box : fork_boxes)
		    | otherwise = (here_box : new_fork_boxes)

	  new_fork_boxes = zipWithEqual "FloatIn.sepBinds" insert_maybe fork_boxes used_in_flags

	  insert :: DropBox -> DropBox
	  insert (fvs,drops) = (fvs `unionVarSet` bind_fvs, bind_w_fvs:drops)

	  insert_maybe box True  = insert box
	  insert_maybe box False = box


floatedBindsFVs :: FloatingBinds -> FreeVarsSet
floatedBindsFVs binds = unionVarSets (map snd binds)

mkCoLets' :: FloatingBinds -> CoreExpr -> CoreExpr
mkCoLets' to_drop e = foldl (flip (Let . fst)) e to_drop
	-- Remember to_drop is in *reverse* dependency order

bindIsDupable (Rec prs)    = all (exprIsDupable . snd) prs
bindIsDupable (NonRec b r) = exprIsDupable r
\end{code}
