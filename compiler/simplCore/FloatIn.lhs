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
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module FloatIn ( floatInwards ) where

#include "HsVersions.h"

import CoreSyn
import MkCore
import CoreUtils	( exprIsDupable, exprIsExpandable, exprOkForSideEffects )
import CoreFVs		( CoreExprWithFVs, freeVars, freeVarsOf, idRuleAndUnfoldingVars )
import Id		( isOneShotBndr, idType )
import Var
import Type		( isUnLiftedType )
import VarSet
import Util
import UniqFM
import DynFlags
import Outputable
\end{code}

Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.

\begin{code}
floatInwards :: DynFlags -> CoreProgram -> CoreProgram
floatInwards dflags = map fi_top_bind
  where
    fi_top_bind (NonRec binder rhs)
      = NonRec binder (fiExpr dflags [] (freeVars rhs))
    fi_top_bind (Rec pairs)
      = Rec [ (b, fiExpr dflags [] (freeVars rhs)) | (b, rhs) <- pairs ]
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
type FreeVarSet  = IdSet
type BoundVarSet = IdSet

data FloatInBind = FB BoundVarSet FreeVarSet FloatBind
	-- The FreeVarSet is the free variables of the binding.  In the case
	-- of recursive bindings, the set doesn't include the bound
	-- variables.

type FloatInBinds = [FloatInBind]
	-- In reverse dependency order (innermost binder first)

fiExpr :: DynFlags
       -> FloatInBinds      -- Binds we're trying to drop
                            -- as far "inwards" as possible
       -> CoreExprWithFVs   -- Input expr
       -> CoreExpr          -- Result

fiExpr _ to_drop (_, AnnLit lit)     = ASSERT( null to_drop ) Lit lit
fiExpr _ to_drop (_, AnnType ty)     = ASSERT( null to_drop ) Type ty
fiExpr _ to_drop (_, AnnVar v)       = wrapFloats to_drop (Var v)
fiExpr _ to_drop (_, AnnCoercion co) = wrapFloats to_drop (Coercion co)
fiExpr dflags to_drop (_, AnnCast expr (fvs_co, co))
  = wrapFloats (drop_here ++ co_drop) $
    Cast (fiExpr dflags e_drop expr) co
  where
    [drop_here, e_drop, co_drop] = sepBindsByDropPoint dflags False [freeVarsOf expr, fvs_co] to_drop
\end{code}

Applications: we do float inside applications, mainly because we
need to get at all the arguments.  The next simplifier run will
pull out any silly ones.

\begin{code}
fiExpr dflags to_drop (_,AnnApp fun arg@(arg_fvs, ann_arg))
  | noFloatIntoRhs ann_arg  = wrapFloats drop_here $ wrapFloats arg_drop $
                              App (fiExpr dflags fun_drop fun) (fiExpr dflags [] arg)
       -- It's inconvenient to test for an unlifted arg here,
       -- and it really doesn't matter if we float into one
  | otherwise               = wrapFloats drop_here $
                              App (fiExpr dflags fun_drop fun) (fiExpr dflags arg_drop arg)
  where
    [drop_here, fun_drop, arg_drop] 
      = sepBindsByDropPoint dflags False [freeVarsOf fun, arg_fvs] to_drop
\end{code}

Note [Floating in past a lambda group]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

* Hack alert!  We only float in through one-shot lambdas, 
  not (as you might guess) through lone big lambdas.  
  Reason: we float *out* past big lambdas (see the test in the Lam
  case of FloatOut.floatExpr) and we don't want to float straight
  back in again.
  
  It *is* important to float into one-shot lambdas, however;
  see the remarks with noFloatIntoRhs.

So we treat lambda in groups, using the following rule:

 Float in if (a) there is at least one Id, 
         and (b) there are no non-one-shot Ids

 Otherwise drop all the bindings outside the group.

This is what the 'go' function in the AnnLam case is doing.

Urk! if all are tyvars, and we don't float in, we may miss an 
      opportunity to float inside a nested case branch

\begin{code}
fiExpr dflags to_drop lam@(_, AnnLam _ _)
  | okToFloatInside bndrs 	-- Float in
     -- NB: Must line up with noFloatIntoRhs (AnnLam...); see Trac #7088
  = mkLams bndrs (fiExpr dflags to_drop body)

  | otherwise	 	-- Dump it all here
  = wrapFloats to_drop (mkLams bndrs (fiExpr dflags [] body))

  where
    (bndrs, body) = collectAnnBndrs lam
\end{code}

We don't float lets inwards past an SCC.
	ToDo: keep info on current cc, and when passing
	one, if it is not the same, annotate all lets in binds with current
	cc, change current cc to the new one and float binds into expr.

\begin{code}
fiExpr dflags to_drop (_, AnnTick tickish expr)
  | tickishScoped tickish
  =     -- Wimp out for now - we could push values in
    wrapFloats to_drop (Tick tickish (fiExpr dflags [] expr))

  | otherwise
  = Tick tickish (fiExpr dflags to_drop expr)
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

Note [extra_fvs (1): avoid floating into RHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider let x=\y....t... in body.  We do not necessarily want to float 
a binding for t into the RHS, because it'll immediately be floated out
again.  (It won't go inside the lambda else we risk losing work.)
In letrec, we need to be more careful still. We don't want to transform
	let x# = y# +# 1#
	in
	letrec f = \z. ...x#...f...
	in ...
into
	letrec f = let x# = y# +# 1# in \z. ...x#...f... in ...
because now we can't float the let out again, because a letrec
can't have unboxed bindings.

So we make "extra_fvs" which is the rhs_fvs of such bindings, and
arrange to dump bindings that bind extra_fvs before the entire let.

Note [extra_fvs (2): free variables of rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider 
  let x{rule mentioning y} = rhs in body 
Here y is not free in rhs or body; but we still want to dump bindings
that bind y outside the let.  So we augment extra_fvs with the
idRuleAndUnfoldingVars of x.  No need for type variables, hence not using
idFreeVars.


\begin{code}
fiExpr dflags to_drop (_,AnnLet (AnnNonRec id rhs@(rhs_fvs, ann_rhs)) body)
  = fiExpr dflags new_to_drop body
  where
    body_fvs = freeVarsOf body `delVarSet` id

    rule_fvs = idRuleAndUnfoldingVars id	-- See Note [extra_fvs (2): free variables of rules]
    extra_fvs | noFloatIntoRhs ann_rhs
	      || isUnLiftedType (idType id) = rule_fvs `unionVarSet` rhs_fvs
	      | otherwise		    = rule_fvs
	-- See Note [extra_fvs (1): avoid floating into RHS]
	-- No point in floating in only to float straight out again
	-- Ditto ok-for-speculation unlifted RHSs

    [shared_binds, extra_binds, rhs_binds, body_binds] 
	= sepBindsByDropPoint dflags False [extra_fvs, rhs_fvs, body_fvs] to_drop

    new_to_drop = body_binds ++				-- the bindings used only in the body
		  [FB (unitVarSet id) rhs_fvs'
                      (FloatLet (NonRec id rhs'))] ++ 	-- the new binding itself
		  extra_binds ++			-- bindings from extra_fvs
		  shared_binds  			-- the bindings used both in rhs and body

	-- Push rhs_binds into the right hand side of the binding
    rhs'     = fiExpr dflags rhs_binds rhs
    rhs_fvs' = rhs_fvs `unionVarSet` floatedBindsFVs rhs_binds `unionVarSet` rule_fvs
			-- Don't forget the rule_fvs; the binding mentions them!

fiExpr dflags to_drop (_,AnnLet (AnnRec bindings) body)
  = fiExpr dflags new_to_drop body
  where
    (ids, rhss) = unzip bindings
    rhss_fvs = map freeVarsOf rhss
    body_fvs = freeVarsOf body 

	-- See Note [extra_fvs (1,2)]
    rule_fvs = foldr (unionVarSet . idRuleAndUnfoldingVars) emptyVarSet ids
    extra_fvs = rule_fvs `unionVarSet` 
		unionVarSets [ fvs | (fvs, rhs) <- rhss
			     , noFloatIntoRhs rhs ]

    (shared_binds:extra_binds:body_binds:rhss_binds) 
	= sepBindsByDropPoint dflags False (extra_fvs:body_fvs:rhss_fvs) to_drop

    new_to_drop = body_binds ++		-- the bindings used only in the body
		  [FB (mkVarSet ids) rhs_fvs' 
                      (FloatLet (Rec (fi_bind rhss_binds bindings)))] ++
					-- The new binding itself
		  extra_binds ++	-- Note [extra_fvs (1,2)]
		  shared_binds		-- Used in more than one place

    rhs_fvs' = unionVarSets rhss_fvs `unionVarSet`
	       unionVarSets (map floatedBindsFVs rhss_binds) `unionVarSet`
	       rule_fvs		-- Don't forget the rule variables!

    -- Push rhs_binds into the right hand side of the binding
    fi_bind :: [FloatInBinds]	    -- one per "drop pt" conjured w/ fvs_of_rhss
	    -> [(Id, CoreExprWithFVs)]
	    -> [(Id, CoreExpr)]

    fi_bind to_drops pairs
      = [ (binder, fiExpr dflags to_drop rhs) 
	| ((binder, rhs), to_drop) <- zipEqual "fi_bind" pairs to_drops ]
\end{code}

For @Case@, the possible ``drop points'' for the \tr{to_drop}
bindings are: (a)~inside the scrutinee, (b)~inside one of the
alternatives/default [default FVs always {\em first}!].

Floating case expressions inward was added to fix Trac #5658: strict bindings
not floated in. In particular, this change allows array indexing operations,
which have a single DEFAULT alternative without any binders, to be floated
inward. SIMD primops for unpacking SIMD vectors into an unboxed tuple of unboxed
scalars also need to be floated inward, but unpacks have a single non-DEFAULT
alternative that binds the elements of the tuple. We now therefore also support
floating in cases with a single alternative that may bind values.

\begin{code}
fiExpr dflags to_drop (_, AnnCase scrut case_bndr _ [(con,alt_bndrs,rhs)])
  | isUnLiftedType (idType case_bndr)
  , exprOkForSideEffects (deAnnotate scrut)
  = wrapFloats shared_binds $
    fiExpr dflags (case_float : rhs_binds) rhs
  where
    case_float = FB (mkVarSet (case_bndr : alt_bndrs)) scrut_fvs 
                    (FloatCase scrut' case_bndr con alt_bndrs)
    scrut' = fiExpr dflags scrut_binds scrut
    [shared_binds, scrut_binds, rhs_binds]
       = sepBindsByDropPoint dflags False [freeVarsOf scrut, rhs_fvs] to_drop
    rhs_fvs   = freeVarsOf rhs `delVarSetList` (case_bndr : alt_bndrs)
    scrut_fvs = freeVarsOf scrut

fiExpr dflags to_drop (_, AnnCase scrut case_bndr ty alts)
  = wrapFloats drop_here1 $
    wrapFloats drop_here2 $
    Case (fiExpr dflags scrut_drops scrut) case_bndr ty
	 (zipWith fi_alt alts_drops_s alts)
  where
	-- Float into the scrut and alts-considered-together just like App
    [drop_here1, scrut_drops, alts_drops] 
       = sepBindsByDropPoint dflags False [scrut_fvs, all_alts_fvs] to_drop

	-- Float into the alts with the is_case flag set
    (drop_here2 : alts_drops_s) = sepBindsByDropPoint dflags True alts_fvs alts_drops

    scrut_fvs    = freeVarsOf scrut
    alts_fvs     = map alt_fvs alts
    all_alts_fvs = unionVarSets alts_fvs
    alt_fvs (_con, args, rhs) = foldl delVarSet (freeVarsOf rhs) (case_bndr:args)
				-- Delete case_bndr and args from free vars of rhs 
				-- to get free vars of alt

    fi_alt to_drop (con, args, rhs) = (con, args, fiExpr dflags to_drop rhs)

okToFloatInside :: [Var] -> Bool
okToFloatInside bndrs = all ok bndrs
  where
    ok b = not (isId b) || isOneShotBndr b
    -- Push the floats inside there are no non-one-shot value binders

noFloatIntoRhs :: AnnExpr' Var (UniqFM Var) -> Bool
noFloatIntoRhs (AnnLam bndr e) 
   = not (okToFloatInside (bndr:bndrs))
     -- NB: Must line up with fiExpr (AnnLam...); see Trac #7088
   where
     (bndrs, _) = collectAnnBndrs e
	-- IMPORTANT: don't say 'True' for a RHS with a one-shot lambda at the top.
	-- This makes a big difference for things like
	--	f x# = let x = I# x#
	--	       in let j = \() -> ...x...
	--		  in if <condition> then normal-path else j ()
	-- If x is used only in the error case join point, j, we must float the
	-- boxing constructor into it, else we box it every time which is very bad
	-- news indeed.

noFloatIntoRhs rhs = exprIsExpandable (deAnnotate' rhs)	
       -- We'd just float right back out again...
       -- Should match the test in SimplEnv.doFloatFromRhs
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
    :: DynFlags
    -> Bool             -- True <=> is case expression
    -> [FreeVarSet]	    -- One set of FVs per drop point
    -> FloatInBinds 	    -- Candidate floaters
    -> [FloatInBinds]      -- FIRST one is bindings which must not be floated
			    -- inside any drop point; the rest correspond
			    -- one-to-one with the input list of FV sets

-- Every input floater is returned somewhere in the result;
-- none are dropped, not even ones which don't seem to be
-- free in *any* of the drop-point fvs.  Why?  Because, for example,
-- a binding (let x = E in B) might have a specialised version of
-- x (say x') stored inside x, but x' isn't free in E or B.

type DropBox = (FreeVarSet, FloatInBinds)

sepBindsByDropPoint _ _is_case drop_pts []
  = [] : [[] | _ <- drop_pts]	-- cut to the chase scene; it happens

sepBindsByDropPoint dflags is_case drop_pts floaters
  = go floaters (map (\fvs -> (fvs, [])) (emptyVarSet : drop_pts))
  where
    go :: FloatInBinds -> [DropBox] -> [FloatInBinds]
	-- The *first* one in the argument list is the drop_here set
	-- The FloatInBinds in the lists are in the reverse of
	-- the normal FloatInBinds order; that is, they are the right way round!

    go [] drop_boxes = map (reverse . snd) drop_boxes

    go (bind_w_fvs@(FB bndrs bind_fvs bind) : binds) drop_boxes@(here_box : fork_boxes)
	= go binds new_boxes
	where
	  -- "here" means the group of bindings dropped at the top of the fork

	  (used_here : used_in_flags) = [ fvs `intersectsVarSet` bndrs
					| (fvs, _) <- drop_boxes]

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
		       floatIsDupable dflags bind) -- and we can duplicate the binding

	  new_boxes | drop_here = (insert here_box : fork_boxes)
		    | otherwise = (here_box : new_fork_boxes)

	  new_fork_boxes = zipWithEqual "FloatIn.sepBinds" insert_maybe fork_boxes used_in_flags

	  insert :: DropBox -> DropBox
	  insert (fvs,drops) = (fvs `unionVarSet` bind_fvs, bind_w_fvs:drops)

	  insert_maybe box True  = insert box
	  insert_maybe box False = box

    go _ _ = panic "sepBindsByDropPoint/go"


floatedBindsFVs :: FloatInBinds -> FreeVarSet
floatedBindsFVs binds = foldr (unionVarSet . fbFVs) emptyVarSet binds

fbFVs :: FloatInBind -> VarSet
fbFVs (FB _ fvs _) = fvs

wrapFloats :: FloatInBinds -> CoreExpr -> CoreExpr
-- Remember FloatInBinds is in *reverse* dependency order
wrapFloats []               e = e
wrapFloats (FB _ _ fl : bs) e = wrapFloats bs (wrapFloat fl e)

floatIsDupable :: DynFlags -> FloatBind -> Bool
floatIsDupable dflags (FloatCase scrut _ _ _) = exprIsDupable dflags scrut
floatIsDupable dflags (FloatLet (Rec prs))    = all (exprIsDupable dflags . snd) prs
floatIsDupable dflags (FloatLet (NonRec _ r)) = exprIsDupable dflags r
\end{code}
