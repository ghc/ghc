{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

************************************************************************
*                                                                      *
\section[FloatIn]{Floating Inwards pass}
*                                                                      *
************************************************************************

The main purpose of @floatInwards@ is floating into branches of a
case, so that we don't allocate things, save them on the stack, and
then discover that they aren't needed in the chosen branch.
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fprof-auto #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Core.Op.FloatIn ( floatInwards ) where

#include "HsVersions.h"

import GhcPrelude
import GHC.Platform

import GHC.Core
import GHC.Core.Make hiding ( wrapFloats )
import GHC.Driver.Types     ( ModGuts(..) )
import GHC.Core.Utils
import GHC.Core.FVs
import GHC.Core.Op.Monad    ( CoreM )
import GHC.Types.Id         ( isOneShotBndr, idType, isJoinId, isJoinId_maybe )
import GHC.Types.Var
import GHC.Core.Type
import GHC.Types.Var.Set
import Util
import GHC.Driver.Session
import Outputable
-- import Data.List        ( mapAccumL )
import GHC.Types.Basic      ( RecFlag(..), isRec )

{-
Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.
-}

floatInwards :: ModGuts -> CoreM ModGuts
floatInwards pgm@(ModGuts { mg_binds = binds })
  = do { dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; return (pgm { mg_binds = map (fi_top_bind platform) binds }) }
  where
    fi_top_bind platform (NonRec binder rhs)
      = NonRec binder (fiExpr platform [] (freeVars rhs))
    fi_top_bind platform (Rec pairs)
      = Rec [ (b, fiExpr platform [] (freeVars rhs)) | (b, rhs) <- pairs ]


{-
************************************************************************
*                                                                      *
\subsection{Mail from Andr\'e [edited]}
*                                                                      *
************************************************************************

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
GHC.Core.Op.SetLevels.hs module, which tags things with their level numbers.
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

************************************************************************
*                                                                      *
\subsection{Main floating-inwards code}
*                                                                      *
************************************************************************
-}

type FreeVarSet  = DIdSet
type BoundVarSet = DIdSet

data FloatInBind = FB BoundVarSet FreeVarSet FloatBind
        -- The FreeVarSet is the free variables of the binding.  In the case
        -- of recursive bindings, the set doesn't include the bound
        -- variables.

type FloatInBinds = [FloatInBind]
        -- In reverse dependency order (innermost binder first)

fiExpr :: Platform
       -> FloatInBinds      -- Binds we're trying to drop
                            -- as far "inwards" as possible
       -> CoreExprWithFVs   -- Input expr
       -> CoreExpr          -- Result

fiExpr _ to_drop (_, AnnLit lit)     = wrapFloats to_drop (Lit lit)
                                       -- See Note [Dead bindings]
fiExpr _ to_drop (_, AnnType ty)     = ASSERT( null to_drop ) Type ty
fiExpr _ to_drop (_, AnnVar v)       = wrapFloats to_drop (Var v)
fiExpr _ to_drop (_, AnnCoercion co) = wrapFloats to_drop (Coercion co)
fiExpr platform to_drop (_, AnnCast expr (co_ann, co))
  = wrapFloats (drop_here ++ co_drop) $
    Cast (fiExpr platform e_drop expr) co
  where
    [drop_here, e_drop, co_drop]
      = sepBindsByDropPoint platform False
          [freeVarsOf expr, freeVarsOfAnn co_ann]
          to_drop

{-
Applications: we do float inside applications, mainly because we
need to get at all the arguments.  The next simplifier run will
pull out any silly ones.
-}

fiExpr platform to_drop ann_expr@(_,AnnApp {})
  = wrapFloats drop_here $ wrapFloats extra_drop $
    mkTicks ticks $
    mkApps (fiExpr platform fun_drop ann_fun)
           (zipWith (fiExpr platform) arg_drops ann_args)
  where
    (ann_fun, ann_args, ticks) = collectAnnArgsTicks tickishFloatable ann_expr
    fun_ty  = exprType (deAnnotate ann_fun)
    fun_fvs = freeVarsOf ann_fun
    arg_fvs = map freeVarsOf ann_args

    (drop_here : extra_drop : fun_drop : arg_drops)
       = sepBindsByDropPoint platform False
                             (extra_fvs : fun_fvs : arg_fvs)
                             to_drop
         -- Shortcut behaviour: if to_drop is empty,
         -- sepBindsByDropPoint returns a suitable bunch of empty
         -- lists without evaluating extra_fvs, and hence without
         -- peering into each argument

    (_, extra_fvs) = foldl' add_arg (fun_ty, extra_fvs0) ann_args
    extra_fvs0 = case ann_fun of
                   (_, AnnVar _) -> fun_fvs
                   _             -> emptyDVarSet
          -- Don't float the binding for f into f x y z; see Note [Join points]
          -- for why we *can't* do it when f is a join point. (If f isn't a
          -- join point, floating it in isn't especially harmful but it's
          -- useless since the simplifier will immediately float it back out.)

    add_arg :: (Type,FreeVarSet) -> CoreExprWithFVs -> (Type,FreeVarSet)
    add_arg (fun_ty, extra_fvs) (_, AnnType ty)
      = (piResultTy fun_ty ty, extra_fvs)

    add_arg (fun_ty, extra_fvs) (arg_fvs, arg)
      | noFloatIntoArg arg arg_ty
      = (res_ty, extra_fvs `unionDVarSet` arg_fvs)
      | otherwise
      = (res_ty, extra_fvs)
      where
       (arg_ty, res_ty) = splitFunTy fun_ty

{- Note [Dead bindings]
~~~~~~~~~~~~~~~~~~~~~~~
At a literal we won't usually have any floated bindings; the
only way that can happen is if the binding wrapped the literal
/in the original input program/.  e.g.
   case x of { DEFAULT -> 1# }
But, while this may be unusual it is not actually wrong, and it did
once happen (#15696).

Note [Do not destroy the let/app invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Watch out for
   f (x +# y)
We don't want to float bindings into here
   f (case ... of { x -> x +# y })
because that might destroy the let/app invariant, which requires
unlifted function arguments to be ok-for-speculation.

Note [Join points]
~~~~~~~~~~~~~~~~~~
Generally, we don't need to worry about join points - there are places we're
not allowed to float them, but since they can't have occurrences in those
places, we're not tempted.

We do need to be careful about jumps, however:

  joinrec j x y z = ... in
  jump j a b c

Previous versions often floated the definition of a recursive function into its
only non-recursive occurrence. But for a join point, this is a disaster:

  (joinrec j x y z = ... in
  jump j) a b c -- wrong!

Every jump must be exact, so the jump to j must have three arguments. Hence
we're careful not to float into the target of a jump (though we can float into
the arguments just fine).

Note [Floating in past a lambda group]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* We must be careful about floating inside a value lambda.
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

(Join points are handled similarly: a join point is considered one-shot iff
it's non-recursive, so we float only into non-recursive join points.)

Urk! if all are tyvars, and we don't float in, we may miss an
      opportunity to float inside a nested case branch


Note [Floating coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
We could, in principle, have a coercion binding like
   case f x of co { DEFAULT -> e1 e2 }
It's not common to have a function that returns a coercion, but nothing
in Core prohibits it.  If so, 'co' might be mentioned in e1 or e2
/only in a type/.  E.g. suppose e1 was
  let (x :: Int |> co) = blah in blah2


But, with coercions appearing in types, there is a complication: we
might be floating in a "strict let" -- that is, a case. Case expressions
mention their return type. We absolutely can't float a coercion binding
inward to the point that the type of the expression it's about to wrap
mentions the coercion. So we include the union of the sets of free variables
of the types of all the drop points involved. If any of the floaters
bind a coercion variable mentioned in any of the types, that binder must
be dropped right away.

-}

fiExpr platform to_drop lam@(_, AnnLam _ _)
  | noFloatIntoLam bndrs       -- Dump it all here
     -- NB: Must line up with noFloatIntoRhs (AnnLam...); see #7088
  = wrapFloats to_drop (mkLams bndrs (fiExpr platform [] body))

  | otherwise           -- Float inside
  = mkLams bndrs (fiExpr platform to_drop body)

  where
    (bndrs, body) = collectAnnBndrs lam

{-
We don't float lets inwards past an SCC.
        ToDo: keep info on current cc, and when passing
        one, if it is not the same, annotate all lets in binds with current
        cc, change current cc to the new one and float binds into expr.
-}

fiExpr platform to_drop (_, AnnTick tickish expr)
  | tickish `tickishScopesLike` SoftScope
  = Tick tickish (fiExpr platform to_drop expr)

  | otherwise -- Wimp out for now - we could push values in
  = wrapFloats to_drop (Tick tickish (fiExpr platform [] expr))

{-
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
-}

fiExpr platform to_drop (_,AnnLet bind body)
  = fiExpr platform (after ++ new_float : before) body
           -- to_drop is in reverse dependency order
  where
    (before, new_float, after) = fiBind platform to_drop bind body_fvs
    body_fvs    = freeVarsOf body

{- Note [Floating primops]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We try to float-in a case expression over an unlifted type.  The
motivating example was #5658: in particular, this change allows
array indexing operations, which have a single DEFAULT alternative
without any binders, to be floated inward.

SIMD primops for unpacking SIMD vectors into an unboxed tuple of unboxed
scalars also need to be floated inward, but unpacks have a single non-DEFAULT
alternative that binds the elements of the tuple. We now therefore also support
floating in cases with a single alternative that may bind values.

But there are wrinkles

* Which unlifted cases do we float?
  See Note [PrimOp can_fail and has_side_effects] in PrimOp.hs which explains:
   - We can float in can_fail primops (which concerns imprecise exceptions),
     but we can't float them out.
   - But we can float a has_side_effects primop, but NOT inside a lambda,
     so for now we don't float them at all. Hence exprOkForSideEffects.
   - Throwing precise exceptions is a special case of the previous point: We
     may /never/ float in a call to (something that ultimately calls)
     'raiseIO#', in which case its strictness type has a Divergence of topDiv
     or exnDiv.

* Because we can float can-fail primops (array indexing, division) inwards
  but not outwards, we must be careful not to transform
     case a /# b of r -> f (F# r)
  ===>
    f (case a /# b of r -> F# r)
  because that creates a new thunk that wasn't there before.  And
  because it can't be floated out (can_fail), the thunk will stay
  there.  Disaster!  (This happened in nofib 'simple' and 'scs'.)

  Solution: only float cases into the branches of other cases, and
  not into the arguments of an application, or the RHS of a let. This
  is somewhat conservative, but it's simple.  And it still hits the
  cases like #5658.   This is implemented in sepBindsByJoinPoint;
  if is_case is False we dump all floating cases right here.

* #14511 is another example of why we want to restrict float-in
  of case-expressions.  Consider
     case indexArray# a n of (# r #) -> writeArray# ma i (f r)
  Now, floating that indexing operation into the (f r) thunk will
  not create any new thunks, but it will keep the array 'a' alive
  for much longer than the programmer expected.

  So again, not floating a case into a let or argument seems like
  the Right Thing

For @Case@, the possible drop points for the 'to_drop'
bindings are:
  (a) inside the scrutinee
  (b) inside one of the alternatives/default (default FVs always /first/!).

-}

fiExpr platform to_drop (_, AnnCase scrut case_bndr _ [(con,alt_bndrs,rhs)])
  | isUnliftedType (idType case_bndr)
  , exprOkForSideEffects (deAnnotate scrut)
      -- See Note [Floating primops]
  = wrapFloats shared_binds $
    fiExpr platform (case_float : rhs_binds) rhs
  where
    case_float = FB (mkDVarSet (case_bndr : alt_bndrs)) scrut_fvs
                    (FloatCase scrut' case_bndr con alt_bndrs)
    scrut'     = fiExpr platform scrut_binds scrut
    rhs_fvs    = freeVarsOf rhs `delDVarSetList` (case_bndr : alt_bndrs)
    scrut_fvs  = freeVarsOf scrut

    [shared_binds, scrut_binds, rhs_binds]
       = sepBindsByDropPoint platform False
           [scrut_fvs, rhs_fvs]
           to_drop

fiExpr platform to_drop (_, AnnCase scrut case_bndr ty alts)
  = wrapFloats drop_here1 $
    wrapFloats drop_here2 $
    Case (fiExpr platform scrut_drops scrut) case_bndr ty
         (zipWith fi_alt alts_drops_s alts)
  where
        -- Float into the scrut and alts-considered-together just like App
    [drop_here1, scrut_drops, alts_drops]
       = sepBindsByDropPoint platform False
           [scrut_fvs, all_alts_fvs]
           to_drop

        -- Float into the alts with the is_case flag set
    (drop_here2 : alts_drops_s)
      | [ _ ] <- alts = [] : [alts_drops]
      | otherwise     = sepBindsByDropPoint platform True alts_fvs alts_drops

    scrut_fvs    = freeVarsOf scrut
    alts_fvs     = map alt_fvs alts
    all_alts_fvs = unionDVarSets alts_fvs
    alt_fvs (_con, args, rhs)
      = foldl' delDVarSet (freeVarsOf rhs) (case_bndr:args)
           -- Delete case_bndr and args from free vars of rhs
           -- to get free vars of alt

    fi_alt to_drop (con, args, rhs) = (con, args, fiExpr platform to_drop rhs)

------------------
fiBind :: Platform
       -> FloatInBinds      -- Binds we're trying to drop
                            -- as far "inwards" as possible
       -> CoreBindWithFVs   -- Input binding
       -> DVarSet           -- Free in scope of binding
       -> ( FloatInBinds    -- Land these before
          , FloatInBind     -- The binding itself
          , FloatInBinds)   -- Land these after

fiBind platform to_drop (AnnNonRec id ann_rhs@(rhs_fvs, rhs)) body_fvs
  = ( extra_binds ++ shared_binds          -- Land these before
                                           -- See Note [extra_fvs (1,2)]
    , FB (unitDVarSet id) rhs_fvs'         -- The new binding itself
          (FloatLet (NonRec id rhs'))
    , body_binds )                         -- Land these after

  where
    body_fvs2 = body_fvs `delDVarSet` id

    rule_fvs = bndrRuleAndUnfoldingVarsDSet id        -- See Note [extra_fvs (2): free variables of rules]
    extra_fvs | noFloatIntoRhs NonRecursive id rhs
              = rule_fvs `unionDVarSet` rhs_fvs
              | otherwise
              = rule_fvs
        -- See Note [extra_fvs (1): avoid floating into RHS]
        -- No point in floating in only to float straight out again
        -- We *can't* float into ok-for-speculation unlifted RHSs
        -- But do float into join points

    [shared_binds, extra_binds, rhs_binds, body_binds]
        = sepBindsByDropPoint platform False
            [extra_fvs, rhs_fvs, body_fvs2]
            to_drop

        -- Push rhs_binds into the right hand side of the binding
    rhs'     = fiRhs platform rhs_binds id ann_rhs
    rhs_fvs' = rhs_fvs `unionDVarSet` floatedBindsFVs rhs_binds `unionDVarSet` rule_fvs
                        -- Don't forget the rule_fvs; the binding mentions them!

fiBind platform to_drop (AnnRec bindings) body_fvs
  = ( extra_binds ++ shared_binds
    , FB (mkDVarSet ids) rhs_fvs'
         (FloatLet (Rec (fi_bind rhss_binds bindings)))
    , body_binds )
  where
    (ids, rhss) = unzip bindings
    rhss_fvs = map freeVarsOf rhss

        -- See Note [extra_fvs (1,2)]
    rule_fvs = mapUnionDVarSet bndrRuleAndUnfoldingVarsDSet ids
    extra_fvs = rule_fvs `unionDVarSet`
                unionDVarSets [ rhs_fvs | (bndr, (rhs_fvs, rhs)) <- bindings
                              , noFloatIntoRhs Recursive bndr rhs ]

    (shared_binds:extra_binds:body_binds:rhss_binds)
        = sepBindsByDropPoint platform False
            (extra_fvs:body_fvs:rhss_fvs)
            to_drop

    rhs_fvs' = unionDVarSets rhss_fvs `unionDVarSet`
               unionDVarSets (map floatedBindsFVs rhss_binds) `unionDVarSet`
               rule_fvs         -- Don't forget the rule variables!

    -- Push rhs_binds into the right hand side of the binding
    fi_bind :: [FloatInBinds]       -- one per "drop pt" conjured w/ fvs_of_rhss
            -> [(Id, CoreExprWithFVs)]
            -> [(Id, CoreExpr)]

    fi_bind to_drops pairs
      = [ (binder, fiRhs platform to_drop binder rhs)
        | ((binder, rhs), to_drop) <- zipEqual "fi_bind" pairs to_drops ]

------------------
fiRhs :: Platform -> FloatInBinds -> CoreBndr -> CoreExprWithFVs -> CoreExpr
fiRhs platform to_drop bndr rhs
  | Just join_arity <- isJoinId_maybe bndr
  , let (bndrs, body) = collectNAnnBndrs join_arity rhs
  = mkLams bndrs (fiExpr platform to_drop body)
  | otherwise
  = fiExpr platform to_drop rhs

------------------
noFloatIntoLam :: [Var] -> Bool
noFloatIntoLam bndrs = any bad bndrs
  where
    bad b = isId b && not (isOneShotBndr b)
    -- Don't float inside a non-one-shot lambda

noFloatIntoRhs :: RecFlag -> Id -> CoreExprWithFVs' -> Bool
-- ^ True if it's a bad idea to float bindings into this RHS
noFloatIntoRhs is_rec bndr rhs
  | isJoinId bndr
  = isRec is_rec -- Joins are one-shot iff non-recursive

  | otherwise
  = noFloatIntoArg rhs (idType bndr)

noFloatIntoArg :: CoreExprWithFVs' -> Type -> Bool
noFloatIntoArg expr expr_ty
  | isUnliftedType expr_ty
  = True  -- See Note [Do not destroy the let/app invariant]

   | AnnLam bndr e <- expr
   , (bndrs, _) <- collectAnnBndrs e
   =  noFloatIntoLam (bndr:bndrs)  -- Wrinkle 1 (a)
   || all isTyVar (bndr:bndrs)     -- Wrinkle 1 (b)
      -- See Note [noFloatInto considerations] wrinkle 2

  | otherwise  -- Note [noFloatInto considerations] wrinkle 2
  = exprIsTrivial deann_expr || exprIsHNF deann_expr
  where
    deann_expr = deAnnotate' expr

{- Note [noFloatInto considerations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When do we want to float bindings into
   - noFloatIntoRHs: the RHS of a let-binding
   - noFloatIntoArg: the argument of a function application

Definitely don't float in if it has unlifted type; that
would destroy the let/app invariant.

* Wrinkle 1: do not float in if
     (a) any non-one-shot value lambdas
  or (b) all type lambdas
  In both cases we'll float straight back out again
  NB: Must line up with fiExpr (AnnLam...); see #7088

  (a) is important: we /must/ float into a one-shot lambda group
  (which includes join points). This makes a big difference
  for things like
     f x# = let x = I# x#
            in let j = \() -> ...x...
               in if <condition> then normal-path else j ()
  If x is used only in the error case join point, j, we must float the
  boxing constructor into it, else we box it every time which is very
  bad news indeed.

* Wrinkle 2: for RHSs, do not float into a HNF; we'll just float right
  back out again... not tragic, but a waste of time.

  For function arguments we will still end up with this
  in-then-out stuff; consider
    letrec x = e in f x
  Here x is not a HNF, so we'll produce
    f (letrec x = e in x)
  which is OK... it's not that common, and we'll end up
  floating out again, in CorePrep if not earlier.
  Still, we use exprIsTrivial to catch this case (sigh)


************************************************************************
*                                                                      *
\subsection{@sepBindsByDropPoint@}
*                                                                      *
************************************************************************

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
-}

-- pprFIB :: FloatInBinds -> SDoc
-- pprFIB fibs = text "FIB:" <+> ppr [b | FB _ _ b <- fibs]

sepBindsByDropPoint
    :: Platform
    -> Bool                -- True <=> is case expression
    -> [FreeVarSet]        -- One set of FVs per drop point
                           -- Always at least two long!
    -> FloatInBinds        -- Candidate floaters
    -> [FloatInBinds]      -- FIRST one is bindings which must not be floated
                           -- inside any drop point; the rest correspond
                           -- one-to-one with the input list of FV sets

-- Every input floater is returned somewhere in the result;
-- none are dropped, not even ones which don't seem to be
-- free in *any* of the drop-point fvs.  Why?  Because, for example,
-- a binding (let x = E in B) might have a specialised version of
-- x (say x') stored inside x, but x' isn't free in E or B.

type DropBox = (FreeVarSet, FloatInBinds)

sepBindsByDropPoint platform is_case drop_pts floaters
  | null floaters  -- Shortcut common case
  = [] : [[] | _ <- drop_pts]

  | otherwise
  = ASSERT( drop_pts `lengthAtLeast` 2 )
    go floaters (map (\fvs -> (fvs, [])) (emptyDVarSet : drop_pts))
  where
    n_alts = length drop_pts

    go :: FloatInBinds -> [DropBox] -> [FloatInBinds]
        -- The *first* one in the argument list is the drop_here set
        -- The FloatInBinds in the lists are in the reverse of
        -- the normal FloatInBinds order; that is, they are the right way round!

    go [] drop_boxes = map (reverse . snd) drop_boxes

    go (bind_w_fvs@(FB bndrs bind_fvs bind) : binds) drop_boxes@(here_box : fork_boxes)
        = go binds new_boxes
        where
          -- "here" means the group of bindings dropped at the top of the fork

          (used_here : used_in_flags) = [ fvs `intersectsDVarSet` bndrs
                                        | (fvs, _) <- drop_boxes]

          drop_here = used_here || cant_push

          n_used_alts = count id used_in_flags -- returns number of Trues in list.

          cant_push
            | is_case   = n_used_alts == n_alts   -- Used in all, don't push
                                                  -- Remember n_alts > 1
                          || (n_used_alts > 1 && not (floatIsDupable platform bind))
                             -- floatIsDupable: see Note [Duplicating floats]

            | otherwise = floatIsCase bind || n_used_alts > 1
                             -- floatIsCase: see Note [Floating primops]

          new_boxes | drop_here = (insert here_box : fork_boxes)
                    | otherwise = (here_box : new_fork_boxes)

          new_fork_boxes = zipWithEqual "FloatIn.sepBinds" insert_maybe
                                        fork_boxes used_in_flags

          insert :: DropBox -> DropBox
          insert (fvs,drops) = (fvs `unionDVarSet` bind_fvs, bind_w_fvs:drops)

          insert_maybe box True  = insert box
          insert_maybe box False = box

    go _ _ = panic "sepBindsByDropPoint/go"


{- Note [Duplicating floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For case expressions we duplicate the binding if it is reasonably
small, and if it is not used in all the RHSs This is good for
situations like
     let x = I# y in
     case e of
       C -> error x
       D -> error x
       E -> ...not mentioning x...

If the thing is used in all RHSs there is nothing gained,
so we don't duplicate then.
-}

floatedBindsFVs :: FloatInBinds -> FreeVarSet
floatedBindsFVs binds = mapUnionDVarSet fbFVs binds

fbFVs :: FloatInBind -> DVarSet
fbFVs (FB _ fvs _) = fvs

wrapFloats :: FloatInBinds -> CoreExpr -> CoreExpr
-- Remember FloatInBinds is in *reverse* dependency order
wrapFloats []               e = e
wrapFloats (FB _ _ fl : bs) e = wrapFloats bs (wrapFloat fl e)

floatIsDupable :: Platform -> FloatBind -> Bool
floatIsDupable platform (FloatCase scrut _ _ _) = exprIsDupable platform scrut
floatIsDupable platform (FloatLet (Rec prs))    = all (exprIsDupable platform . snd) prs
floatIsDupable platform (FloatLet (NonRec _ r)) = exprIsDupable platform r

floatIsCase :: FloatBind -> Bool
floatIsCase (FloatCase {}) = True
floatIsCase (FloatLet {})  = False
