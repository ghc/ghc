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


{-# OPTIONS_GHC -fprof-auto #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Core.Opt.FloatIn ( floatInwards ) where

import GHC.Prelude
import GHC.Platform

import GHC.Core
import GHC.Core.Unfold( ExprSize(..), sizeExpr,
                        UnfoldingOpts(..), defaultUnfoldingOpts )
import GHC.Core.Opt.Arity( isOneShotBndr )
import GHC.Core.Opt.OccurAnal( occurAnalyseExpr )
-- import GHC.Core.Opt.Simplify.Inline( smallEnoughToInline )
import GHC.Core.Make hiding ( wrapFloats )
import GHC.Core.Utils
import GHC.Core.FVs
import GHC.Core.Type

import GHC.Types.Basic      ( RecFlag(..), isRec, isOneOcc )
import GHC.Types.Id         ( idType, isJoinId, idJoinPointHood, idDemandInfo, idOccInfo )
import GHC.Types.Demand     ( isStrUsedDmd )
import GHC.Types.Tickish
import GHC.Types.Var
import GHC.Types.Var.Set

import GHC.Utils.Misc
import GHC.Utils.Panic.Plain

import GHC.Utils.Outputable

import Data.List        ( mapAccumL )

{-
Top-level interface function, @floatInwards@.  Note that we do not
actually float any bindings downwards from the top-level.
-}

floatInwards :: Platform -> CoreProgram -> CoreProgram
floatInwards platform binds = map (fi_top_bind platform) binds
  where
    fi_top_bind platform (NonRec binder rhs)
      = NonRec binder (fiExpr platform [] (preprocess rhs))
    fi_top_bind platform (Rec pairs)
      = Rec [ (b, fiExpr platform [] (preprocess rhs)) | (b, rhs) <- pairs ]

    preprocess rhs = freeVars (occurAnalyseExpr rhs)

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
GHC.Core.Opt.SetLevels module, which tags things with their level numbers.
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

type FreeVarSet  = DVarSet
type BoundVarSet = DIdSet

data FloatInBind = FB BoundVarSet FreeVarSet FloatBind
        -- The FreeVarSet is the free variables of the binding.  In the case
        -- of recursive bindings, the set doesn't include the bound
        -- variables.

type FloatInBinds    = [FloatInBind] -- In normal dependency order
                                     --    (outermost binder first)
type RevFloatInBinds = [FloatInBind] -- In reverse dependency order
                                     --    (innermost binder first)

instance Outputable FloatInBind where
  ppr (FB bvs fvs _) = text "FB" <> braces (sep [ text "bndrs =" <+> ppr bvs
                                                , text "fvs =" <+> ppr fvs ])

fiExpr :: Platform
       -> RevFloatInBinds   -- Binds we're trying to drop
                            -- as far "inwards" as possible
       -> CoreExprWithFVs   -- Input expr
       -> CoreExpr          -- Result

fiExpr _ to_drop (_, AnnLit lit)     = wrapFloats to_drop (Lit lit)
                                       -- See Note [Dead bindings]
fiExpr _ to_drop (_, AnnType ty)     = assert (null to_drop) $ Type ty
fiExpr _ to_drop (_, AnnVar v)       = wrapFloats to_drop (Var v)
fiExpr _ to_drop (_, AnnCoercion co) = wrapFloats to_drop (Coercion co)
fiExpr platform to_drop (_, AnnCast expr (co_ann, co))
  = wrapFloats drop_here $
    Cast (fiExpr platform e_drop expr) co
  where
    (drop_here, [e_drop])
      = sepBindsByDropPoint platform False to_drop
          (freeVarsOfAnn co_ann) [freeVarsOf expr]

{-
Applications: we do float inside applications, mainly because we
need to get at all the arguments.  The next simplifier run will
pull out any silly ones.
-}

fiExpr platform to_drop ann_expr@(_,AnnApp {})
  = wrapFloats drop_here $
    mkTicks ticks $
    mkApps (fiExpr platform fun_drop ann_fun)
           (zipWithEqual "fiExpr" (fiExpr platform) arg_drops ann_args)
           -- use zipWithEqual, we should have
           -- length ann_args = length arg_fvs = length arg_drops
  where
    (ann_fun, ann_args, ticks) = collectAnnArgsTicks tickishFloatable ann_expr
    fun_fvs = freeVarsOf ann_fun

    (drop_here, fun_drop : arg_drops)
       = sepBindsByDropPoint platform False to_drop
                             here_fvs (fun_fvs : arg_fvs)

         -- Shortcut behaviour: if to_drop is empty,
         -- sepBindsByDropPoint returns a suitable bunch of empty
         -- lists without evaluating extra_fvs, and hence without
         -- peering into each argument

    (here_fvs, arg_fvs) = mapAccumL add_arg here_fvs0 ann_args
    here_fvs0 = case ann_fun of
                   (_, AnnVar _) -> fun_fvs
                   _             -> emptyDVarSet
          -- Don't float the binding for f into f x y z; see Note [Join points]
          -- for why we *can't* do it when f is a join point. (If f isn't a
          -- join point, floating it in isn't especially harmful but it's
          -- useless since the simplifier will immediately float it back out.)

    add_arg :: FreeVarSet -> CoreExprWithFVs -> (FreeVarSet,FreeVarSet)
    -- We can't float into some arguments, so put them into the here_fvs
    add_arg here_fvs (arg_fvs, arg)
      | noFloatIntoArg arg = (here_fvs `unionDVarSet` arg_fvs, emptyDVarSet)
      | otherwise          = (here_fvs, arg_fvs)

{- Note [Dead bindings]
~~~~~~~~~~~~~~~~~~~~~~~
At a literal we won't usually have any floated bindings; the
only way that can happen is if the binding wrapped the literal
/in the original input program/.  e.g.
   case x of { DEFAULT -> 1# }
But, while this may be unusual it is not actually wrong, and it did
once happen (#15696).

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

Floating in can /enhance/ join points.  Consider this (#3458)
    f2 x = let g :: Int -> Int
               g y = if y==0 then y+x else g (y-1)
           in case g x of
                0 -> True
                _ -> False

Here `g` is not a join point. But if we float inwards it becomes one!  We
float in; the occurrence analyser identifies `g` as a join point; the Simplifier
retains that property, so we get
    f2 x = case (joinrec
                    g y = if y==0 then y+x else g (y-1)
                 in jump g x) of
              0 -> True
              _ -> False

Now that outer case gets pushed into the RHS of the joinrec, giving
    f2 x = joinrec g y = if y==0
                         then case y+x of { 0 -> True; _ -> False }
                         else jump g (y-1)
           in jump g x
Nice!

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

Note [Shadowing and name capture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
    let x = y+1 in
    case p of
       (y:ys) -> ...x...
       [] -> blah
It is obviously bogus for FloatIn to transform to
    case p of
       (y:ys) -> ...(let x = y+1 in x)...
       [] -> blah
because the y is captured.  This doesn't happen much, because shadowing is
rare (see Note [Shadowing in Core]), but it did happen in #22662.

One solution would be to clone as we go.  But a simpler one is this:

  at a binding site (like that for (y:ys) above), abandon float-in for
  any floating bindings that mention the binders (y, ys in this case)

We achieve that by calling sepBindsByDropPoint with the binders in
the "used-here" set:

* In fiExpr (AnnLam ...).  For the body there is no need to delete
  the lambda-binders from the body_fvs, because any bindings that
  mention these binders will be dropped here anyway.

* In fiExpr (AnnCase ...). Remember to include the case_bndr in the
  binders.  Again, no need to delete the alt binders from the rhs
  free vars, because any bindings mentioning them will be dropped
  here unconditionally.
-}

fiExpr platform to_drop lam@(_, AnnLam _ _)
  | noFloatIntoLam bndrs       -- Dump it all here
     -- NB: Must line up with noFloatIntoRhs (AnnLam...); see #7088
  = wrapFloats to_drop (mkLams bndrs (fiExpr platform [] body))

  | otherwise           -- Float inside
  = wrapFloats drop_here $
    mkLams bndrs (fiExpr platform body_drop body)

  where
    (bndrs, body) = collectAnnBndrs lam
    body_fvs      = freeVarsOf body

    -- Why sepBindsByDropPoint? Because of potential capture
    -- See Note [Shadowing and name capture]
    (drop_here, [body_drop]) = sepBindsByDropPoint platform False to_drop
                                  (mkDVarSet bndrs) [body_fvs]

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

Note [extra_fvs (1)]: avoid floating into RHS
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

Note [extra_fvs (2)]: free variables of rules
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

In particular, we want to be able to transform

  case indexIntArray# arr i of vi {
    __DEFAULT -> case <# j n of _ {
      __DEFAULT -> False
      1# -> case indexIntArray# arr j of vj {
        __DEFAULT -> ... vi ... vj ...
      }
    }
  }

by floating in `indexIntArray# arr i` to produce

  case <# j n of _ {
    __DEFAULT -> False
    1# -> case indexIntArray# arr i of vi {
      __DEFAULT -> case indexIntArray# arr j of vj {
        __DEFAULT -> ... vi ... vj ...
      }
    }
  }

...which skips the `indexIntArray# arr i` call entirely in the out-of-bounds branch.

SIMD primops for unpacking SIMD vectors into an unboxed tuple of unboxed
scalars also need to be floated inward, but unpacks have a single non-DEFAULT
alternative that binds the elements of the tuple. We now therefore also support
floating in cases with a single alternative that may bind values.

But there are wrinkles

* Which unlifted cases do we float?
  See Note [Transformations affected by primop effects] in GHC.Builtin.PrimOps
  which explains:
   - We can float in or discard CanFail primops, but we can't float them out.
   - We don't want to discard a synchronous exception or side effect
     so we don't float those at all. Hence exprOkToDiscard.
   - Throwing precise exceptions is a special case of the previous point: We
     may /never/ float in a call to (something that ultimately calls)
     'raiseIO#'.
     See Note [Precise exceptions and strictness analysis] in GHC.Types.Demand.

* Because we can float can-fail primops (array indexing, division) inwards
  but not outwards, we must be careful not to transform
     case a /# b of r -> f (F# r)
  ===>
    f (case a /# b of r -> F# r)
  because that creates a new thunk that wasn't there before.  And
  because it can't be floated out (CanFail), the thunk will stay
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

fiExpr platform to_drop (_, AnnCase scrut case_bndr _ [AnnAlt con alt_bndrs rhs])
  | isUnliftedType (idType case_bndr)
     -- binders have a fixed RuntimeRep so it's OK to call isUnliftedType
  , exprOkToDiscard (deAnnotate scrut)
      -- See Note [Floating primops]
  = wrapFloats shared_binds $
    fiExpr platform (case_float : rhs_binds) rhs
  where
    case_float = FB all_bndrs scrut_fvs
                    (FloatCase scrut' case_bndr con alt_bndrs)
    scrut'     = fiExpr platform scrut_binds scrut
    rhs_fvs    = freeVarsOf rhs    -- No need to delete alt_bndrs
    scrut_fvs  = freeVarsOf scrut  -- See Note [Shadowing and name capture]
    all_bndrs  = mkDVarSet alt_bndrs `extendDVarSet` case_bndr

    (shared_binds, [scrut_binds, rhs_binds])
       = sepBindsByDropPoint platform False to_drop
                     all_bndrs [scrut_fvs, rhs_fvs]

fiExpr platform to_drop (_, AnnCase scrut case_bndr ty alts)
  = wrapFloats drop_here1 $
    wrapFloats drop_here2 $
    Case (fiExpr platform scrut_drops scrut) case_bndr ty
         (zipWithEqual "fiExpr" fi_alt alts_drops_s alts)
         -- use zipWithEqual, we should have length alts_drops_s = length alts
  where
        -- Float into the scrut and alts-considered-together just like App
    (drop_here1, [scrut_drops, alts_drops])
       = sepBindsByDropPoint platform False to_drop
             all_alt_bndrs [scrut_fvs, all_alt_fvs]
             -- all_alt_bndrs: see Note [Shadowing and name capture]

    -- Float into the alts with the is_case flag set
    -- Efficiency short-cut for the common case of a single alternative,
    --   e.g.  case e of I# x -> blah
    -- In that case just float in unconditionally.
    (drop_here2, alts_drops_s)
       = case alts of
            [_] -> ([], [alts_drops])
            _   -> sepBindsByDropPoint platform True alts_drops emptyDVarSet alts_fvs

    scrut_fvs = freeVarsOf scrut

    all_alt_bndrs = foldr (unionDVarSet . ann_alt_bndrs) (unitDVarSet case_bndr) alts
    ann_alt_bndrs (AnnAlt _ bndrs _) = mkDVarSet bndrs

    alts_fvs :: [DVarSet]
    alts_fvs = [freeVarsOf rhs | AnnAlt _ _ rhs <- alts]
               -- No need to delete binders
               -- See Note [Shadowing and name capture]

    all_alt_fvs :: DVarSet
    all_alt_fvs = foldr unionDVarSet (unitDVarSet case_bndr) alts_fvs

    fi_alt to_drop (AnnAlt con args rhs) = Alt con args (fiExpr platform to_drop rhs)

------------------
fiBind :: Platform
       -> RevFloatInBinds    -- Binds we're trying to drop
                             -- as far "inwards" as possible
       -> CoreBindWithFVs    -- Input binding
       -> DVarSet            -- Free in scope of binding
       -> ( RevFloatInBinds  -- Land these before
          , FloatInBind      -- The binding itself
          , RevFloatInBinds) -- Land these after

fiBind platform to_drop (AnnNonRec id ann_rhs@(rhs_fvs, rhs)) body_fvs
  = ( shared_binds          -- Land these before
                            -- See Note [extra_fvs (1)] and Note [extra_fvs (2)]
    , FB (unitDVarSet id) rhs_fvs'         -- The new binding itself
          (FloatLet (NonRec id rhs'))
    , body_binds )                         -- Land these after

  where
    body_fvs2 = body_fvs `delDVarSet` id

    rule_fvs = bndrRuleAndUnfoldingVarsDSet id        -- See Note [extra_fvs (2)]
    extra_fvs | noFloatIntoRhs NonRecursive id rhs
              = rule_fvs `unionDVarSet` rhs_fvs
              | otherwise
              = rule_fvs
        -- See Note [extra_fvs (1)]
        -- No point in floating in only to float straight out again
        -- We *can't* float into ok-for-speculation unlifted RHSs
        -- But do float into join points

    (shared_binds, [rhs_binds, body_binds])
        = sepBindsByDropPoint platform False to_drop
                      extra_fvs [rhs_fvs, body_fvs2]

        -- Push rhs_binds into the right hand side of the binding
    rhs'     = fiRhs platform rhs_binds id ann_rhs
    rhs_fvs' = rhs_fvs `unionDVarSet` floatedBindsFVs rhs_binds `unionDVarSet` rule_fvs
                        -- Don't forget the rule_fvs; the binding mentions them!

fiBind platform to_drop (AnnRec bindings) body_fvs
  = ( shared_binds
    , FB (mkDVarSet ids) rhs_fvs'
         (FloatLet (Rec (fi_bind rhss_binds bindings)))
    , body_binds )
  where
    (ids, rhss) = unzip bindings
    rhss_fvs = map freeVarsOf rhss

        -- See Note [extra_fvs (1)] and Note [extra_fvs (2)]
    rule_fvs = mapUnionDVarSet bndrRuleAndUnfoldingVarsDSet ids
    extra_fvs = rule_fvs `unionDVarSet`
                unionDVarSets [ rhs_fvs | (bndr, (rhs_fvs, rhs)) <- bindings
                              , noFloatIntoRhs Recursive bndr rhs ]

    (shared_binds, body_binds:rhss_binds)
        = sepBindsByDropPoint platform False to_drop
                       extra_fvs (body_fvs:rhss_fvs)

    rhs_fvs' = unionDVarSets rhss_fvs `unionDVarSet`
               unionDVarSets (map floatedBindsFVs rhss_binds) `unionDVarSet`
               rule_fvs         -- Don't forget the rule variables!

    -- Push rhs_binds into the right hand side of the binding
    fi_bind :: [RevFloatInBinds]   -- One per "drop pt" conjured w/ fvs_of_rhss
            -> [(Id, CoreExprWithFVs)]
            -> [(Id, CoreExpr)]

    fi_bind to_drops pairs
      = [ (binder, fiRhs platform to_drop binder rhs)
        | ((binder, rhs), to_drop) <- zipEqual "fi_bind" pairs to_drops ]

------------------
fiRhs :: Platform -> RevFloatInBinds -> CoreBndr -> CoreExprWithFVs -> CoreExpr
fiRhs platform to_drop bndr rhs
  | JoinPoint join_arity <- idJoinPointHood bndr
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

  | definitelyUnliftedType (idType bndr)
  = True  -- Preserve let-can-float invariant, see Note [noFloatInto considerations]

  | otherwise
  = noFloatIntoArg rhs

noFloatIntoArg :: CoreExprWithFVs' -> Bool
noFloatIntoArg expr
   | AnnLam bndr e <- expr
   , (bndrs, _) <- collectAnnBndrs e
   =  noFloatIntoLam (bndr:bndrs)  -- Wrinkle 1 (a)
   || all isTyVar (bndr:bndrs)     -- Wrinkle 1 (b)
      -- See Note [noFloatInto considerations] wrinkle 2

  | otherwise  -- See Note [noFloatInto considerations] wrinkle 2
  = exprIsTrivial deann_expr -- || exprIsHNF deann_expr
      -- let x = e in Just (Just (x+1))
      -- here we want to float in!
  where
    deann_expr = deAnnotate' expr

{- Note [noFloatInto considerations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When do we want to float bindings into
   - noFloatIntoRhs: the RHS of a let-binding
   - noFloatIntoArg: the argument of a function application

Definitely don't float into RHS if it has unlifted type;
that would destroy the let-can-float invariant.

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

-- pprFIB :: RevFloatInBinds -> SDoc
-- pprFIB fibs = text "FIB:" <+> ppr [b | FB _ _ b <- fibs]

sepBindsByDropPoint
    :: Platform
    -> Bool                  -- True <=> is case expression
    -> RevFloatInBinds       -- Candidate floaters
    -> FreeVarSet            -- here_fvs: if these vars are free in a binding,
                             --   don't float that binding inside any drop point
    -> [FreeVarSet]          -- fork_fvs: one set of FVs per drop point
    -> ( RevFloatInBinds     -- Bindings which must not be floated inside
       , [RevFloatInBinds] ) -- Corresponds 1-1 with the input list of FV sets

-- Every input floater is returned somewhere in the result;
-- none are dropped, not even ones which don't seem to be
-- free in *any* of the drop-point fvs.  Why?  Because, for example,
-- a binding (let x = E in B) might have a specialised version of
-- x (say x') stored inside x, but x' isn't free in E or B.
--
-- The here_fvs argument is used for two things:
-- * Avoid shadowing bugs: see Note [Shadowing and name capture]
-- * Drop some of the bindings at the top, e.g. of an application

type DropBox = (FreeVarSet, FloatInBinds)

dropBoxFloats :: DropBox -> RevFloatInBinds
dropBoxFloats (_, floats) = reverse floats

usedInDropBox :: DIdSet -> DropBox -> Bool
usedInDropBox bndrs (db_fvs, _) = db_fvs `intersectsDVarSet` bndrs

initDropBox :: DVarSet -> DropBox
initDropBox fvs = (fvs, [])

sepBindsByDropPoint platform is_case floaters here_fvs fork_fvs
  | null floaters  -- Shortcut common case
  = ([], [[] | _ <- fork_fvs])

  | otherwise
  = go floaters (initDropBox here_fvs) (map initDropBox fork_fvs)
  where
    n_alts = length fork_fvs

    go :: RevFloatInBinds -> DropBox -> [DropBox]
       -> (RevFloatInBinds, [RevFloatInBinds])
        -- The *first* one in the pair is the drop_here set

    go [] here_box fork_boxes
        = (dropBoxFloats here_box, map dropBoxFloats fork_boxes)

    go (bind_w_fvs@(FB bndrs bind_fvs bind) : binds) here_box fork_boxes
        | push_it_in = go binds here_box          new_fork_boxes
        | otherwise  = go binds (insert here_box) fork_boxes
        where
          push_it_in = not used_here && can_push && (n_used_alts == 1 || some_benefit)
          -- "here" means the group of bindings dropped at the top of the fork
          -- Otherwise always float in if there is just one arm; or if there is
          -- some benefit to doing so

          -- can_push: see Note [Floating primops]
          can_push | is_case   = True
                   | otherwise = not (floatIsCase bind)

          -- some_benefit is used only if (n_used_alts > 1) and (not used_here)
          -- So some duplication is going to occur
          some_benefit = small_enough &&
                         no_work_duplication &&
                         (saves_alloc || not not_thunky)

          saves_alloc = n_used_alts < n_alts
          small_enough = floatIsDupable platform bind
          no_work_duplication = is_case || case bind of
                                  FloatCase {}          -> True   -- Always a primop
                                  FloatLet (NonRec b _) -> isOneOcc (idOccInfo b)
                                  FloatLet (Rec {})     -> False  -- One will be a loop breaker

          used_here     = bndrs `usedInDropBox` here_box
          used_in_flags = case fork_boxes of
                            []  -> []
                            [_] -> [True]  -- Push all bindings into a single branch
                                           -- No need to look at its free vars
                            _   -> map (bndrs `usedInDropBox`) fork_boxes
               -- Short-cut for the singleton case;
               -- used for lambdas and singleton cases

          n_used_alts = count id used_in_flags -- returns number of Trues in list.

          not_thunky = case bind of
                         FloatCase{}           -> True
                         FloatLet (NonRec b r) -> isStrUsedDmd (idDemandInfo b)
                                                  || exprIsHNF r
                         FloatLet (Rec prs)    -> all (exprIsHNF . snd) prs

{-
          cant_push
            | is_case
            = -- The alternatives of a case expresison
              dont_float_into_alts

            | otherwise
            = -- Not the alternatives of a case expression
              -- floatIsCase: see Note [Floating primops]
              floatIsCase bind || n_used_alts > 1

          -- See Note [Duplicating floats into case alternatives]
          dont_float_into_alts
            = (n_used_alts == n_alts && not_thunky) ||
                 -- Don't float in if used in all alternatives and not a thunk
              (n_used_alts > 1 && not (floatIsDupable platform bind))
                 -- Nor if used in multiple alts and not small
-}
          new_fork_boxes = zipWithEqual "FloatIn.sepBinds" insert_maybe
                                        fork_boxes used_in_flags

          insert :: DropBox -> DropBox
          insert (fvs,drops) = (fvs `unionDVarSet` bind_fvs, bind_w_fvs:drops)

          insert_maybe box True  = insert box
          insert_maybe box False = box


{- Note [Duplicating floats into case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For case expressions (is_case = True) it is safe to float the bining into all
RHSs, without duplicating work.  But it might duplicate code.  So we refrain if
* It is used in all alternatives
* It is used in multiple alternatives, and is not small (floatIsDupable)

This is good for situations like
     let x = I# y in
     case e of
       C -> error x
       D -> error x
       E -> ...not mentioning x...
-}

floatedBindsFVs :: RevFloatInBinds -> FreeVarSet
floatedBindsFVs binds = mapUnionDVarSet fbFVs binds

fbFVs :: FloatInBind -> DVarSet
fbFVs (FB _ fvs _) = fvs

wrapFloats :: RevFloatInBinds -> CoreExpr -> CoreExpr
-- Remember RevFloatInBinds is in *reverse* dependency order
wrapFloats []               e = e
wrapFloats (FB _ _ fl : bs) e = wrapFloats bs (wrapFloat fl e)

floatIsDupable :: Platform -> FloatBind -> Bool
floatIsDupable _ (FloatCase scrut _ _ _) = small_enough_e scrut
floatIsDupable _ (FloatLet bind)         = bindIsDupable bind

bindIsDupable :: CoreBind -> Bool
bindIsDupable bind
  | isJoinBind bind        = False  -- No point in duplicating join points
bindIsDupable (Rec prs)    = all small_enough_b prs
bindIsDupable (NonRec b r) = small_enough_b (b,r)

small_enough_b :: (Id,CoreExpr) -> Bool
small_enough_b (_,rhs) = small_enough_e rhs

small_enough_e :: CoreExpr -> Bool
small_enough_e e
  = case sizeExpr opts (unfoldingUseThreshold opts) [] e of
      TooBig    -> False
      SizeIs {} -> True
  where
    opts = defaultUnfoldingOpts

{-
floatIsDupable platform (FloatCase scrut _ _ _) = exprIsDupable platform scrut
floatIsDupable platform (FloatLet (Rec prs))    = all (exprIsDupable platform . snd) prs
floatIsDupable platform (FloatLet (NonRec _ r)) = exprIsDupable platform r
-}

floatIsCase :: FloatBind -> Bool
floatIsCase (FloatCase {}) = True
floatIsCase (FloatLet {})  = False
