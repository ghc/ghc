{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[WwLib]{A library for the ``worker\/wrapper'' back-end to the strictness analyser}
-}

{-# LANGUAGE CPP #-}

module WwLib ( mkWwBodies, mkWWstr, mkWorkerArgs
             , deepSplitProductType_maybe, findTypeShape
             , isWorkerSmallEnough
 ) where

#include "HsVersions.h"

import GhcPrelude

import CoreSyn
import CoreUtils        ( exprType, mkCast )
import Id
import IdInfo           ( JoinArity, vanillaIdInfo )
import DataCon
import Demand
import MkCore           ( mkAbsentErrorApp, mkCoreUbxTup
                        , mkCoreApp, mkCoreLet )
import MkId             ( voidArgId, voidPrimId )
import TysWiredIn       ( tupleDataCon )
import TysPrim          ( voidPrimTy )
import Literal          ( absentLiteralOf )
import VarEnv           ( mkInScopeSet )
import VarSet           ( VarSet )
import Type
import RepType          ( isVoidTy )
import Coercion
import FamInstEnv
import BasicTypes       ( Boxity(..) )
import TyCon
import UniqSupply
import Unique
import Maybes
import Util
import Outputable
import DynFlags
import FastString
import ListSetOps

{-
************************************************************************
*                                                                      *
\subsection[mkWrapperAndWorker]{@mkWrapperAndWorker@}
*                                                                      *
************************************************************************

Here's an example.  The original function is:

\begin{verbatim}
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          0 -> head ys
          _ -> head (tail ys)
\end{verbatim}

From this, we want to produce:
\begin{verbatim}
-- wrapper (an unfolding)
g :: forall a . Int -> [a] -> a

g = \/\ a -> \ x ys ->
        case x of
          I# x# -> $wg a x# ys
            -- call the worker; don't forget the type args!

-- worker
$wg :: forall a . Int# -> [a] -> a

$wg = \/\ a -> \ x# ys ->
        let
            x = I# x#
        in
            case x of               -- note: body of g moved intact
              0 -> head ys
              _ -> head (tail ys)
\end{verbatim}

Something we have to be careful about:  Here's an example:

\begin{verbatim}
-- "f" strictness: U(P)U(P)
f (I# a) (I# b) = a +# b

g = f   -- "g" strictness same as "f"
\end{verbatim}

\tr{f} will get a worker all nice and friendly-like; that's good.
{\em But we don't want a worker for \tr{g}}, even though it has the
same strictness as \tr{f}.  Doing so could break laziness, at best.

Consequently, we insist that the number of strictness-info items is
exactly the same as the number of lambda-bound arguments.  (This is
probably slightly paranoid, but OK in practice.)  If it isn't the
same, we ``revise'' the strictness info, so that we won't propagate
the unusable strictness-info into the interfaces.


************************************************************************
*                                                                      *
\subsection{The worker wrapper core}
*                                                                      *
************************************************************************

@mkWwBodies@ is called when doing the worker\/wrapper split inside a module.
-}

type WwResult
  = ([Demand],              -- Demands for worker (value) args
     JoinArity,             -- Number of worker (type OR value) args
     Id -> CoreExpr,        -- Wrapper body, lacking only the worker Id
     CoreExpr -> CoreExpr)  -- Worker body, lacking the original function rhs

mkWwBodies :: DynFlags
           -> FamInstEnvs
           -> VarSet         -- Free vars of RHS
                             -- See Note [Freshen WW arguments]
           -> Maybe JoinArity -- Just ar <=> is join point with join arity ar
           -> Type           -- Type of original function
           -> [Demand]       -- Strictness of original function
           -> DmdResult      -- Info about function result
           -> UniqSM (Maybe WwResult)

-- wrap_fn_args E       = \x y -> E
-- work_fn_args E       = E x y

-- wrap_fn_str E        = case x of { (a,b) ->
--                        case a of { (a1,a2) ->
--                        E a1 a2 b y }}
-- work_fn_str E        = \a2 a2 b y ->
--                        let a = (a1,a2) in
--                        let x = (a,b) in
--                        E

mkWwBodies dflags fam_envs rhs_fvs mb_join_arity fun_ty demands res_info
  = do  { let empty_subst = mkEmptyTCvSubst (mkInScopeSet rhs_fvs)
                -- See Note [Freshen WW arguments]

        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty) <- mkWWargs empty_subst fun_ty demands
        ; (useful1, work_args, wrap_fn_str, work_fn_str) <- mkWWstr dflags fam_envs wrap_args

        -- Do CPR w/w.  See Note [Always do CPR w/w]
        ; (useful2, wrap_fn_cpr, work_fn_cpr, cpr_res_ty)
              <- mkWWcpr (gopt Opt_CprAnal dflags) fam_envs res_ty res_info

        ; let (work_lam_args, work_call_args) = mkWorkerArgs dflags work_args cpr_res_ty
              worker_args_dmds = [idDemandInfo v | v <- work_call_args, isId v]
              wrapper_body = wrap_fn_args . wrap_fn_cpr . wrap_fn_str . applyToVars work_call_args . Var
              worker_body = mkLams work_lam_args. work_fn_str . work_fn_cpr . work_fn_args

        ; if isWorkerSmallEnough dflags work_args
             && not (too_many_args_for_join_point wrap_args)
             && (useful1 && not only_one_void_argument || useful2)
          then return (Just (worker_args_dmds, length work_call_args,
                       wrapper_body, worker_body))
          else return Nothing
        }
        -- We use an INLINE unconditionally, even if the wrapper turns out to be
        -- something trivial like
        --      fw = ...
        --      f = __inline__ (coerce T fw)
        -- The point is to propagate the coerce to f's call sites, so even though
        -- f's RHS is now trivial (size 1) we still want the __inline__ to prevent
        -- fw from being inlined into f's RHS
  where
    -- Note [Do not split void functions]
    only_one_void_argument
      | [d] <- demands
      , Just (arg_ty1, _) <- splitFunTy_maybe fun_ty
      , isAbsDmd d && isVoidTy arg_ty1
      = True
      | otherwise
      = False

    -- Note [Join points returning functions]
    too_many_args_for_join_point wrap_args
      | Just join_arity <- mb_join_arity
      , wrap_args `lengthExceeds` join_arity
      = WARN(True, text "Unable to worker/wrapper join point with arity " <+>
                     int join_arity <+> text "but" <+>
                     int (length wrap_args) <+> text "args")
        True
      | otherwise
      = False

-- See Note [Limit w/w arity]
isWorkerSmallEnough :: DynFlags -> [Var] -> Bool
isWorkerSmallEnough dflags vars = count isId vars <= maxWorkerArgs dflags
    -- We count only Free variables (isId) to skip Type, Kind
    -- variables which have no runtime representation.

{-
Note [Always do CPR w/w]
~~~~~~~~~~~~~~~~~~~~~~~~
At one time we refrained from doing CPR w/w for thunks, on the grounds that
we might duplicate work.  But that is already handled by the demand analyser,
which doesn't give the CPR proprety if w/w might waste work: see
Note [CPR for thunks] in DmdAnal.

And if something *has* been given the CPR property and we don't w/w, it's
a disaster, because then the enclosing function might say it has the CPR
property, but now doesn't and there a cascade of disaster.  A good example
is Trac #5920.

Note [Limit w/w arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Guard against high worker arity as it generates a lot of stack traffic.
A simplified example is Trac #11565#comment:6

Current strategy is very simple: don't perform w/w transformation at all
if the result produces a wrapper with arity higher than -fmax-worker-args=.

It is a bit all or nothing, consider

        f (x,y) (a,b,c,d,e ... , z) = rhs

Currently we will remove all w/w ness entirely. But actually we could
w/w on the (x,y) pair... it's the huge product that is the problem.

Could we instead refrain from w/w on an arg-by-arg basis? Yes, that'd
solve f. But we can get a lot of args from deeply-nested products:

        g (a, (b, (c, (d, ...)))) = rhs

This is harder to spot on an arg-by-arg basis. Previously mkWwStr was
given some "fuel" saying how many arguments it could add; when we ran
out of fuel it would stop w/wing.
Still not very clever because it had a left-right bias.

************************************************************************
*                                                                      *
\subsection{Making wrapper args}
*                                                                      *
************************************************************************

During worker-wrapper stuff we may end up with an unlifted thing
which we want to let-bind without losing laziness.  So we
add a void argument.  E.g.

        f = /\a -> \x y z -> E::Int#    -- E does not mention x,y,z
==>
        fw = /\ a -> \void -> E
        f  = /\ a -> \x y z -> fw realworld

We use the state-token type which generates no code.
-}

mkWorkerArgs :: DynFlags -> [Var]
             -> Type    -- Type of body
             -> ([Var], -- Lambda bound args
                 [Var]) -- Args at call site
mkWorkerArgs dflags args res_ty
    | any isId args || not needsAValueLambda
    = (args, args)
    | otherwise
    = (args ++ [voidArgId], args ++ [voidPrimId])
    where
      needsAValueLambda =
        isUnliftedType res_ty
        || not (gopt Opt_FunToThunk dflags)
           -- see Note [Protecting the last value argument]

{-
Note [Protecting the last value argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the user writes (\_ -> E), they might be intentionally disallowing
the sharing of E. Since absence analysis and worker-wrapper are keen
to remove such unused arguments, we add in a void argument to prevent
the function from becoming a thunk.

The user can avoid adding the void argument with the -ffun-to-thunk
flag. However, this can create sharing, which may be bad in two ways. 1) It can
create a space leak. 2) It can prevent inlining *under a lambda*. If w/w
removes the last argument from a function f, then f now looks like a thunk, and
so f can't be inlined *under a lambda*.

Note [Join points and beta-redexes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Originally, the worker would invoke the original function by calling it with
arguments, thus producing a beta-redex for the simplifier to munch away:

  \x y z -> e => (\x y z -> e) wx wy wz

Now that we have special rules about join points, however, this is Not Good if
the original function is itself a join point, as then it may contain invocations
of other join points:

  join j1 x = ...
  join j2 y = if y == 0 then 0 else j1 y

  =>

  join j1 x = ...
  join $wj2 y# = let wy = I# y# in (\y -> if y == 0 then 0 else jump j1 y) wy
  join j2 y = case y of I# y# -> jump $wj2 y#

There can't be an intervening lambda between a join point's declaration and its
occurrences, so $wj2 here is wrong. But of course, this is easy enough to fix:

  ...
  let join $wj2 y# = let wy = I# y# in let y = wy in if y == 0 then 0 else j1 y
  ...

Hence we simply do the beta-reduction here. (This would be harder if we had to
worry about hygiene, but luckily wy is freshly generated.)

Note [Join points returning functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is crucial that the arity of a join point depends on its *callers,* not its
own syntax. What this means is that a join point can have "extra lambdas":

f :: Int -> Int -> (Int, Int) -> Int
f x y = join j (z, w) = \(u, v) -> ...
        in jump j (x, y)

Typically this happens with functions that are seen as computing functions,
rather than being curried. (The real-life example was GraphOps.addConflicts.)

When we create the wrapper, it *must* be in "eta-contracted" form so that the
jump has the right number of arguments:

f x y = join $wj z' w' = \u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
             j (z, w)  = jump $wj z w

(See Note [Join points and beta-redexes] for where the lets come from.) If j
were a function, we would instead say

f x y = let $wj = \z' w' u' v' -> let {z = z'; w = w'; u = u'; v = v'} in ...
            j (z, w) (u, v) = $wj z w u v

Notice that the worker ends up with the same lambdas; it's only the wrapper we
have to be concerned about.

FIXME Currently the functionality to produce "eta-contracted" wrappers is
unimplemented; we simply give up.

************************************************************************
*                                                                      *
\subsection{Coercion stuff}
*                                                                      *
************************************************************************

We really want to "look through" coerces.
Reason: I've seen this situation:

        let f = coerce T (\s -> E)
        in \x -> case x of
                    p -> coerce T' f
                    q -> \s -> E2
                    r -> coerce T' f

If only we w/w'd f, we'd get
        let f = coerce T (\s -> fw s)
            fw = \s -> E
        in ...

Now we'll inline f to get

        let fw = \s -> E
        in \x -> case x of
                    p -> fw
                    q -> \s -> E2
                    r -> fw

Now we'll see that fw has arity 1, and will arity expand
the \x to get what we want.
-}

-- mkWWargs just does eta expansion
-- is driven off the function type and arity.
-- It chomps bites off foralls, arrows, newtypes
-- and keeps repeating that until it's satisfied the supplied arity

mkWWargs :: TCvSubst            -- Freshening substitution to apply to the type
                                --   See Note [Freshen WW arguments]
         -> Type                -- The type of the function
         -> [Demand]     -- Demands and one-shot info for value arguments
         -> UniqSM  ([Var],            -- Wrapper args
                     CoreExpr -> CoreExpr,      -- Wrapper fn
                     CoreExpr -> CoreExpr,      -- Worker fn
                     Type)                      -- Type of wrapper body

mkWWargs subst fun_ty demands
  | null demands
  = return ([], id, id, substTy subst fun_ty)

  | (dmd:demands') <- demands
  , Just (arg_ty, fun_ty') <- splitFunTy_maybe fun_ty
  = do  { uniq <- getUniqueM
        ; let arg_ty' = substTy subst arg_ty
              id = mk_wrap_arg uniq arg_ty' dmd
        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty)
              <- mkWWargs subst fun_ty' demands'
        ; return (id : wrap_args,
                  Lam id . wrap_fn_args,
                  apply_or_bind_then work_fn_args (varToCoreExpr id),
                  res_ty) }

  | Just (tv, fun_ty') <- splitForAllTy_maybe fun_ty
  = do  { uniq <- getUniqueM
        ; let (subst', tv') = cloneTyVarBndr subst tv uniq
                -- See Note [Freshen WW arguments]
        ; (wrap_args, wrap_fn_args, work_fn_args, res_ty)
             <- mkWWargs subst' fun_ty' demands
        ; return (tv' : wrap_args,
                  Lam tv' . wrap_fn_args,
                  apply_or_bind_then work_fn_args (mkTyArg (mkTyVarTy tv')),
                  res_ty) }

  | Just (co, rep_ty) <- topNormaliseNewType_maybe fun_ty
        -- The newtype case is for when the function has
        -- a newtype after the arrow (rare)
        --
        -- It's also important when we have a function returning (say) a pair
        -- wrapped in a  newtype, at least if CPR analysis can look
        -- through such newtypes, which it probably can since they are
        -- simply coerces.

  = do { (wrap_args, wrap_fn_args, work_fn_args, res_ty)
            <-  mkWWargs subst rep_ty demands
       ; let co' = substCo subst co
       ; return (wrap_args,
                  \e -> Cast (wrap_fn_args e) (mkSymCo co'),
                  \e -> work_fn_args (Cast e co'),
                  res_ty) }

  | otherwise
  = WARN( True, ppr fun_ty )                    -- Should not happen: if there is a demand
    return ([], id, id, substTy subst fun_ty)   -- then there should be a function arrow
  where
    -- See Note [Join points and beta-redexes]
    apply_or_bind_then k arg (Lam bndr body)
      = mkCoreLet (NonRec bndr arg) (k body)    -- Important that arg is fresh!
    apply_or_bind_then k arg fun
      = k $ mkCoreApp (text "mkWWargs") fun arg
applyToVars :: [Var] -> CoreExpr -> CoreExpr
applyToVars vars fn = mkVarApps fn vars

mk_wrap_arg :: Unique -> Type -> Demand -> Id
mk_wrap_arg uniq ty dmd
  = mkSysLocalOrCoVar (fsLit "w") uniq ty
       `setIdDemandInfo` dmd

{- Note [Freshen WW arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Wen we do a worker/wrapper split, we must not in-scope names as the arguments
of the worker, else we'll get name capture.  E.g.

   -- y1 is in scope from further out
   f x = ..y1..

If we accidentally choose y1 as a worker argument disaster results:

   fww y1 y2 = let x = (y1,y2) in ...y1...

To avoid this:

  * We use a fresh unique for both type-variable and term-variable binders
    Originally we lacked this freshness for type variables, and that led
    to the very obscure Trac #12562.  (A type variable in the worker shadowed
    an outer term-variable binding.)

  * Because of this cloning we have to substitute in the type/kind of the
    new binders.  That's why we carry the TCvSubst through mkWWargs.

    So we need a decent in-scope set, just in case that type/kind
    itself has foralls.  We get this from the free vars of the RHS of the
    function since those are the only variables that might be captured.
    It's a lazy thunk, which will only be poked if the type/kind has a forall.

    Another tricky case was when f :: forall a. a -> forall a. a->a
    (i.e. with shadowing), and then the worker used the same 'a' twice.

************************************************************************
*                                                                      *
\subsection{Strictness stuff}
*                                                                      *
************************************************************************
-}

mkWWstr :: DynFlags
        -> FamInstEnvs
        -> [Var]                                -- Wrapper args; have their demand info on them
                                                --  *Includes type variables*
        -> UniqSM (Bool,                        -- Is this useful
                   [Var],                       -- Worker args
                   CoreExpr -> CoreExpr,        -- Wrapper body, lacking the worker call
                                                -- and without its lambdas
                                                -- This fn adds the unboxing

                   CoreExpr -> CoreExpr)        -- Worker body, lacking the original body of the function,
                                                -- and lacking its lambdas.
                                                -- This fn does the reboxing
mkWWstr _ _ []
  = return (False, [], nop_fn, nop_fn)

mkWWstr dflags fam_envs (arg : args) = do
    (useful1, args1, wrap_fn1, work_fn1) <- mkWWstr_one dflags fam_envs arg
    (useful2, args2, wrap_fn2, work_fn2) <- mkWWstr dflags fam_envs args
    return (useful1 || useful2, args1 ++ args2, wrap_fn1 . wrap_fn2, work_fn1 . work_fn2)

{-
Note [Unpacking arguments with product and polymorphic demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The argument is unpacked in a case if it has a product type and has a
strict *and* used demand put on it. I.e., arguments, with demands such
as the following ones:

   <S,U(U, L)>
   <S(L,S),U>

will be unpacked, but

   <S,U> or <B,U>

will not, because the pieces aren't used. This is quite important otherwise
we end up unpacking massive tuples passed to the bottoming function. Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

Does 'main' print "error 1" or "error no"?  We don't really want 'f'
to unbox its second argument.  This actually happened in GHC's onwn
source code, in Packages.applyPackageFlag, which ended up un-boxing
the enormous DynFlags tuple, and being strict in the
as-yet-un-filled-in pkgState files.
-}

----------------------
-- mkWWstr_one wrap_arg = (useful, work_args, wrap_fn, work_fn)
--   *  wrap_fn assumes wrap_arg is in scope,
--        brings into scope work_args (via cases)
--   * work_fn assumes work_args are in scope, a
--        brings into scope wrap_arg (via lets)
mkWWstr_one :: DynFlags -> FamInstEnvs -> Var
    -> UniqSM (Bool, [Var], CoreExpr -> CoreExpr, CoreExpr -> CoreExpr)
mkWWstr_one dflags fam_envs arg
  | isTyVar arg
  = return (False, [arg],  nop_fn, nop_fn)

  -- See Note [Worker-wrapper for bottoming functions]
  | isAbsDmd dmd
  , Just work_fn <- mk_absent_let dflags arg
     -- Absent case.  We can't always handle absence for arbitrary
     -- unlifted types, so we need to choose just the cases we can
     --- (that's what mk_absent_let does)
  = return (True, [], nop_fn, work_fn)

  -- See Note [Worthy functions for Worker-Wrapper split]
  | isSeqDmd dmd  -- `seq` demand; evaluate in wrapper in the hope
                  -- of dropping seqs in the worker
  = let arg_w_unf = arg `setIdUnfolding` evaldUnfolding
          -- Tell the worker arg that it's sure to be evaluated
          -- so that internal seqs can be dropped
    in return (True, [arg_w_unf], mk_seq_case arg, nop_fn)
                -- Pass the arg, anyway, even if it is in theory discarded
                -- Consider
                --      f x y = x `seq` y
                -- x gets a (Eval (Poly Abs)) demand, but if we fail to pass it to the worker
                -- we ABSOLUTELY MUST record that x is evaluated in the wrapper.
                -- Something like:
                --      f x y = x `seq` fw y
                --      fw y = let x{Evald} = error "oops" in (x `seq` y)
                -- If we don't pin on the "Evald" flag, the seq doesn't disappear, and
                -- we end up evaluating the absent thunk.
                -- But the Evald flag is pretty weird, and I worry that it might disappear
                -- during simplification, so for now I've just nuked this whole case

  | isStrictDmd dmd
  , Just cs <- splitProdDmd_maybe dmd
      -- See Note [Unpacking arguments with product and polymorphic demands]
  , Just (data_con, inst_tys, inst_con_arg_tys, co)
             <- deepSplitProductType_maybe fam_envs (idType arg)
  , cs `equalLength` inst_con_arg_tys
      -- See Note [mkWWstr and unsafeCoerce]
  = do { (uniq1:uniqs) <- getUniquesM
        ; let   unpk_args = zipWith3 mk_ww_arg uniqs inst_con_arg_tys cs
                unbox_fn  = mkUnpackCase (Var arg) co uniq1
                                         data_con unpk_args
                arg_no_unf = zapStableUnfolding arg
                             -- See Note [Zap unfolding when beta-reducing]
                             -- in Simplify.hs; and see Trac #13890
                rebox_fn   = Let (NonRec arg_no_unf con_app)
                con_app    = mkConApp2 data_con inst_tys unpk_args `mkCast` mkSymCo co
         ; (_, worker_args, wrap_fn, work_fn) <- mkWWstr dflags fam_envs unpk_args
         ; return (True, worker_args, unbox_fn . wrap_fn, work_fn . rebox_fn) }
                           -- Don't pass the arg, rebox instead

  | otherwise   -- Other cases
  = return (False, [arg], nop_fn, nop_fn)

  where
    dmd = idDemandInfo arg
    mk_ww_arg uniq ty sub_dmd = setIdDemandInfo (mk_ww_local uniq ty) sub_dmd

----------------------
nop_fn :: CoreExpr -> CoreExpr
nop_fn body = body

{-
Note [mkWWstr and unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By using unsafeCoerce, it is possible to make the number of demands fail to
match the number of constructor arguments; this happened in Trac #8037.
If so, the worker/wrapper split doesn't work right and we get a Core Lint
bug.  The fix here is simply to decline to do w/w if that happens.

Note [Record evaluated-ness in worker/wrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   data T = MkT !Int Int

   f :: T -> T
   f x = e

and f's is strict, and has the CPR property.  The we are going to generate
this w/w split

   f x = case x of
           MkT x1 x2 -> case $wf x1 x2 of
                           (# r1, r2 #) -> MkT r1 r2

   $wfw x1 x2 = let x = MkT x1 x2 in
                case e of
                  MkT r1 r2 -> (# r1, r2 #)

Note that

* In the worker $wf, inside 'e' we can be sure that x1 will be
  evaluated (it came from unpacking the argument MkT.  But that's no
  immediately apparent in $wf

* In the wrapper 'f', which we'll inline at call sites, we can be sure
  that 'r1' has been evaluated (because it came from unpacking the result
  MkT.  But that is not immediately apparent from the wrapper code.

Missing these facts isn't unsound, but it loses possible future
opportunities for optimisation.

Solution: use setCaseBndrEvald when creating
 (A) The arg binders x1,x2 in mkWstr_one
         See Trac #13077, test T13077
 (B) The result binders r1,r2 in mkWWcpr_help
         See Trace #13077, test T13077a
         And Trac #13027 comment:20, item (4)
to record that the relevant binder is evaluated.


************************************************************************
*                                                                      *
         Type scrutiny that is specific to demand analysis
*                                                                      *
************************************************************************

Note [Do not unpack class dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   f :: Ord a => [a] -> Int -> a
   {-# INLINABLE f #-}
and we worker/wrapper f, we'll get a worker with an INLINABLE pragma
(see Note [Worker-wrapper for INLINABLE functions] in WorkWrap), which
can still be specialised by the type-class specialiser, something like
   fw :: Ord a => [a] -> Int# -> a

BUT if f is strict in the Ord dictionary, we might unpack it, to get
   fw :: (a->a->Bool) -> [a] -> Int# -> a
and the type-class specialiser can't specialise that.  An example is
Trac #6056.

Moreover, dictionaries can have a lot of fields, so unpacking them can
increase closure sizes.

Conclusion: don't unpack dictionaries.
-}

deepSplitProductType_maybe
    :: FamInstEnvs -> Type
    -> Maybe (DataCon, [Type], [(Type, StrictnessMark)], Coercion)
-- If    deepSplitProductType_maybe ty = Just (dc, tys, arg_tys, co)
-- then  dc @ tys (args::arg_tys) :: rep_ty
--       co :: ty ~ rep_ty
-- Why do we return the strictness of the data-con arguments?
-- Answer: see Note [Record evaluated-ness in worker/wrapper]
deepSplitProductType_maybe fam_envs ty
  | let (co, ty1) = topNormaliseType_maybe fam_envs ty
                    `orElse` (mkRepReflCo ty, ty)
  , Just (tc, tc_args) <- splitTyConApp_maybe ty1
  , Just con <- isDataProductTyCon_maybe tc
  , not (isClassTyCon tc)  -- See Note [Do not unpack class dictionaries]
  , let arg_tys = dataConInstArgTys con tc_args
        strict_marks = dataConRepStrictness con
  = Just (con, tc_args, zipEqual "dspt" arg_tys strict_marks, co)
deepSplitProductType_maybe _ _ = Nothing

deepSplitCprType_maybe
    :: FamInstEnvs -> ConTag -> Type
    -> Maybe (DataCon, [Type], [(Type, StrictnessMark)], Coercion)
-- If    deepSplitCprType_maybe n ty = Just (dc, tys, arg_tys, co)
-- then  dc @ tys (args::arg_tys) :: rep_ty
--       co :: ty ~ rep_ty
-- Why do we return the strictness of the data-con arguments?
-- Answer: see Note [Record evaluated-ness in worker/wrapper]
deepSplitCprType_maybe fam_envs con_tag ty
  | let (co, ty1) = topNormaliseType_maybe fam_envs ty
                    `orElse` (mkRepReflCo ty, ty)
  , Just (tc, tc_args) <- splitTyConApp_maybe ty1
  , isDataTyCon tc
  , let cons = tyConDataCons tc
  , cons `lengthAtLeast` con_tag -- This might not be true if we import the
                                 -- type constructor via a .hs-bool file (#8743)
  , let con = cons `getNth` (con_tag - fIRST_TAG)
        arg_tys = dataConInstArgTys con tc_args
        strict_marks = dataConRepStrictness con
  = Just (con, tc_args, zipEqual "dsct" arg_tys strict_marks, co)
deepSplitCprType_maybe _ _ _ = Nothing

findTypeShape :: FamInstEnvs -> Type -> TypeShape
-- Uncover the arrow and product shape of a type
-- The data type TypeShape is defined in Demand
-- See Note [Trimming a demand to a type] in Demand
findTypeShape fam_envs ty
  | Just (tc, tc_args)  <- splitTyConApp_maybe ty
  , Just con <- isDataProductTyCon_maybe tc
  = TsProd (map (findTypeShape fam_envs) $ dataConInstArgTys con tc_args)

  | Just (_, res) <- splitFunTy_maybe ty
  = TsFun (findTypeShape fam_envs res)

  | Just (_, ty') <- splitForAllTy_maybe ty
  = findTypeShape fam_envs ty'

  | Just (_, ty') <- topNormaliseType_maybe fam_envs ty
  = findTypeShape fam_envs ty'

  | otherwise
  = TsUnk

{-
************************************************************************
*                                                                      *
\subsection{CPR stuff}
*                                                                      *
************************************************************************


@mkWWcpr@ takes the worker/wrapper pair produced from the strictness
info and adds in the CPR transformation.  The worker returns an
unboxed tuple containing non-CPR components.  The wrapper takes this
tuple and re-produces the correct structured output.

The non-CPR results appear ordered in the unboxed tuple as if by a
left-to-right traversal of the result structure.
-}

mkWWcpr :: Bool
        -> FamInstEnvs
        -> Type                              -- function body type
        -> DmdResult                         -- CPR analysis results
        -> UniqSM (Bool,                     -- Is w/w'ing useful?
                   CoreExpr -> CoreExpr,     -- New wrapper
                   CoreExpr -> CoreExpr,     -- New worker
                   Type)                     -- Type of worker's body

mkWWcpr opt_CprAnal fam_envs body_ty res
    -- CPR explicitly turned off (or in -O0)
  | not opt_CprAnal = return (False, id, id, body_ty)
    -- CPR is turned on by default for -O and O2
  | otherwise
  = case returnsCPR_maybe res of
       Nothing      -> return (False, id, id, body_ty)  -- No CPR info
       Just con_tag | Just stuff <- deepSplitCprType_maybe fam_envs con_tag body_ty
                    -> mkWWcpr_help stuff
                    |  otherwise
                       -- See Note [non-algebraic or open body type warning]
                    -> WARN( True, text "mkWWcpr: non-algebraic or open body type" <+> ppr body_ty )
                       return (False, id, id, body_ty)

mkWWcpr_help :: (DataCon, [Type], [(Type,StrictnessMark)], Coercion)
             -> UniqSM (Bool, CoreExpr -> CoreExpr, CoreExpr -> CoreExpr, Type)

mkWWcpr_help (data_con, inst_tys, arg_tys, co)
  | [arg1@(arg_ty1, _)] <- arg_tys
  , isUnliftedType arg_ty1
        -- Special case when there is a single result of unlifted type
        --
        -- Wrapper:     case (..call worker..) of x -> C x
        -- Worker:      case (   ..body..    ) of C x -> x
  = do { (work_uniq : arg_uniq : _) <- getUniquesM
       ; let arg       = mk_ww_local arg_uniq arg1
             con_app   = mkConApp2 data_con inst_tys [arg] `mkCast` mkSymCo co

       ; return ( True
                , \ wkr_call -> Case wkr_call arg (exprType con_app) [(DEFAULT, [], con_app)]
                , \ body     -> mkUnpackCase body co work_uniq data_con [arg] (varToCoreExpr arg)
                                -- varToCoreExpr important here: arg can be a coercion
                                -- Lacking this caused Trac #10658
                , arg_ty1 ) }

  | otherwise   -- The general case
        -- Wrapper: case (..call worker..) of (# a, b #) -> C a b
        -- Worker:  case (   ...body...  ) of C a b -> (# a, b #)
  = do { (work_uniq : wild_uniq : uniqs) <- getUniquesM
       ; let wrap_wild   = mk_ww_local wild_uniq (ubx_tup_ty,MarkedStrict)
             args        = zipWith mk_ww_local uniqs arg_tys
             ubx_tup_ty  = exprType ubx_tup_app
             ubx_tup_app = mkCoreUbxTup (map fst arg_tys) (map varToCoreExpr args)
             con_app     = mkConApp2 data_con inst_tys args `mkCast` mkSymCo co

       ; return (True
                , \ wkr_call -> Case wkr_call wrap_wild (exprType con_app)  [(DataAlt (tupleDataCon Unboxed (length arg_tys)), args, con_app)]
                , \ body     -> mkUnpackCase body co work_uniq data_con args ubx_tup_app
                , ubx_tup_ty ) }

mkUnpackCase ::  CoreExpr -> Coercion -> Unique -> DataCon -> [Id] -> CoreExpr -> CoreExpr
-- (mkUnpackCase e co uniq Con args body)
--      returns
-- case e |> co of bndr { Con args -> body }

mkUnpackCase (Tick tickish e) co uniq con args body   -- See Note [Profiling and unpacking]
  = Tick tickish (mkUnpackCase e co uniq con args body)
mkUnpackCase scrut co uniq boxing_con unpk_args body
  = Case casted_scrut bndr (exprType body)
         [(DataAlt boxing_con, unpk_args, body)]
  where
    casted_scrut = scrut `mkCast` co
    bndr = mk_ww_local uniq (exprType casted_scrut, MarkedStrict)

{-
Note [non-algebraic or open body type warning]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a few cases where the W/W transformation is told that something
returns a constructor, but the type at hand doesn't really match this. One
real-world example involves unsafeCoerce:
  foo = IO a
  foo = unsafeCoerce c_exit
  foreign import ccall "c_exit" c_exit :: IO ()
Here CPR will tell you that `foo` returns a () constructor for sure, but trying
to create a worker/wrapper for type `a` obviously fails.
(This was a real example until ee8e792  in libraries/base.)

It does not seem feasible to avoid all such cases already in the analyser (and
after all, the analysis is not really wrong), so we simply do nothing here in
mkWWcpr. But we still want to emit warning with -DDEBUG, to hopefully catch
other cases where something went avoidably wrong.


Note [Profiling and unpacking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the original function looked like
        f = \ x -> {-# SCC "foo" #-} E

then we want the CPR'd worker to look like
        \ x -> {-# SCC "foo" #-} (case E of I# x -> x)
and definitely not
        \ x -> case ({-# SCC "foo" #-} E) of I# x -> x)

This transform doesn't move work or allocation
from one cost centre to another.

Later [SDM]: presumably this is because we want the simplifier to
eliminate the case, and the scc would get in the way?  I'm ok with
including the case itself in the cost centre, since it is morally
part of the function (post transformation) anyway.


************************************************************************
*                                                                      *
\subsection{Utilities}
*                                                                      *
************************************************************************

Note [Absent errors]
~~~~~~~~~~~~~~~~~~~~
We make a new binding for Ids that are marked absent, thus
   let x = absentError "x :: Int"
The idea is that this binding will never be used; but if it
buggily is used we'll get a runtime error message.

Coping with absence for *unlifted* types is important; see, for
example, Trac #4306.  For these we find a suitable literal,
using Literal.absentLiteralOf.  We don't have literals for
every primitive type, so the function is partial.

Note: I did try the experiment of using an error thunk for unlifted
things too, relying on the simplifier to drop it as dead code.
But this is fragile

 - It fails when profiling is on, which disables various optimisations

 - It fails when reboxing happens. E.g.
      data T = MkT Int Int#
      f p@(MkT a _) = ...g p....
   where g is /lazy/ in 'p', but only uses the first component.  Then
   'f' is /strict/ in 'p', and only uses the first component.  So we only
   pass that component to the worker for 'f', which reconstructs 'p' to
   pass it to 'g'.  Alas we can't say
       ...f (MkT a (absentError Int# "blah"))...
   bacause `MkT` is strict in its Int# argument, so we get an absentError
   exception when we shouldn't.  Very annoying!

So absentError is only used for lifted types.
-}

mk_absent_let :: DynFlags -> Id -> Maybe (CoreExpr -> CoreExpr)
mk_absent_let dflags arg
  | not (isUnliftedType arg_ty)
  = Just (Let (NonRec lifted_arg abs_rhs))
  | Just tc <- tyConAppTyCon_maybe arg_ty
  , Just lit <- absentLiteralOf tc
  = Just (Let (NonRec arg (Lit lit)))
  | arg_ty `eqType` voidPrimTy
  = Just (Let (NonRec arg (Var voidPrimId)))
  | otherwise
  = WARN( True, text "No absent value for" <+> ppr arg_ty )
    Nothing
  where
    lifted_arg = arg `setIdStrictness` exnSig
              -- Note in strictness signature that this is bottoming
              -- (for the sake of the "empty case scrutinee not known to
              -- diverge for sure lint" warning)
    arg_ty     = idType arg
    abs_rhs    = mkAbsentErrorApp arg_ty msg
    msg        = showSDoc (gopt_set dflags Opt_SuppressUniques)
                          (ppr arg <+> ppr (idType arg))
              -- We need to suppress uniques here because otherwise they'd
              -- end up in the generated code as strings. This is bad for
              -- determinism, because with different uniques the strings
              -- will have different lengths and hence different costs for
              -- the inliner leading to different inlining.
              -- See also Note [Unique Determinism] in Unique

mk_seq_case :: Id -> CoreExpr -> CoreExpr
mk_seq_case arg body = Case (Var arg) (sanitiseCaseBndr arg) (exprType body) [(DEFAULT, [], body)]

sanitiseCaseBndr :: Id -> Id
-- The argument we are scrutinising has the right type to be
-- a case binder, so it's convenient to re-use it for that purpose.
-- But we *must* throw away all its IdInfo.  In particular, the argument
-- will have demand info on it, and that demand info may be incorrect for
-- the case binder.  e.g.       case ww_arg of ww_arg { I# x -> ... }
-- Quite likely ww_arg isn't used in '...'.  The case may get discarded
-- if the case binder says "I'm demanded".  This happened in a situation
-- like         (x+y) `seq` ....
sanitiseCaseBndr id = id `setIdInfo` vanillaIdInfo

mk_ww_local :: Unique -> (Type, StrictnessMark) -> Id
-- The StrictnessMark comes form the data constructor and says
-- whether this field is strict
-- See Note [Record evaluated-ness in worker/wrapper]
mk_ww_local uniq (ty,str)
  = setCaseBndrEvald str $
    mkSysLocalOrCoVar (fsLit "ww") uniq ty
