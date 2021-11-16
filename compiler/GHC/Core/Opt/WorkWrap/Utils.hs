{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

A library for the ``worker\/wrapper'' back-end to the strictness analyser
-}


{-# LANGUAGE ViewPatterns #-}

module GHC.Core.Opt.WorkWrap.Utils
   ( WwOpts(..), initWwOpts, mkWwBodies, mkWWstr, mkWWstr_one, mkWorkerArgs
   , DataConPatContext(..)
   , UnboxingDecision(..), InsideInlineableFun(..), wantToUnboxArg
   , findTypeShape, IsRecDataConResult(..), isRecDataCon, finaliseBoxity
   , mkAbsentFiller
   , isWorkerSmallEnough
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Config (initSimpleOpts)

import GHC.Core
import GHC.Core.Utils
import GHC.Core.DataCon
import GHC.Core.Make
import GHC.Core.Subst
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Predicate ( isClassPred )
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.FamInstEnv
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Core.SimpleOpt( SimpleOpts )

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Id.Make ( voidArgId, voidPrimId )
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.Unique.Supply
import GHC.Types.Name ( getOccFS )

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Data.List.SetOps

import GHC.Builtin.Types ( tupleDataCon )

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Trace

import Control.Applicative ( (<|>) )
import Control.Monad ( zipWithM )
import Data.List ( unzip4 )

import GHC.Types.RepType
import GHC.Unit.Types

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

data WwOpts
  = MkWwOpts
  { wo_fam_envs          :: !FamInstEnvs
  , wo_simple_opts       :: !SimpleOpts
  , wo_cpr_anal          :: !Bool
  , wo_fun_to_thunk      :: !Bool
  , wo_max_worker_args   :: !Int
  -- Used for absent argument error message
  , wo_module            :: !Module
  }

initWwOpts :: Module -> DynFlags -> FamInstEnvs -> WwOpts
initWwOpts this_mod dflags fam_envs = MkWwOpts
  { wo_fam_envs          = fam_envs
  , wo_simple_opts       = initSimpleOpts dflags
  , wo_cpr_anal          = gopt Opt_CprAnal dflags
  , wo_fun_to_thunk      = gopt Opt_FunToThunk dflags
  , wo_max_worker_args   = maxWorkerArgs dflags
  , wo_module            = this_mod
  }

type WwResult
  = ([Demand],              -- Demands for worker (value) args
     JoinArity,             -- Number of worker (type OR value) args
     Id -> CoreExpr,        -- Wrapper body, lacking only the worker Id
     CoreExpr -> CoreExpr)  -- Worker body, lacking the original function rhs

nop_fn :: CoreExpr -> CoreExpr
nop_fn body = body


mkWwBodies :: WwOpts
           -> Id             -- ^ The original function
           -> [Var]          -- ^ Manifest args of original function
           -> Type           -- ^ Result type of the original function,
                             --   after being stripped of args
           -> [Demand]       -- ^ Strictness of original function
           -> Cpr            -- ^ Info about function result
           -> UniqSM (Maybe WwResult)
-- ^ Given a function definition
--
-- > data T = MkT Int Bool Char
-- > f :: (a, b) -> Int -> T
-- > f = \x y -> E
--
-- @mkWwBodies _ 'f' ['x::(a,b)','y::Int'] '(a,b)' ['1P(L,L)', '1P(L)'] '1'@
-- returns
--
--   * The wrapper body context for the call to the worker function, lacking
--     only the 'Id' for the worker function:
--
--     > W[_] :: Id -> CoreExpr
--     > W[work_fn] = \x y ->          -- args of the wrapper    (cloned_arg_vars)
--     >   case x of (a, b) ->         -- unbox wrapper args     (wrap_fn_str)
--     >   case y of I# n ->           --
--     >   case <work_fn> a b n of     -- call to the worker fun (call_work)
--     >   (# i, b, c #) -> MkT i b c  -- rebox result           (wrap_fn_cpr)
--
--   * The worker body context that wraps around its hole reboxing defns for x
--     and y, as well as returning CPR transit variables of the unboxed MkT
--     result in an unboxed tuple:
--
--     > w[_] :: CoreExpr -> CoreExpr
--     > w[fn_rhs] = \a b n ->              -- args of the worker       (work_lam_args)
--     >   let { y = I# n; x = (a, b) } in  -- reboxing wrapper args    (work_fn_str)
--     >   case <fn_rhs> x y of             -- call to the original RHS (call_rhs)
--     >   MkT i b c -> (# i, b, c #)       -- return CPR transit vars  (work_fn_cpr)
--
--     NB: The wrap_rhs hole is to be filled with the original wrapper RHS
--     @\x y -> E@. This is so that we can also use @w@ to transform stable
--     unfoldings, the lambda args of which may be different than x and y.
--
--   * Id details for the worker function like demands on arguments and its join
--     arity.
--
-- All without looking at E (except for beta reduction, see Note [Join points
-- and beta-redexes]), which allows us to apply the same split to function body
-- and its unfolding(s) alike.
--
mkWwBodies opts fun_id arg_vars res_ty demands res_cpr
  = do  { massertPpr (filter isId arg_vars `equalLength` demands)
                     (text "wrong wrapper arity" $$ ppr fun_id $$ ppr arg_vars $$ ppr res_ty $$ ppr demands)

        -- Clone and prepare arg_vars of the original fun RHS
        -- See Note [Freshen WW arguments]
        -- and Note [Zap IdInfo on worker args]
        ; uniq_supply <- getUniqueSupplyM
        ; let args_free_tcvs = tyCoVarsOfTypes (res_ty : map varType arg_vars)
              empty_subst = mkEmptySubst (mkInScopeSet args_free_tcvs)
              zapped_arg_vars = map zap_var arg_vars
              (subst, cloned_arg_vars) = cloneBndrs empty_subst uniq_supply zapped_arg_vars
              res_ty' = GHC.Core.Subst.substTy subst res_ty

        ; (useful1, work_args, wrap_fn_str, fn_args)
             <- mkWWstr opts cloned_arg_vars

        -- Do CPR w/w.  See Note [Always do CPR w/w]
        ; (useful2, wrap_fn_cpr, work_fn_cpr, cpr_res_ty)
              <- mkWWcpr_entry opts res_ty' res_cpr

        ; let (work_lam_args, work_call_args) = mkWorkerArgs fun_id (wo_fun_to_thunk opts)
                                                             work_args cpr_res_ty
              call_work work_fn  = mkVarApps (Var work_fn) work_call_args
              call_rhs fn_rhs = mkAppsBeta fn_rhs fn_args
                                  -- See Note [Join points and beta-redexes]
              wrapper_body = mkLams cloned_arg_vars . wrap_fn_cpr . wrap_fn_str . call_work
              worker_body = mkLams work_lam_args . work_fn_cpr . call_rhs
              worker_args_dmds = [idDemandInfo v | v <- work_call_args, isId v]

        ; if isWorkerSmallEnough (wo_max_worker_args opts) (length demands) work_args
             && not (too_many_args_for_join_point arg_vars)
             && ((useful1 && not only_one_void_argument) || useful2)
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
    zap_var v | isTyVar v = v
              | otherwise = modifyIdInfo zap_info v
    zap_info info -- See Note [Zap IdInfo on worker args]
      = info `setOccInfo`       noOccInfo

    mb_join_arity = isJoinId_maybe fun_id

    -- Note [Do not split void functions]
    only_one_void_argument
      | [d] <- demands
      , [v] <- filter isId arg_vars
      , isAbsDmd d && isZeroBitTy (idType v)
      = True
      | otherwise
      = False

    -- Note [Join points returning functions]
    too_many_args_for_join_point wrap_args
      | Just join_arity <- mb_join_arity
      , wrap_args `lengthExceeds` join_arity
      = warnPprTrace True (text "Unable to worker/wrapper join point with arity " <+>
                     int join_arity <+> text "but" <+>
                     int (length wrap_args) <+> text "args") $
        True
      | otherwise
      = False

-- | Version of 'GHC.Core.mkApps' that does beta reduction on-the-fly.
-- PRECONDITION: The arg expressions are not free in any of the lambdas binders.
mkAppsBeta :: CoreExpr -> [CoreArg] -> CoreExpr
-- The precondition holds for our call site in mkWwBodies, because all the FVs
-- of as are either cloned_arg_vars (and thus fresh) or fresh worker args.
mkAppsBeta (Lam b body) (a:as) = bindNonRec b a $! mkAppsBeta body as
mkAppsBeta f            as     = mkApps f as

-- See Note [Limit w/w arity]
isWorkerSmallEnough :: Int -> Int -> [Var] -> Bool
isWorkerSmallEnough max_worker_args old_n_args vars
  = count isId vars <= max old_n_args max_worker_args
    -- We count only Free variables (isId) to skip Type, Kind
    -- variables which have no runtime representation.
    -- Also if the function took 82 arguments before (old_n_args), it's fine if
    -- it takes <= 82 arguments afterwards.

{-
Note [Always do CPR w/w]
~~~~~~~~~~~~~~~~~~~~~~~~
At one time we refrained from doing CPR w/w for thunks, on the grounds that
we might duplicate work.  But that is already handled by the demand analyser,
which doesn't give the CPR property if w/w might waste work: see
Note [CPR for thunks] in GHC.Core.Opt.DmdAnal.

And if something *has* been given the CPR property and we don't w/w, it's
a disaster, because then the enclosing function might say it has the CPR
property, but now doesn't and there a cascade of disaster.  A good example
is #5920.

Note [Limit w/w arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Guard against high worker arity as it generates a lot of stack traffic.
A simplified example is #11565#comment:6

Current strategy is very simple: don't perform w/w transformation at all
if the result produces a wrapper with arity higher than -fmax-worker-args
and the number arguments before w/w (see #18122).

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

Note [Zap IdInfo on worker args]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have to zap the following IdInfo when re-using arg variables of the original
function for the worker:

  * OccInfo: Dead wrapper args now occur in Apps of the worker's call to the
    original fun body. Those occurrences will quickly cancel away with the lambdas
    of the fun body in the next run of the Simplifier, but CoreLint will complain
    in the meantime, so zap it.

We zap in mkWwBodies because we need the zapped variables both when binding them
in mkWWstr (mkAbsentFiller, specifically) and in mkWorkerArgs, where we produce
the call to the fun body.

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

mkWorkerArgs :: Id      -- The wrapper Id
             -> Bool
             -> [Var]
             -> Type    -- Type of body
             -> ([Var], -- Lambda bound args
                 [Var]) -- Args at call site
mkWorkerArgs wrap_id fun_to_thunk args res_ty
    | not (isJoinId wrap_id) -- Join Ids never need an extra arg
    , not (any isId args)    -- No existing value lambdas
    , needs_a_value_lambda   -- and we need to add one
    = (args ++ [voidArgId], args ++ [voidPrimId])

    | otherwise
    = (args, args)
    where
      -- If fun_to_thunk is False we always keep at least one value
      --   argument: see Note [Protecting the last value argument]
      -- If it is True, we only need to keep a value argument if
      --   the result type is (or might be) unlifted, in which case
      --   dropping the last arg would mean we wrongly used call-by-value
      needs_a_value_lambda
        = not fun_to_thunk
          || might_be_unlifted

      -- Might the result be lifted?
      --     False => definitely lifted
      --     True  => might be unlifted
      -- We may encounter a representation-polymorphic result, in which case we
      -- conservatively assume that we have laziness that needs
      -- preservation. See #15186.
      might_be_unlifted = case isLiftedType_maybe res_ty of
                            Just lifted -> not lifted
                            Nothing     -> True

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
rather than being curried. (The real-life example was GHC.Data.Graph.Ops.addConflicts.)

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

Note [Freshen WW arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we do a worker/wrapper split, we must freshen the arg vars of the original
fun RHS because they might shadow each other. E.g.

  f :: forall a. Maybe a -> forall a. Maybe a -> Int -> Int
  f @a x @a y z = case x <|> y of
    Nothing -> z
    Just _  -> z + 1

  ==> {WW split unboxing the Int}

  $wf :: forall a. Maybe a -> forall a. Maybe a -> Int# -> Int
  $wf @a x @a y wz = (\@a x @a y z -> case x <|> y of ...) ??? x @a y (I# wz)

(Notice that the code we actually emit will sort-of ANF-ise the lambda args,
leading to even more shadowing issues. The above demonstrates that even if we
try harder we'll still get shadowing issues.)

What should we put in place for ??? ? Certainly not @a, because that would
reference the wrong, inner a. A similar situation occurred in #12562, we even
saw a type variable in the worker shadowing an outer term-variable binding.

We avoid the issue by freshening the argument variables from the original fun
RHS through 'cloneBndrs', which will also take care of subsitution in binder
types. Fortunately, it's sufficient to pick the FVs of the arg vars as in-scope
set, so that we don't need to do a FV traversal over the whole body of the
original function.

At the moment, #12562 has no regression test. As such, this Note is not covered
by any test logic or when bootstrapping the compiler. Yet we clearly want to
freshen the binders, as the example above demonstrates.
Adding a Core pass that maximises shadowing for testing purposes might help,
see #17478.
-}

{-
************************************************************************
*                                                                      *
\subsection{Unboxing Decision for Strictness and CPR}
*                                                                      *
************************************************************************
-}

-- | The information needed to build a pattern for a DataCon to be unboxed.
-- The pattern can be generated from 'dcpc_dc' and 'dcpc_tc_args' via
-- 'GHC.Core.Utils.dataConRepInstPat'. The coercion 'dcpc_co' is for newtype
-- wrappers.
--
-- If we get @DataConPatContext dc tys co@ for some type @ty@
-- and @dataConRepInstPat ... dc tys = (exs, flds)@, then
--
--   * @dc @exs flds :: T tys@
--   * @co :: T tys ~ ty@
data DataConPatContext
  = DataConPatContext
  { dcpc_dc      :: !DataCon
  , dcpc_tc_args :: ![Type]
  , dcpc_co      :: !Coercion
  }

-- | Describes the outer shape of an argument to be unboxed or left as-is
-- Depending on how @s@ is instantiated (e.g., 'Demand' or 'Cpr').
data UnboxingDecision s
  = StopUnboxing
  -- ^ We ran out of strictness info. Leave untouched.
  | DropAbsent
  -- ^ The argument/field was absent. Drop it.
  | Unbox !DataConPatContext [s]
  -- ^ The argument is used strictly or the returned product was constructed, so
  -- unbox it.
  -- The 'DataConPatContext' carries the bits necessary for
  -- instantiation with 'dataConRepInstPat'.
  -- The @[s]@ carries the bits of information with which we can continue
  -- unboxing, e.g. @s@ will be 'Demand' or 'Cpr'.

-- | Unwraps the 'Boxity' decision encoded in the given 'SubDemand' and returns
-- a 'DataConPatContext' as well the nested demands on fields of the 'DataCon'
-- to unbox.
wantToUnboxArg
  :: FamInstEnvs
  -> Type                -- ^ Type of the argument
  -> Demand              -- ^ How the arg was used
  -> UnboxingDecision Demand
-- See Note [Which types are unboxed?]
wantToUnboxArg fam_envs ty (n :* sd)
  | isAbs n
  = DropAbsent

  | Just (tc, tc_args, co) <- normSplitTyConApp_maybe fam_envs ty
  , Just dc <- tyConSingleAlgDataCon_maybe tc
  , let arity = dataConRepArity dc
  , Just (Unboxed, ds) <- viewProd arity sd -- See Note [Boxity Analysis]
  -- NB: No strictness or evaluatedness checks here. That is done by
  -- 'finaliseBoxity'!
  = Unbox (DataConPatContext dc tc_args co) ds

  | otherwise
  = StopUnboxing


-- | Unboxing strategy for constructed results.
wantToUnboxResult :: FamInstEnvs -> Type -> Cpr -> UnboxingDecision Cpr
-- See Note [Which types are unboxed?]
wantToUnboxResult fam_envs ty cpr
  | Just (con_tag, arg_cprs) <- asConCpr cpr
  , Just (tc, tc_args, co) <- normSplitTyConApp_maybe fam_envs ty
  , Just dcs <- tyConAlgDataCons_maybe tc <|> open_body_ty_warning
  , dcs `lengthAtLeast` con_tag -- This might not be true if we import the
                                -- type constructor via a .hs-boot file (#8743)
  , let dc = dcs `getNth` (con_tag - fIRST_TAG)
  , null (dataConExTyCoVars dc) -- no existentials;
                                -- See Note [Which types are unboxed?]
                                -- and GHC.Core.Opt.CprAnal.argCprType
                                -- where we also check this.
  , all isLinear (dataConInstArgTys dc tc_args)
  -- Deactivates CPR worker/wrapper splits on constructors with non-linear
  -- arguments, for the moment, because they require unboxed tuple with variable
  -- multiplicity fields.
  = Unbox (DataConPatContext dc tc_args co) arg_cprs

  | otherwise
  = StopUnboxing

  where
    -- | See Note [non-algebraic or open body type warning]
    open_body_ty_warning = warnPprTrace True (text "wantToUnboxResult: non-algebraic or open body type" <+> ppr ty) Nothing

isLinear :: Scaled a -> Bool
isLinear (Scaled w _ ) =
  case w of
    One -> True
    _ -> False


{- Note [Which types are unboxed?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Worker/wrapper will unbox

  1. A strict data type argument, that
       * is an algebraic data type (not a newtype)
       * has a single constructor (thus is a "product")
       * that may bind existentials
     We can transform
     > data D a = forall b. D a b
     > f (D @ex a b) = e
     to
     > $wf @ex a b = e
     via 'mkWWstr'.

  2. The constructed result of a function, if
       * its type is an algebraic data type (not a newtype)
       * (might have multiple constructors, in contrast to (1))
       * the applied data constructor *does not* bind existentials
     We can transform
     > f x y = let ... in D a b
     to
     > $wf x y = let ... in (# a, b #)
     via 'mkWWcpr'.

     NB: We don't allow existentials for CPR W/W, because we don't have unboxed
     dependent tuples (yet?). Otherwise, we could transform
     > f x y = let ... in D @ex (a :: ..ex..) (b :: ..ex..)
     to
     > $wf x y = let ... in (# @ex, (a :: ..ex..), (b :: ..ex..) #)

The respective tests are in 'wantToUnboxArg' and
'wantToUnboxResult', respectively.

Note that the data constructor /can/ have evidence arguments: equality
constraints, type classes etc.  So it can be GADT.  These evidence
arguments are simply value arguments, and should not get in the way.

Note [Do not unbox class dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   f :: Ord a => [a] -> Int -> a
   {-# INLINABLE f #-}
and we worker/wrapper f, we'll get a worker with an INLINABLE pragma
(see Note [Worker/wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap),
which can still be specialised by the type-class specialiser, something like
   fw :: Ord a => [a] -> Int# -> a

BUT if f is strict in the Ord dictionary, we might unpack it, to get
   fw :: (a->a->Bool) -> [a] -> Int# -> a
and the type-class specialiser can't specialise that. An example is #6056.

But in any other situation, a dictionary is just an ordinary value,
and can be unpacked.  So we track the INLINABLE pragma, and discard the boxity
flag in finaliseBoxity (see the isClassPred test).

Historical note: #14955 describes how I got this fix wrong the first time.

Note that the simplicity of this fix implies that INLINE functions (such as
wrapper functions after the WW run) will never say that they unbox class
dictionaries. That's not ideal, but not worth losing sleep over, as INLINE
functions will have been inlined by the time we run demand analysis so we'll
see the unboxing around the worker in client modules. I got aware of the issue
in T5075 by the change in boxity of loop between demand analysis runs.

Note [mkWWstr and unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By using unsafeCoerce, it is possible to make the number of demands fail to
match the number of constructor arguments; this happened in #8037.
If so, the worker/wrapper split doesn't work right and we get a Core Lint
bug.  The fix here is simply to decline to do w/w if that happens.

Note [Unboxing evaluated arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this program (due to Roman):

    data X a = X !a

    foo :: X Int -> Int -> Int
    foo x@(X a) n = go 0
     where
       go i | i < n     = a + go (i+1)
            | otherwise = 0

We want the worker for 'foo' too look like this:

    $wfoo :: Int# -> Int# -> Int#

with the first argument unboxed, so that it is not eval'd each time around the
'go' loop (which would otherwise happen, since 'foo' is not strict in 'a'). It
is sound for the wrapper to pass an unboxed arg because X is strict
(see Note [Strictness and Unboxing] in "GHC.Core.Opt.DmdAnal"), so its argument
must be evaluated. And if we *don't* pass an unboxed argument, we can't even
repair it by adding a `seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

So here's what we do

* Since this has nothing to do with how 'foo' uses 'a', we leave demand analysis
  alone, but account for the additional evaluatedness when annotating the binder
  in 'annotateLamIdBndr' via 'finaliseBoxity', which will retain the Unboxed boxity
  on 'a' in the definition of 'foo' in the demand 'L!P(L)'; meaning it's used
  lazily but unboxed nonetheless. This seems to contradict
  Note [No lazy, Unboxed demands in demand signature], but we know that 'a' is
  evaluated and thus can be unboxed.

* When 'finaliseBoxity' decides to unbox a record, it will zip the field demands
  together with the respective 'StrictnessMark'. In case of 'x', it will pair
  up the lazy field demand 'L!P(L)' on 'a' with 'MarkedStrict' to account for
  the strict field.

* Said 'StrictnessMark' is passed to the recursive invocation of
  'finaliseBoxity' when deciding whether to unbox 'a'. 'a' was used lazily, but
  since it also says 'MarkedStrict', we'll retain the 'Unboxed' boxity on 'a'.

* Worker/wrapper will consult 'wantToUnboxArg' for its unboxing decision. It will
  /not/ look at the strictness bits of the demand, only at Boxity flags. As such,
  it will happily unbox 'a' despite the lazy demand on it.

The net effect is that boxity analysis and the w/w transformation are more
aggressive about unboxing the strict arguments of a data constructor than when
looking at strictness info exclusively. It is very much like (Nested) CPR, which
needs its nested fields to be evaluated in order for it to unbox nestedly.

There is the usual danger of reboxing, which as usual we ignore. But
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!

This works in nested situations like T10482

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k = case f of BarPair x y ->
              case burble of
                 True -> case x of
                           BarPair p q -> ...
                 False -> ...

The extra eagerness lets us produce a worker of type:
     $wfoo :: Int# -> Int# -> Int# -> Int -> Int
     $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated.

--------- Historical note ------------
We used to add data-con strictness demands when demand analysing case
expression. However, it was noticed in #15696 that this misses some cases. For
instance, consider the program (from T10482)

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k =
      case f of
        BarPair x y -> case burble of
                          True -> case x of
                                    BarPair p q -> ...
                          False -> ...

We really should be able to assume that `p` is already evaluated since it came
from a strict field of BarPair. This strictness would allow us to produce a
worker of type:

    $wfoo :: Int# -> Int# -> Int# -> Int -> Int
    $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated

Indeed before we fixed #15696 this would happen since we would float the inner
`case x` through the `case burble` to get:

    foo f k =
      case f of
        BarPair x y -> case x of
                          BarPair p q -> case burble of
                                          True -> ...
                                          False -> ...

However, after fixing #15696 this could no longer happen (for the reasons
discussed in ticket:15696#comment:76). This means that the demand placed on `f`
would then be significantly weaker (since the False branch of the case on
`burble` is not strict in `p` or `q`).

Consequently, we now instead account for data-con strictness in mkWWstr_one,
applying the strictness demands to the final result of DmdAnal. The result is
that we get the strict demand signature we wanted even if we can't float
the case on `x` up through the case on `burble`.

Note [No nested Unboxed inside Boxed in demand signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
```
f p@(x,y)
  | even (x+y) = []
  | otherwise  = [p]
```
Demand analysis will infer that the function body puts a demand of `1P(1!L,1!L)`
on 'p', e.g., Boxed on the outside but Unboxed on the inside. But worker/wrapper
can't unbox the pair components without unboxing the pair! So we better say
`1P(1L,1L)` in the demand signature in order not to spread wrong Boxity info.
That happens in 'finaliseBoxity'.

Note [No lazy, Unboxed demands in demand signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider T19407:

  data Huge = Huge Bool () ... () -- think: DynFlags
  data T = T { h :: Huge, n :: Int }
  f t@(T h _) = g h t
  g (H b _ ... _) t = if b then 1 else n t

The body of `g` puts (approx.) demand `L!P(A,1)` on `t`. But we better
not put that demand in `g`'s demand signature, because worker/wrapper will not
in general unbox a lazy-and-unboxed demand like `L!P(..)`.
(The exception are known-to-be-evaluated arguments like strict fields,
see Note [Unboxing evaluated arguments].)

The program above is an example where spreading misinformed boxity through the
signature is particularly egregious. If we give `g` that signature, then `f`
puts demand `S!P(1!P(1L,A,..),ML)` on `t`. Now we will unbox `t` in `f` it and
we get

  f (T (H b _ ... _) n) = $wf b n
  $wf b n = $wg b (T (H b x ... x) n)
  $wg = ...

Massive reboxing in `$wf`! Solution: Trim boxity on lazy demands in
'finaliseBoxity', modulo Note [Unboxing evaluated arguments].

Note [Finalising boxity for demand signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The worker/wrapper pass must strictly adhere to the boxity decisions encoded
in the demand signature, because that is the information that demand analysis
propagates throughout the program. Failing to implement the strategy laid out
in the signature can result in reboxing in unexpected places. Hence, we must
completely anticipate unboxing decisions during demand analysis and reflect
these decicions in demand annotations. That is the job of 'finaliseBoxity',
which is defined here and called from demand analysis.

Here is a list of different Notes it has to take care of:

  * Note [No lazy, Unboxed demands in demand signature] such as `L!P(L)` in
    general, but still allow Note [Unboxing evaluated arguments]
  * Note [No nested Unboxed inside Boxed in demand signature] such as `1P(1!L)`
  * Implement fixes for corner cases Note [Do not unbox class dictionaries]
    and Note [mkWWstr and unsafeCoerce]

Then, in worker/wrapper blindly trusts the boxity info in the demand signature
and will not look at strictness info *at all*, in 'wantToUnboxArg'.

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

This warning also triggers for the stream fusion library within `text`.
We can'easily W/W constructed results like `Stream` because we have no simple
way to express existential types in the worker's type signature.
-}

{-
************************************************************************
*                                                                      *
\subsection{Worker/wrapper for Strictness and Absence}
*                                                                      *
************************************************************************
-}

mkWWstr :: WwOpts
        -> [Var]                         -- Wrapper args; have their demand info on them
                                         --  *Includes type variables*
        -> UniqSM (Bool,                 -- Is this useful
                   [Var],                -- Worker args
                   CoreExpr -> CoreExpr, -- Wrapper body, lacking the worker call
                                         -- and without its lambdas
                                         -- This fn adds the unboxing
                   [CoreExpr])           -- Reboxed args for the call to the
                                         -- original RHS. Corresponds one-to-one
                                         -- with the wrapper arg vars
mkWWstr opts args
  = go args
  where
    go_one arg = mkWWstr_one opts arg

    go []           = return (False, [], nop_fn, [])
    go (arg : args) = do { (useful1, args1, wrap_fn1, wrap_arg)  <- go_one arg
                         ; (useful2, args2, wrap_fn2, wrap_args) <- go args
                         ; return ( useful1 || useful2
                                  , args1 ++ args2
                                  , wrap_fn1 . wrap_fn2
                                  , wrap_arg:wrap_args ) }

----------------------
-- mkWWstr_one wrap_var = (useful, work_args, wrap_fn, wrap_arg)
--   *  wrap_fn assumes wrap_var is in scope,
--        brings into scope work_args (via cases)
--   * wrap_arg assumes work_args are in scope, and builds a ConApp that
--        reconstructs the RHS of wrap_var that we pass to the original RHS
-- See Note [Worker/wrapper for Strictness and Absence]
mkWWstr_one :: WwOpts -> Var -> UniqSM (Bool, [Var], CoreExpr -> CoreExpr, CoreExpr)
mkWWstr_one opts arg =
  case wantToUnboxArg fam_envs arg_ty arg_dmd of
    _ | isTyVar arg -> do_nothing

    DropAbsent
      | Just absent_filler <- mkAbsentFiller opts arg
         -- Absent case.  We can't always handle absence for arbitrary
         -- unlifted types, so we need to choose just the cases we can
         -- (that's what mkAbsentFiller does)
      -> return (True, [], nop_fn, absent_filler)

    Unbox dcpc ds -> unbox_one_arg opts arg ds dcpc

    _ -> do_nothing -- Other cases, like StopUnboxing

  where
    fam_envs   = wo_fam_envs opts
    arg_ty     = idType arg
    arg_dmd    = idDemandInfo arg
    do_nothing = return (False, [arg], nop_fn, varToCoreExpr arg)

unbox_one_arg :: WwOpts
          -> Var
          -> [Demand]
          -> DataConPatContext
          -> UniqSM (Bool, [Var], CoreExpr -> CoreExpr, CoreExpr)
unbox_one_arg opts arg_var ds
          DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args
                            , dcpc_co = co }
  = do { pat_bndrs_uniqs <- getUniquesM
       ; let ex_name_fss = map getOccFS $ dataConExTyCoVars dc
             (ex_tvs', arg_ids) =
               dataConRepFSInstPat (ex_name_fss ++ repeat ww_prefix) pat_bndrs_uniqs (idMult arg_var) dc tc_args
             arg_ids' = zipWithEqual "unbox_one_arg" setIdDemandInfo arg_ids ds
             unbox_fn = mkUnpackCase (Var arg_var) co (idMult arg_var)
                                     dc (ex_tvs' ++ arg_ids')
       ; (_, worker_args, wrap_fn, wrap_args) <- mkWWstr opts (ex_tvs' ++ arg_ids')
       ; let wrap_arg = mkConApp dc (map Type tc_args ++ wrap_args) `mkCast` mkSymCo co
       ; return (True, worker_args, unbox_fn . wrap_fn, wrap_arg) }
                          -- Don't pass the arg, rebox instead

-- | Tries to find a suitable absent filler to bind the given absent identifier
-- to. See Note [Absent fillers].
--
-- If @mkAbsentFiller _ id == Just e@, then @e@ is an absent filler with the
-- same type as @id@. Otherwise, no suitable filler could be found.
mkAbsentFiller :: WwOpts -> Id -> Maybe CoreExpr
mkAbsentFiller opts arg
  -- The lifted case: Bind 'absentError' for a nice panic message if we are
  -- wrong (like we were in #11126). See (1) in Note [Absent fillers]
  | not (isUnliftedType arg_ty)
  , not is_strict, not is_evald -- See (2) in Note [Absent fillers]
  = Just (mkAbsentErrorApp arg_ty msg)

  -- The default case for mono rep: Bind `RUBBISH[rr] arg_ty`
  -- See Note [Absent fillers], the main part
  | otherwise
  = mkLitRubbish arg_ty

  where
    arg_ty    = idType arg
    is_strict = isStrictDmd (idDemandInfo arg)
    is_evald  = isEvaldUnfolding $ idUnfolding arg

    msg = renderWithContext
            (defaultSDocContext { sdocSuppressUniques = True })
            (vcat
              [ text "Arg:" <+> ppr arg
              , text "Type:" <+> ppr arg_ty
              , file_msg ])
              -- We need to suppress uniques here because otherwise they'd
              -- end up in the generated code as strings. This is bad for
              -- determinism, because with different uniques the strings
              -- will have different lengths and hence different costs for
              -- the inliner leading to different inlining.
              -- See also Note [Unique Determinism] in GHC.Types.Unique
    file_msg = text "In module" <+> quotes (ppr $ wo_module opts)

{- Note [Worker/wrapper for Strictness and Absence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The worker/wrapper transformation, mkWWstr_one, takes concrete action
based on the 'UnboxingDescision' returned by 'wantToUnboxArg'.
The latter takes into account several possibilities to decide if the
function is worthy for splitting:

1. If an argument is absent, it would be silly to pass it to
   the worker.  Hence the DropAbsent case.  This case must come
   first because the bottom demand B is also strict.
   E.g. B comes from a function like
       f x = error "urk"
   and the absent demand A can come from Note [Unboxing evaluated arguments]

2. If the argument is evaluated strictly (or known to be eval'd),
   we can take a view into the product demand ('viewProd'). In accordance
   with Note [Boxity analysis], 'wantToUnboxArg' will say 'Unbox'.
   'mkWWstr_one' then follows suit it and recurses into the fields of the
   product demand. For example

     f :: (Int, Int) -> Int
     f p = (case p of (a,b) -> a) + 1
   is split to
     f :: (Int, Int) -> Int
     f p = case p of (a,b) -> $wf a

     $wf :: Int -> Int
     $wf a = a + 1

   and
     g :: Bool -> (Int, Int) -> Int
     g c p = case p of (a,b) ->
                if c then a else b
   is split to
     g c p = case p of (a,b) -> $gw c a b
     $gw c a b = if c then a else b

2a But do /not/ split if Boxity Analysis said "Boxed".
   In this case, 'wantToUnboxArg' returns 'StopUnboxing'.
   Otherwise we risk decomposing and reboxing a massive
   tuple which is barely used. Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

   Here, f does not take 'pr' apart, and it's stupid to do so.
   Imagine that it had millions of fields. This actually happened
   in GHC itself where the tuple was DynFlags

3. In all other cases (e.g., lazy, used demand and not eval'd),
   'finaliseBoxity' will have cleared the Boxity flag to 'Boxed'
   (see Note [Finalising boxity for demand signature]) and
   'wantToUnboxArg' returns 'StopUnboxing' so that 'mkWWstr_one'
   stops unboxing.

Note [Worker/wrapper for bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used not to split if the result is bottom.
[Justification:  there's no efficiency to be gained.]

But it's sometimes bad not to make a wrapper.  Consider
        fw = \x# -> let x = I# x# in case e of
                                        p1 -> error_fn x
                                        p2 -> error_fn x
                                        p3 -> the real stuff
The re-boxing code won't go away unless error_fn gets a wrapper too.
[We don't do reboxing now, but in general it's better to pass an
unboxed thing to f, and have it reboxed in the error cases....]

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
 (A) The arg binders x1,x2 in mkWstr_one via mkUnpackCase
         See #13077, test T13077
 (B) The result binders r1,r2 in mkWWcpr_entry
         See Trace #13077, test T13077a
         And #13027 comment:20, item (4)
to record that the relevant binder is evaluated.

Note [Absent fillers]
~~~~~~~~~~~~~~~~~~~~~
Consider

  data T = MkT [Int] [Int] ![Int]  -- NB: last field is strict
  f :: T -> Int# -> blah
  f ps w = case ps of MkT xs ys zs -> <body mentioning xs>

Then f gets a strictness sig of <S(L,A,A)><A>. We make a worker $wf thus:

  $wf :: [Int] -> blah
  $wf xs = case ps of MkT xs _ _ -> <body mentioning xs>
    where
      ys = absentError "ys :: [Int]"
      zs = RUBBISH[LiftedRep] @[Int]
      ps = MkT xs ys zs
      w  = RUBBISH[IntRep] @Int#

The absent arguments 'ys', 'zs' and 'w' aren't even passed to the worker.
And neither should they! They are never used, their value is irrelevant (hence
they are *dead code*) and they are probably discarded after the next run of the
Simplifier (when they are in fact *unreachable code*). Yet, we have to come up
with "filler" values that we bind the absent arg Ids to.

That is exactly what Note [Rubbish literals] are for: A convenient way to
conjure filler values at any type (and any representation or levity!).

Needless to say, there are some wrinkles:

  1. In case we have a absent, /lazy/, and /lifted/ arg, we use an error-thunk
     instead. If absence analysis was wrong (e.g., #11126) and the binding
     in fact is used, then we get a nice panic message instead of undefined
     runtime behavior (See Modes of failure from Note [Rubbish literals]).

     Obviously, we can't use an error-thunk if the value is of unlifted rep
     (like 'Int#' or 'MutVar#'), because we'd immediately evaluate the panic.

  2. We also mustn't put an error-thunk (that fills in for an absent value of
     lifted rep) in a strict field, because #16970 establishes the invariant
     that strict fields are always evaluated, by (re-)evaluating what is put in
     a strict field. That's the reason why 'zs' binds a rubbish literal instead
     of an error-thunk, see #19133.

     How do we detect when we are about to put an error-thunk in a strict field?
     Ideally, we'd just look at the 'StrictnessMark' of the DataCon's field, but
     it's quite nasty to thread the marks though 'mkWWstr' and 'mkWWstr_one'.
     So we rather look out for a necessary condition for strict fields:
     Note [Unboxing evaluated arguments] makes it so that the demand on
     'zs' is absent and /strict/: It will get cardinality 'C_10', the empty
     interval, rather than 'C_00'. Hence the 'isStrictDmd' check: It guarantees
     we never fill in an error-thunk for an absent strict field.
     But that also means we emit a rubbish lit for other args that have
     cardinality 'C_10' (say, the arg to a bottoming function) where we could've
     used an error-thunk, but that's a small price to pay for simplicity.

     In #19766, we discovered that even if the binder has eval cardinality
     'C_00', it may end up in a strict field, with no surrounding seq
     whatsoever! That happens if the calling code has already evaluated
     said lambda binder, which will then have an evaluated unfolding
     ('isEvaldUnfolding'). That in turn tells the Simplifier it is free to drop
     the seq. So we better don't fill in an error-thunk for eval'd arguments
     either, just in case it ends up in a strict field!

  3. We can only emit a LitRubbish if the arg's type @arg_ty@ is mono-rep, e.g.
     of the form @TYPE rep@ where @rep@ is not (and doesn't contain) a variable.
     Why? Because if we don't know its representation (e.g. size in memory,
     register class), we don't know what or how much rubbish to emit in codegen.
     'typeMonoPrimRep_maybe' returns 'Nothing' in this case and we simply fall
     back to passing the original parameter to the worker.

     Note that currently this case should not occur, because binders always
     have to be representation monomorphic. But in the future, we might allow
     levity polymorphism, e.g. a polymorphic levity variable in 'BoxedRep'.

While (1) and (2) are simply an optimisation in terms of compiler debugging
experience, (3) should be irrelevant in most programs, if not all.

Historical note: I did try the experiment of using an error thunk for unlifted
things too, relying on the simplifier to drop it as dead code.  But this is
fragile

 - It fails when profiling is on, which disables various optimisations

 - It fails when reboxing happens. E.g.
      data T = MkT Int Int#
      f p@(MkT a _) = ...g p....
   where g is /lazy/ in 'p', but only uses the first component.  Then
   'f' is /strict/ in 'p', and only uses the first component.  So we only
   pass that component to the worker for 'f', which reconstructs 'p' to
   pass it to 'g'.  Alas we can't say
       ...f (MkT a (absentError Int# "blah"))...
   because `MkT` is strict in its Int# argument, so we get an absentError
   exception when we shouldn't.  Very annoying!

************************************************************************
*                                                                      *
         Type scrutiny that is specific to demand analysis
*                                                                      *
************************************************************************
-}

-- | Exactly 'dataConInstArgTys', but lacks the (ASSERT'ed) precondition that
-- the 'DataCon' may not have existentials. The lack of cloning the existentials
-- compared to 'dataConInstExAndArgVars' makes this function \"dubious\";
-- only use it where type variables aren't substituted for!
dubiousDataConInstArgTys :: DataCon -> [Type] -> [Type]
dubiousDataConInstArgTys dc tc_args = arg_tys
  where
    univ_tvs = dataConUnivTyVars dc
    ex_tvs   = dataConExTyCoVars dc
    subst    = extendTCvInScopeList (zipTvSubst univ_tvs tc_args) ex_tvs
    arg_tys  = map (GHC.Core.Type.substTy subst . scaledThing) (dataConRepArgTys dc)

findTypeShape :: FamInstEnvs -> Type -> TypeShape
-- Uncover the arrow and product shape of a type
-- The data type TypeShape is defined in GHC.Types.Demand
-- See Note [Trimming a demand to a type] in GHC.Core.Opt.DmdAnal
findTypeShape fam_envs ty
  = go (setRecTcMaxBound 2 initRecTc) ty
       -- You might think this bound of 2 is low, but actually
       -- I think even 1 would be fine.  This only bites for recursive
       -- product types, which are rare, and we really don't want
       -- to look deep into such products -- see #18034
  where
    go rec_tc ty
       | Just (_, _, res) <- splitFunTy_maybe ty
       = TsFun (go rec_tc res)

       | Just (tc, tc_args)  <- splitTyConApp_maybe ty
       = go_tc rec_tc tc tc_args

       | Just (_, ty') <- splitForAllTyCoVar_maybe ty
       = go rec_tc ty'

       | otherwise
       = TsUnk

    go_tc rec_tc tc tc_args
       | Just (HetReduction (Reduction _ rhs) _) <- topReduceTyFamApp_maybe fam_envs tc tc_args
       = go rec_tc rhs

       | Just con <- tyConSingleAlgDataCon_maybe tc
       , Just rec_tc <- if isTupleTyCon tc
                        then Just rec_tc
                        else checkRecTc rec_tc tc
         -- We treat tuples specially because they can't cause loops.
         -- Maybe we should do so in checkRecTc.
         -- The use of 'dubiousDataConInstArgTys' is OK, since this
         -- function performs no substitution at all, hence the uniques
         -- don't matter.
         -- We really do encounter existentials here, see
         -- Note [Which types are unboxed?] for an example.
       = TsProd (map (go rec_tc) (dubiousDataConInstArgTys con tc_args))

       | Just (ty', _) <- instNewTyCon_maybe tc tc_args
       , Just rec_tc <- checkRecTc rec_tc tc
       = go rec_tc ty'

       | otherwise
       = TsUnk

-- | Returned by 'isRecDataCon'.
-- See also Note [Detecting recursive data constructors].
data IsRecDataConResult
  = DefinitelyRecursive  -- ^ The algorithm detected a loop
  | NonRecursiveOrUnsure -- ^ The algorithm detected no loop, went out of fuel
                         -- or hit an .hs-boot file
  deriving (Eq, Show)

instance Outputable IsRecDataConResult where
  ppr = text . show

combineIRDCR :: IsRecDataConResult -> IsRecDataConResult -> IsRecDataConResult
combineIRDCR DefinitelyRecursive _                   = DefinitelyRecursive
combineIRDCR _                   DefinitelyRecursive = DefinitelyRecursive
combineIRDCR _                   _                   = NonRecursiveOrUnsure

combineIRDCRs :: [IsRecDataConResult] -> IsRecDataConResult
combineIRDCRs = foldl' combineIRDCR NonRecursiveOrUnsure
{-# INLINE combineIRDCRs #-}

-- | @isRecDataCon _ fuel dc@, where @tc = dataConTyCon dc@ returns
--
--   * @Just Recursive@ if the analysis found that @tc@ is reachable through one
--     of @dc@'s fields
--   * @Just NonRecursive@ if the analysis found that @tc@ is not reachable
--     through one of @dc@'s fields
--   * @Nothing@ is returned in two cases. The first is when @fuel /= Infinity@
--     and @f@ expansions of nested data TyCons were not enough to prove
--     non-recursivenss, nor arrive at an occurrence of @tc@ thus proving
--     recursiveness. The other is when we hit an abstract TyCon (one without
--     visible DataCons), such as those imported from .hs-boot files.
--
-- If @fuel = 'Infinity'@ and there are no boot files involved, then the result
-- is never @Nothing@ and the analysis is a depth-first search. If @fuel = 'Int'
-- f@, then the analysis behaves like a depth-limited DFS and returns @Nothing@
-- if the search was inconclusive.
--
-- See Note [Detecting recursive data constructors] for which recursive DataCons
-- we want to flag.
isRecDataCon :: FamInstEnvs -> IntWithInf -> DataCon -> IsRecDataConResult
isRecDataCon fam_envs fuel dc
  | isTupleDataCon dc || isUnboxedSumDataCon dc
  = NonRecursiveOrUnsure
  | otherwise
  = -- pprTrace "isRecDataCon" (ppr dc <+> dcolon <+> ppr (dataConRepType dc) $$ ppr fuel $$ ppr answer)
    answer
  where
    answer = go_dc fuel (setRecTcMaxBound 1 initRecTc) dc
    (<||>) = combineIRDCR

    go_dc :: IntWithInf -> RecTcChecker -> DataCon -> IsRecDataConResult
    go_dc fuel rec_tc dc =
      combineIRDCRs [ go_arg_ty fuel rec_tc (scaledThing arg_ty)
                    | arg_ty <- dataConRepArgTys dc ]

    go_arg_ty :: IntWithInf -> RecTcChecker -> Type -> IsRecDataConResult
    go_arg_ty fuel rec_tc ty
      --- | pprTrace "arg_ty" (ppr ty) False = undefined

      | Just (_, _arg_ty, _res_ty) <- splitFunTy_maybe ty
      -- = go_arg_ty fuel rec_tc _arg_ty <||> go_arg_ty fuel rec_tc _res_ty
          -- Plausible, but unnecessary for CPR.
          -- See Note [Detecting recursive data constructors], point (1)
      = NonRecursiveOrUnsure

      | Just (_tcv, ty') <- splitForAllTyCoVar_maybe ty
      = go_arg_ty fuel rec_tc ty'
          -- See Note [Detecting recursive data constructors], point (2)

      | Just (tc, tc_args) <- splitTyConApp_maybe ty
      = combineIRDCRs (map (go_arg_ty fuel rec_tc) tc_args)
        <||> go_tc_app fuel rec_tc tc tc_args

      | otherwise
      = NonRecursiveOrUnsure

    -- | PRECONDITION: tc_args has no recursive occs
    -- See Note [Detecting recursive data constructors], point (5)
    go_tc_app :: IntWithInf -> RecTcChecker -> TyCon -> [Type] -> IsRecDataConResult
    go_tc_app fuel rec_tc tc tc_args
      --- | pprTrace "tc_app" (vcat [ppr tc, ppr tc_args]) False = undefined

      | tc == dataConTyCon dc
      = DefinitelyRecursive -- loop found!

      | isPrimTyCon tc
      = NonRecursiveOrUnsure

      | not $ tcIsRuntimeTypeKind $ tyConResKind tc
      = NonRecursiveOrUnsure

      | isAbstractTyCon tc   -- When tc has no DataCons, from an hs-boot file
      = NonRecursiveOrUnsure -- See Note [Detecting recursive data constructors], point (7)

      | isFamilyTyCon tc
      -- This is the only place where we look at tc_args
      -- See Note [Detecting recursive data constructors], point (5)
      = case topReduceTyFamApp_maybe fam_envs tc tc_args of
          Just (HetReduction (Reduction _ rhs) _) -> go_arg_ty fuel rec_tc rhs
          Nothing                                 -> DefinitelyRecursive -- we hit this case for 'Any'

      | otherwise
      = assertPpr (isAlgTyCon tc) (ppr tc <+> ppr dc) $
        case checkRecTc rec_tc tc of
          Nothing -> NonRecursiveOrUnsure
            -- we expanded this TyCon once already, no need to test it multiple times

          Just rec_tc'
            | Just (_tvs, rhs, _co) <- unwrapNewTyCon_maybe tc
                -- See Note [Detecting recursive data constructors], points (2) and (3)
            -> go_arg_ty fuel rec_tc' rhs

            | fuel < 0
            -> NonRecursiveOrUnsure -- that's why we track fuel!

            | let dcs = expectJust "isRecDataCon:go_tc_app" $ tyConDataCons_maybe tc
            -> combineIRDCRs (map (\dc -> go_dc (subWithInf fuel 1) rec_tc' dc) dcs)
                -- See Note [Detecting recursive data constructors], point (4)

-- | A specialised Bool for an argument to 'finaliseBoxity'.
-- See Note [Do not unbox class dictionaries].
data InsideInlineableFun
  = NotInsideInlineableFun -- ^ Not in an inlineable fun.
  | InsideInlineableFun    -- ^ We are in an inlineable fun, so we won't
                           -- unbox dictionary args.
  deriving Eq

-- | This function makes sure that the demand only says 'Unboxed' where
-- worker/wrapper should actually unbox and trims any boxity beyond that.
-- Called for every demand annotation during DmdAnal.
--
-- > data T a = T !a
-- > f :: (T (Int,Int), Int) -> ()
-- > f p = ... -- demand on p: 1!P(L!P(L!P(L), L!P(L)), L!P(L))
--
-- 'finaliseBoxity' will trim the demand on 'p' to 1!P(L!P(LP(L), LP(L)), LP(L)).
-- This is done when annotating lambdas and thunk bindings.
-- See Note [Finalising boxity for demand signature]
finaliseBoxity
  :: FamInstEnvs
  -> InsideInlineableFun    -- ^ See the haddocks on 'InsideInlineableFun'
  -> Type                   -- ^ Type of the argument
  -> Demand                 -- ^ How the arg was used
  -> Demand
finaliseBoxity env in_inl_fun ty dmd = go NotMarkedStrict ty dmd
  where
    go mark ty dmd@(n :* _) =
      case wantToUnboxArg env ty dmd of
        DropAbsent -> dmd
        Unbox DataConPatContext{dcpc_dc=dc, dcpc_tc_args=tc_args} ds
          -- See Note [No lazy, Unboxed demands in demand signature]
          -- See Note [Unboxing evaluated arguments]
          | isStrict n || isMarkedStrict mark
          -- See Note [Do not unbox class dictionaries]
          , in_inl_fun == NotInsideInlineableFun || not (isClassPred ty)
          -- See Note [mkWWstr and unsafeCoerce]
          , ds `lengthIs` dataConRepArity dc
          , let arg_tys = dubiousDataConInstArgTys dc tc_args
          -> -- pprTrace "finaliseBoxity:Unbox" (ppr ty $$ ppr dmd $$ ppr ds) $
             n :* (mkProd Unboxed $! zip_go_with_marks dc arg_tys ds)
        -- See Note [No nested Unboxed inside Boxed in demand signature]
        _ -> trimBoxity dmd

    -- See Note [Unboxing evaluated arguments]
    zip_go_with_marks dc arg_tys ds = case dataConWrapId_maybe dc of
      Nothing -> strictZipWith  (go NotMarkedStrict)          arg_tys ds
                    -- Shortcut when DataCon worker=wrapper
      Just _  -> strictZipWith3 go  (dataConRepStrictness dc) arg_tys ds

{-
************************************************************************
*                                                                      *
\subsection{Worker/wrapper for CPR}
*                                                                      *
************************************************************************
See Note [Worker/wrapper for CPR] for an overview.
-}

mkWWcpr_entry
  :: WwOpts
  -> Type                              -- function body
  -> Cpr                               -- CPR analysis results
  -> UniqSM (Bool,                     -- Is w/w'ing useful?
             CoreExpr -> CoreExpr,     -- New wrapper. 'nop_fn' if not useful
             CoreExpr -> CoreExpr,     -- New worker.  'nop_fn' if not useful
             Type)                     -- Type of worker's body.
                                       -- Just the input body_ty if not useful
-- ^ Entrypoint to CPR W/W. See Note [Worker/wrapper for CPR] for an overview.
mkWWcpr_entry opts body_ty body_cpr
  | not (wo_cpr_anal opts) = return (False, nop_fn, nop_fn, body_ty)
  | otherwise = do
    -- Part (1)
    res_bndr <- mk_res_bndr body_ty
    let bind_res_bndr body scope = mkDefaultCase body res_bndr scope

    -- Part (2)
    (useful, fromOL -> transit_vars, rebuilt_result, work_unpack_res) <-
      mkWWcpr_one opts res_bndr body_cpr

    -- Part (3)
    let (unbox_transit_tup, transit_tup) = move_transit_vars transit_vars

    -- Stacking unboxer (work_fn) and builder (wrap_fn) together
    let wrap_fn      = unbox_transit_tup rebuilt_result                 -- 3 2
        work_fn body = bind_res_bndr body (work_unpack_res transit_tup) -- 1 2 3
        work_body_ty = exprType transit_tup
    return $ if not useful
                then (False, nop_fn, nop_fn, body_ty)
                else (True, wrap_fn, work_fn, work_body_ty)

-- | Part (1) of Note [Worker/wrapper for CPR].
mk_res_bndr :: Type -> UniqSM Id
mk_res_bndr body_ty = do
  -- See Note [Linear types and CPR]
  bndr <- mkSysLocalOrCoVarM ww_prefix cprCaseBndrMult body_ty
  -- See Note [Record evaluated-ness in worker/wrapper]
  pure (setCaseBndrEvald MarkedStrict bndr)

-- | What part (2) of Note [Worker/wrapper for CPR] collects.
--
--   1. A Bool capturing whether the transformation did anything useful.
--   2. The list of transit variables (see the Note).
--   3. The result builder expression for the wrapper.  The original case binder if not useful.
--   4. The result unpacking expression for the worker. 'nop_fn' if not useful.
type CprWwResultOne  = (Bool, OrdList Var,  CoreExpr , CoreExpr -> CoreExpr)
type CprWwResultMany = (Bool, OrdList Var, [CoreExpr], CoreExpr -> CoreExpr)

mkWWcpr :: WwOpts -> [Id] -> [Cpr] -> UniqSM CprWwResultMany
mkWWcpr _opts vars []   =
  -- special case: No CPRs means all top (for example from FlatConCpr),
  -- hence stop WW.
  return (False, toOL vars, map varToCoreExpr vars, nop_fn)
mkWWcpr opts  vars cprs = do
  -- No existentials in 'vars'. 'wantToUnboxResult' should have checked that.
  massertPpr (not (any isTyVar vars)) (ppr vars $$ ppr cprs)
  massertPpr (equalLength vars cprs) (ppr vars $$ ppr cprs)
  (usefuls, varss, rebuilt_results, work_unpack_ress) <-
    unzip4 <$> zipWithM (mkWWcpr_one opts) vars cprs
  return ( or usefuls
         , concatOL varss
         , rebuilt_results
         , foldl' (.) nop_fn work_unpack_ress )

mkWWcpr_one :: WwOpts -> Id -> Cpr -> UniqSM CprWwResultOne
-- ^ See if we want to unbox the result and hand off to 'unbox_one_result'.
mkWWcpr_one opts res_bndr cpr
  | assert (not (isTyVar res_bndr) ) True
  , Unbox dcpc arg_cprs <- wantToUnboxResult (wo_fam_envs opts) (idType res_bndr) cpr
  = unbox_one_result opts res_bndr arg_cprs dcpc
  | otherwise
  = return (False, unitOL res_bndr, varToCoreExpr res_bndr, nop_fn)

unbox_one_result
  :: WwOpts -> Id -> [Cpr] -> DataConPatContext -> UniqSM CprWwResultOne
-- ^ Implements the main bits of part (2) of Note [Worker/wrapper for CPR]
unbox_one_result opts res_bndr arg_cprs
                 DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args
                                   , dcpc_co = co } = do
  -- unboxer (free in `res_bndr`):       |   builder (where <i> builds what was
  --   ( case res_bndr of (i, j) -> )    |            bound to i)
  --   ( case i of I# a ->          )    |
  --   ( case j of I# b ->          )    |     (      (<i>, <j>)      )
  --   ( <hole>                     )    |
  pat_bndrs_uniqs <- getUniquesM
  let (_exs, arg_ids) =
        dataConRepFSInstPat (repeat ww_prefix) pat_bndrs_uniqs cprCaseBndrMult dc tc_args
  massert (null _exs) -- Should have been caught by wantToUnboxResult

  (nested_useful, transit_vars, con_args, work_unbox_res) <-
    mkWWcpr opts arg_ids arg_cprs

  let -- rebuilt_result = (C a b |> sym co)
      rebuilt_result = mkConApp dc (map Type tc_args ++ con_args) `mkCast` mkSymCo co
      -- this_work_unbox_res alt = (case res_bndr |> co of C a b -> <alt>[a,b])
      this_work_unbox_res = mkUnpackCase (Var res_bndr) co cprCaseBndrMult dc arg_ids

  -- Don't try to WW an unboxed tuple return type when there's nothing inside
  -- to unbox further.
  return $ if isUnboxedTupleDataCon dc && not nested_useful
              then ( False, unitOL res_bndr, Var res_bndr, nop_fn )
              else ( True
                   , transit_vars
                   , rebuilt_result
                   , this_work_unbox_res . work_unbox_res
                   )

-- | Implements part (3) of Note [Worker/wrapper for CPR].
--
-- If `move_transit_vars [a,b] = (unbox, tup)` then
--     * `a` and `b` are the *transit vars* to be returned from the worker
--       to the wrapper
--     * `unbox scrut alt = (case <scrut> of (# a, b #) -> <alt>)`
--     * `tup = (# a, b #)`
-- There is a special case for when there's 1 transit var,
-- see Note [No unboxed tuple for single, unlifted transit var].
move_transit_vars :: [Id] -> (CoreExpr -> CoreExpr -> CoreExpr, CoreExpr)
move_transit_vars vars
  | [var] <- vars
  , let var_ty = idType var
  , isUnliftedType var_ty || exprIsHNF (Var var)
  -- See Note [No unboxed tuple for single, unlifted transit var]
  --   * Wrapper: `unbox scrut alt = (case <scrut> of a -> <alt>)`
  --   * Worker:  `tup = a`
  = ( \build_res wkr_call -> mkDefaultCase wkr_call var build_res
    , varToCoreExpr var ) -- varToCoreExpr important here: var can be a coercion
                          -- Lacking this caused #10658
  | otherwise
  -- The general case: Just return an unboxed tuple from the worker
  --   * Wrapper: `unbox scrut alt = (case <scrut> of (# a, b #) -> <alt>)`
  --   * Worker:  `tup = (# a, b #)`
  = ( \build_res wkr_call -> mkSingleAltCase wkr_call case_bndr
                                    (DataAlt tup_con) vars build_res
    , ubx_tup_app )
   where
    ubx_tup_app = mkCoreUbxTup (map idType vars) (map varToCoreExpr vars)
    tup_con     = tupleDataCon Unboxed (length vars)
    -- See also Note [Linear types and CPR]
    case_bndr   = mkWildValBinder cprCaseBndrMult (exprType ubx_tup_app)


{- Note [Worker/wrapper for CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'mkWWcpr_entry' is the entry-point to the worker/wrapper transformation that
exploits CPR info. Here's an example:
```
  f :: ... -> (Int, Int)
  f ... = <body>
```
Let's assume the CPR info `body_cpr` for the body of `f` says
"unbox the pair and its components" and `body_ty` is the type of the function
body `body` (i.e., `(Int, Int)`). Then `mkWWcpr_entry body_ty body_cpr` returns

  * A result-unpacking expression for the worker, with a hole for the fun body:
    ```
      unpack body = ( case <body> of r __DEFAULT -> )    -- (1)
                    ( case r of (i, j) ->           )    -- (2)
                    ( case i of I# a ->             )    -- (2)
                    ( case j of I# b ->             )    -- (2)
                    ( (# a, b #)                    )    -- (3)
    ```
  * A result-building expression for the wrapper, with a hole for the worker call:
    ```
      build wkr_call = ( case <wkr_call> of (# a, b #) -> )    -- (3)
                       ( (I# a, I# b)                     )    -- (2)
    ```
  * The result type of the worker, e.g., `(# Int#, Int# #)` above.

To achieve said transformation, 'mkWWcpr_entry'

  1. First allocates a fresh result binder `r`, giving a name to the `body`
     expression and contributing part (1) of the unpacker and builder.
  2. Then it delegates to 'mkWWcpr_one', which recurses into all result fields
     to unbox, contributing the parts marked with (2). Crucially, it knows
     what belongs in the case scrutinee of the unpacker through the communicated
     Id `r`: The unpacking expression will be free in that variable.
     (This is a similar contract as that of 'mkWWstr_one' for strict args.)
  3. 'mkWWstr_one' produces a bunch of *transit vars*: Those result variables
     that have to be transferred from the worker to the wrapper, where the
     constructed result can be rebuilt, `a` and `b` above. Part (3) is
     responsible for tupling them up in the worker and taking the tuple apart
     in the wrapper. This is implemented in 'move_transit_vars'.

Note [No unboxed tuple for single, unlifted transit var]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there's only a single, unlifted transit var (Note [Worker/wrapper for CPR]),
we don't wrap an unboxed singleton tuple around it (which otherwise would be
needed to suspend evaluation) and return the unlifted thing directly. E.g.
```
  f :: Int -> Int
  f x = x+1
```
We certainly want `$wf :: Int# -> Int#`, not `$wf :: Int# -> (# Int# #)`.
This is OK as long as we know that evaluation of the returned thing terminates
quickly, as is the case for fields of unlifted type like `Int#`.

But more generally, this should also be true for *lifted* types that terminate
quickly! Consider from `T18109`:
```
  data F = F (Int -> Int)
  f :: Int -> F
  f n = F (+n)

  data T = T (Int, Int)
  g :: T -> T
  g t@(T p) = p `seq` t

  data U = U ![Int]
  h :: Int -> U
  h n = U [0..n]
```
All of the nested fields are actually ok-for-speculation and thus OK to
return unboxed instead of in an unboxed singleton tuple:

 1. The field of `F` is a HNF.
    We want `$wf :: Int -> Int -> Int`.
    We get  `$wf :: Int -> (# Int -> Int #)`.
 2. The field of `T` is `seq`'d in `g`.
    We want `$wg :: (Int, Int) -> (Int, Int)`.
    We get  `$wg :: (Int, Int) -> (# (Int, Int) #)`.
 3. The field of `U` is strict and thus always evaluated.
    We want  `$wh :: Int# -> [Int]`.
    We'd get `$wh :: Int# -> (# [Int] #)`.

By considering vars as unlifted that satsify 'exprIsHNF', we catch (3).
Why not check for 'exprOkForSpeculation'? Quite perplexingly, evaluated vars
are not ok-for-spec, see Note [exprOkForSpeculation and evaluated variables].
For (1) and (2) we would have to look at the term. WW only looks at the
type and the CPR signature, so the only way to fix (1) and (2) would be to
have a nested termination signature, like in MR !1866.

Note [Linear types and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Remark on linearity: in both the case of the wrapper and the worker,
we build a linear case to unpack constructed products. All the
multiplicity information is kept in the constructors (both C and (#, #)).
In particular (#,#) is parametrised by the multiplicity of its fields.
Specifically, in this instance, the multiplicity of the fields of (#,#)
is chosen to be the same as those of C.


************************************************************************
*                                                                      *
\subsection{Utilities}
*                                                                      *
************************************************************************
-}

mkUnpackCase ::  CoreExpr -> Coercion -> Mult -> DataCon -> [Id] -> CoreExpr -> CoreExpr
-- (mkUnpackCase e co Con args body)
--      returns
-- case e |> co of _dead { Con args -> body }
mkUnpackCase (Tick tickish e) co mult con args body   -- See Note [Profiling and unpacking]
  = Tick tickish (mkUnpackCase e co mult con args body)
mkUnpackCase scrut co mult boxing_con unpk_args body
  = mkSingleAltCase casted_scrut bndr
                    (DataAlt boxing_con) unpk_args body
  where
    casted_scrut = scrut `mkCast` co
    bndr = mkWildValBinder mult (exprType casted_scrut)

-- | The multiplicity of a case binder unboxing a constructed result.
-- See Note [Linear types and CPR]
cprCaseBndrMult :: Mult
cprCaseBndrMult = One

ww_prefix :: FastString
ww_prefix = fsLit "ww"

{- Note [Profiling and unpacking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-}
