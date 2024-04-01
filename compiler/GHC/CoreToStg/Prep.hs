{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow, 1994-2006


Core pass to saturate constructors and PrimOps
-}

module GHC.CoreToStg.Prep
   ( CorePrepConfig (..)
   , CorePrepPgmConfig (..)
   , corePrepPgm
   , corePrepExpr
   , mkConvertNumLiteral
   )
where

import GHC.Prelude

import GHC.Platform

import GHC.Driver.Flags

import GHC.Tc.Utils.Env
import GHC.Unit

import GHC.Builtin.Names
import GHC.Builtin.Types

import GHC.Core.Utils
import GHC.Core.Opt.Arity
import GHC.Core.Lint    ( EndPassConfig(..), endPassIO )
import GHC.Core
import GHC.Core.Subst
import GHC.Core.Make hiding( FloatBind(..) )   -- We use our own FloatBind here
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Opt.OccurAnal

import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Data.FastString
import GHC.Data.Graph.UnVar

import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Utils.Monad  ( mapAccumLM )
import GHC.Utils.Logger

import GHC.Types.Demand
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Id.Make ( realWorldPrimId )
import GHC.Types.Basic
import GHC.Types.Name   ( Name, NamedThing(..), nameSrcSpan, isInternalName )
import GHC.Types.SrcLoc ( SrcSpan(..), realSrcLocSpan, mkRealSrcLoc )
import GHC.Types.Literal
import GHC.Types.Tickish
import GHC.Types.TyThing
import GHC.Types.Unique.Supply

import Data.List        ( unfoldr )
import Control.Monad

{-
Note [CorePrep Overview]
~~~~~~~~~~~~~~~~~~~~~~~~

The goal of this pass is to prepare for code generation.

1.  Saturate constructor and primop applications.

2.  Convert to A-normal form; that is, function arguments
    are always variables.

    * Use case for strict arguments:
        f E ==> case E of x -> f x
        (where f is strict)

    * Use let for non-trivial lazy arguments
        f E ==> let x = E in f x
        (were f is lazy and x is non-trivial)

3.  Similarly, convert any unboxed lets into cases.
    [I'm experimenting with leaving 'ok-for-speculation'
     rhss in let-form right up to this point.]

4.  Ensure that *value* lambdas only occur as the RHS of a binding
    (The code generator can't deal with anything else.)
    Type lambdas are ok, however, because the code gen discards them.

5.  ANF-isation results in additional bindings that can obscure values.
    We float these out; see Note [Floating in CorePrep].

6.  Clone all local Ids.  See Note [Cloning in CorePrep]

7.  Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

8.  Inject bindings for the "implicit" Ids:
        * Constructor wrappers
        * Constructor workers
    We want curried definitions for all of these in case they
    aren't inlined by some caller.

 9. Convert bignum literals into their core representation.

10. Uphold tick consistency while doing this: We move ticks out of
    (non-type) applications where we can, and make sure that we
    annotate according to scoping rules when floating.

11. Collect cost centres (including cost centres in unfoldings) if we're in
    profiling mode. We have to do this here because we won't have unfoldings
    after this pass (see `trimUnfolding` and Note [Drop unfoldings and rules].

12. Eliminate some magic Ids, specifically
     runRW# (\s. e)  ==>  e[readWorldId/s]
             lazy e  ==>  e (see Note [lazyId magic] in GHC.Types.Id.Make)
         noinline e  ==>  e
           nospec e  ==>  e
     ToDo:  keepAlive# ...
    This is done in cpeApp

This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.

Note [CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is the syntax of the Core produced by CorePrep:

    Trivial expressions
       arg ::= lit |  var
              | arg ty  |  /\a. arg
              | truv co  |  /\c. arg  |  arg |> co

    Applications
       app ::= lit  |  var  |  app arg  |  app ty  | app co | app |> co

    Expressions
       body ::= app
              | let(rec) x = rhs in body     -- Boxed only
              | case app of pat -> body
              | /\a. body | /\c. body
              | body |> co

    Right hand sides (only place where value lambdas can occur)
       rhs ::= /\a.rhs  |  \x.rhs  |  body

We define a synonym for each of these non-terminals.  Functions
with the corresponding name produce a result in that syntax.

Note [Cloning in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In CorePrep we
* Always clone non-CoVar Ids, so each has a unique Unique
* Sometimes clone CoVars and TyVars

We always clone non-CoVarIds, for three reasons

1. Things associated with labels in the final code must be truly unique in
   order to avoid labels being shadowed in the final output.

2. Even binders without info tables like function arguments or alternative
   bound binders must be unique at least in their type/unique combination.
   We only emit a single declaration for each binder when compiling to C
   so if binders are not unique we would either get duplicate declarations
   or misstyped variables. The later happend in #22402.

3. We heavily use unique-keyed maps in the backend which can go wrong when
   ids with the same unique are meant to represent the same variable.

Generally speaking we don't clone TyVars or CoVars. The code gen doesn't need
that (they are erased), and doing so would be tiresome because then we'd need
to substitute in types and coercions.  But sometimes need to: see
Note [Cloning CoVars and TyVars]

Note [Cloning CoVars and TyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally we don't need to clone TyVars and CoVars, but there is one occasion
when we do (see #24463).  When we have
    case unsafeEqualityProof ... of UnsafeRefl g -> ...
we try to float it, using UnsafeEqualityCase.
Why?  See (U3) in Note [Implementing unsafeCoerce]

Alas, floating it widens the scope of `g`, and that led to catastrophe in
#24463, when two identically-named g's shadowed.

Solution: clone `g`; see `cpCloneCoVarBndr`.

BUT once we clone `g` we must apply the cloning substitution to all types
and coercions.  But that in turn means that, given a binder like
   /\ (a :: kind |> g). blah
we must substitute in a's kind, and hence need to substitute for `a`
itself in `blah`.

So our plan is:
  * Maintain a full Subst in `cpe_subst`

  * Clone a CoVar when we we meet an `isUnsafeEqualityCase`;
    otherwise TyVar/CoVar binders are never cloned.

  * So generally the TCvSubst is empty

  * Apply the substitution to type and coercion arguments in Core; but
    happily `substTy` has a no-op short-cut for an empty TCvSubst, so this
    is usually very cheap.

  * In `cpCloneBndr`, for a tyvar/covar binder, check for an empty substitution;
    in that case just do nothing
-}

type CpeArg  = CoreExpr    -- Non-terminal 'arg'
type CpeApp  = CoreExpr    -- Non-terminal 'app'
type CpeBody = CoreExpr    -- Non-terminal 'body'
type CpeRhs  = CoreExpr    -- Non-terminal 'rhs'

{-
************************************************************************
*                                                                      *
                Top level stuff
*                                                                      *
************************************************************************
-}

data CorePrepPgmConfig = CorePrepPgmConfig
  { cpPgm_endPassConfig     :: !EndPassConfig
  , cpPgm_generateDebugInfo :: !Bool
  }

corePrepPgm :: Logger
            -> CorePrepConfig
            -> CorePrepPgmConfig
            -> Module -> ModLocation -> CoreProgram -> [TyCon]
            -> IO CoreProgram
corePrepPgm logger cp_cfg pgm_cfg
            this_mod mod_loc binds data_tycons =
    withTiming logger
               (text "CorePrep"<+>brackets (ppr this_mod))
               (\a -> a `seqList` ()) $ do
    us <- mkSplitUniqSupply 's'
    let initialCorePrepEnv = mkInitialCorePrepEnv cp_cfg

    let
        implicit_binds = mkDataConWorkers
          (cpPgm_generateDebugInfo pgm_cfg)
          mod_loc data_tycons
            -- NB: we must feed mkImplicitBinds through corePrep too
            -- so that they are suitably cloned and eta-expanded

        binds_out = initUs_ us $ do
                      floats1 <- corePrepTopBinds initialCorePrepEnv binds
                      floats2 <- corePrepTopBinds initialCorePrepEnv implicit_binds
                      return (deFloatTop (floats1 `zipFloats` floats2))

    endPassIO logger (cpPgm_endPassConfig pgm_cfg)
              binds_out []
    return binds_out

corePrepExpr :: Logger -> CorePrepConfig -> CoreExpr -> IO CoreExpr
corePrepExpr logger config expr = do
    withTiming logger (text "CorePrep [expr]") (\e -> e `seq` ()) $ do
      us <- mkSplitUniqSupply 's'
      let initialCorePrepEnv = mkInitialCorePrepEnv config
      let new_expr = initUs_ us (cpeBodyNF initialCorePrepEnv expr)
      putDumpFileMaybe logger Opt_D_dump_prep "CorePrep" FormatCore (ppr new_expr)
      return new_expr

corePrepTopBinds :: CorePrepEnv -> [CoreBind] -> UniqSM Floats
-- Note [Floating out of top level bindings]
corePrepTopBinds initialCorePrepEnv binds
  = go initialCorePrepEnv binds
  where
    go _   []             = return emptyFloats
    go env (bind : binds) = do (env', floats, maybe_new_bind)
                                 <- cpeBind TopLevel env bind
                               massert (isNothing maybe_new_bind)
                                 -- Only join points get returned this way by
                                 -- cpeBind, and no join point may float to top
                               floatss <- go env' binds
                               return (floats `zipFloats` floatss)

mkDataConWorkers :: Bool -> ModLocation -> [TyCon] -> [CoreBind]
-- See Note [Data constructor workers]
-- c.f. Note [Injecting implicit bindings] in GHC.Iface.Tidy
mkDataConWorkers generate_debug_info mod_loc data_tycons
  = [ NonRec id (tick_it (getName data_con) (Var id))
                                -- The ice is thin here, but it works
    | tycon <- data_tycons,     -- CorePrep will eta-expand it
      data_con <- tyConDataCons tycon,
      let id = dataConWorkId data_con
    ]
 where
   -- If we want to generate debug info, we put a source note on the
   -- worker. This is useful, especially for heap profiling.
   tick_it name
     | not generate_debug_info               = id
     | RealSrcSpan span _ <- nameSrcSpan name = tick span
     | Just file <- ml_hs_file mod_loc       = tick (span1 file)
     | otherwise                             = tick (span1 "???")
     where tick span  = Tick $ SourceNote span $
             LexicalFastString $ mkFastString $ renderWithContext defaultSDocContext $ ppr name
           span1 file = realSrcLocSpan $ mkRealSrcLoc (mkFastString file) 1 1

{- Note [Floating in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ANFisation risks producing a lot of nested lets that obscures values:
  let v = (:) (f 14) [] in e
  ==> { ANF in CorePrep }
  let v = let sat = f 14 in (:) sat [] in e
Here, `v` is not a value anymore, and we'd allocate a thunk closure for `v` that
allocates a thunk for `sat` and then allocates the cons cell.
Hence we carry around a bunch of floated bindings with us so that we again
expose the values:
  let v = let sat = f 14 in (:) sat [] in e
  ==> { Float sat }
  let sat = f 14 in
  let v = (:) sat [] in e
(We will not do this transformation if `v` does not become a value afterwards;
see Note [wantFloatLocal].)
If `v` is bound at the top-level, we might even float `sat` to top-level;
see Note [Floating out of top level bindings].
For nested let bindings, we have to keep in mind Note [Core letrec invariant]
and may exploit strict contexts; see Note [wantFloatLocal].

There are 3 main categories of floats, encoded in the `FloatingBind` type:

  * `Float`: A floated binding, as `sat` above.
    These come in different flavours as described by their `FloatInfo` and
    `BindInfo`, which captures how far the binding can be floated and whether or
    not we want to case-bind. See Note [BindInfo and FloatInfo].
  * `UnsafeEqualityCase`: Used for floating around unsafeEqualityProof bindings;
    see (U3) of Note [Implementing unsafeCoerce].
    It's exactly a `Float` that is `CaseBound` and `LazyContextFloatable`
    (see `mkNonRecFloat`), but one that has a non-DEFAULT Case alternative to
    bind the unsafe coercion field of the Refl constructor.
  * `FloatTick`: A floated `Tick`. See Note [Floating Ticks in CorePrep].

Note [Floating out of top level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: we do need to float out of top-level bindings
Consider        x = length [True,False]
We want to get
                s1 = False : []
                s2 = True  : s1
                x  = length s2

We return a *list* of bindings, because we may start with
        x* = f (g y)
where x is demanded, in which case we want to finish with
        a = g y
        x* = f a
And then x will actually end up case-bound

Note [Join points and floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Join points can float out of other join points but not out of value bindings:

  let z =
    let  w = ... in -- can float
    join k = ... in -- can't float
    ... jump k ...
  join j x1 ... xn =
    let  y = ... in -- can float (but don't want to)
    join h = ... in -- can float (but not much point)
    ... jump h ...
  in ...

Here, the jump to h remains valid if h is floated outward, but the jump to k
does not.

We don't float *out* of join points. It would only be safe to float out of
nullary join points (or ones where the arguments are all either type arguments
or dead binders). Nullary join points aren't ever recursive, so they're always
effectively one-shot functions, which we don't float out of. We *could* float
join points from nullary join points, but there's no clear benefit at this
stage.

Note [Data constructor workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Create any necessary "implicit" bindings for data con workers.  We
create the rather strange (non-recursive!) binding

        $wC = \x y -> $wC x y

i.e. a curried constructor that allocates.  This means that we can
treat the worker for a constructor like any other function in the rest
of the compiler.  The point here is that CoreToStg will generate a
StgConApp for the RHS, rather than a call to the worker (which would
give a loop).  As Lennart says: the ice is thin here, but it works.

Hmm.  Should we create bindings for dictionary constructors?  They are
always fully applied, and the bindings are just there to support
partial applications. But it's easier to let them through.


Note [Dead code in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Imagine that we got an input program like this (see #4962):

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g True (Just x) + g () (Just x), g)
    where
      g :: Show a => a -> Maybe Int -> Int
      g _ Nothing = x
      g y (Just z) = if z > 100 then g y (Just (z + length (show y))) else g y unknown

After specialisation and SpecConstr, we would get something like this:

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g$Bool_True_Just x + g$Unit_Unit_Just x, g)
    where
      {-# RULES g $dBool = g$Bool
                g $dUnit = g$Unit #-}
      g = ...
      {-# RULES forall x. g$Bool True (Just x) = g$Bool_True_Just x #-}
      g$Bool = ...
      {-# RULES forall x. g$Unit () (Just x) = g$Unit_Unit_Just x #-}
      g$Unit = ...
      g$Bool_True_Just = ...
      g$Unit_Unit_Just = ...

Note that the g$Bool and g$Unit functions are actually dead code: they
are only kept alive by the occurrence analyser because they are
referred to by the rules of g, which is being kept alive by the fact
that it is used (unspecialised) in the returned pair.

However, at the CorePrep stage there is no way that the rules for g
will ever fire, and it really seems like a shame to produce an output
program that goes to the trouble of allocating a closure for the
unreachable g$Bool and g$Unit functions.

The way we fix this is to:
 * In cloneBndr, drop all unfoldings/rules

 * In deFloatTop, run a simple dead code analyser on each top-level
   RHS to drop the dead local bindings.

The reason we don't just OccAnal the whole output of CorePrep is that
the tidier ensures that all top-level binders are GlobalIds, so they
don't show up in the free variables any longer. So if you run the
occurrence analyser on the output of CoreTidy (or later) you e.g. turn
this program:

  Rec {
  f = ... f ...
  }

Into this one:

  f = ... f ...

(Since f is not considered to be free in its own RHS.)


Note [keepAlive# magic]
~~~~~~~~~~~~~~~~~~~~~~~
When interacting with foreign code, it is often necessary for the user to
extend the lifetime of a heap object beyond the lifetime that would be apparent
from the on-heap references alone. For instance, a program like:

  foreign import safe "hello" hello :: ByteArray# -> IO ()

  callForeign :: IO ()
  callForeign = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
      unIO hello barr s1

As-written this program is susceptible to memory-unsafety since there are
no references to `barr` visible to the garbage collector. Consequently, if a
garbage collection happens during the execution of the C function `hello`, it
may be that the array is freed while in use by the foreign function.

To address this, we introduced a new primop, keepAlive#, which "scopes over"
the computation needing the kept-alive value:

  keepAlive# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep) (a :: TYPE a) (b :: TYPE b).
                a -> State# RealWorld -> (State# RealWorld -> b) -> b

When entered, an application (keepAlive# x s k) will apply `k` to the state
token, evaluating it to WHNF. However, during the course of this evaluation
will *guarantee* that `x` is considered to be alive.

There are a few things to note here:

 - we are RuntimeRep-polymorphic in the value to be kept-alive. This is
   necessary since we will often (but not always) be keeping alive something
   unlifted (like a ByteArray#)

 - we are RuntimeRep-polymorphic in the result value since the result may take
   many forms (e.g. a boxed value, a raw state token, or a (# State s, result #).

We implement this operation by desugaring to touch# during CorePrep (see
GHC.CoreToStg.Prep.cpeApp). Specifically,

  keepAlive# x s0 k

is transformed to:

  case k s0 of r ->
  case touch# x realWorld# of s1 ->
    r

Operationally, `keepAlive# x s k` is equivalent to pushing a stack frame with a
pointer to `x` and entering `k s0`. This compilation strategy is safe
because we do no optimization on STG that would drop or re-order the
continuation containing the `touch#`. However, if we were to become more
aggressive in our STG pipeline then we would need to revisit this.

Beyond this CorePrep transformation, there is very little special about
keepAlive#. However, we did explore (and eventually gave up on)
an optimisation which would allow unboxing of constructed product results,
which we describe below.


Lost optimisation: CPR unboxing
--------------------------------
One unfortunate property of this approach is that the simplifier is unable to
unbox the result of a keepAlive# expression. For instance, consider the program:

  case keepAlive# arr s0 (
         \s1 -> case peekInt arr s1 of
                  (# s2, r #) -> I# r
  ) of
    I# x -> ...

This is a surprisingly common pattern, previously used, e.g., in
GHC.IO.Buffer.readWord8Buf. While exploring ideas, we briefly played around
with optimising this away by pushing strict contexts (like the
`case [] of I# x -> ...` above) into keepAlive#'s continuation. While this can
recover unboxing, it can also unfortunately in general change the asymptotic
memory (namely stack) behavior of the program. For instance, consider

  writeN =
    ...
      case keepAlive# x s0 (\s1 -> something s1) of
        (# s2, x #) ->
          writeN ...

As it is tail-recursive, this program will run in constant space. However, if
we push outer case into the continuation we get:

  writeN =

      case keepAlive# x s0 (\s1 ->
        case something s1 of
          (# s2, x #) ->
            writeN ...
      ) of
        ...

Which ends up building a stack which is linear in the recursion depth. For this
reason, we ended up giving up on this optimisation.


Historical note: touch# and its inadequacy
------------------------------------------
Prior to the introduction of `keepAlive#` we instead addressed the need for
lifetime extension with the `touch#` primop:

    touch# :: a -> State# s -> State# s

This operation would ensure that the `a` value passed as the first argument was
considered "alive" at the time the primop application is entered.

For instance, the user might modify `callForeign` as:

  callForeign :: IO ()
  callForeign s0 = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
    case unIO hello barr s1 of (# s2, () #) ->
    case touch# barr s2 of s3 ->
      (# s3, () #)

However, in #14346 we discovered that this primop is insufficient in the
presence of simplification. For instance, consider a program like:

  callForeign :: IO ()
  callForeign s0 = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
    case unIO (forever $ hello barr) s1 of (# s2, () #) ->
    case touch# barr s2 of s3 ->
      (# s3, () #)

In this case the Simplifier may realize that (forever $ hello barr)
will never return and consequently that the `touch#` that follows is dead code.
As such, it will be dropped, resulting in memory unsoundness.
This unsoundness lead to the introduction of keepAlive#.



Other related tickets:

 - #15544
 - #17760
 - #14375
 - #15260
 - #18061

************************************************************************
*                                                                      *
                The main code
*                                                                      *
************************************************************************
-}

cpeBind :: TopLevelFlag -> CorePrepEnv -> CoreBind
        -> UniqSM (CorePrepEnv,
                   Floats,         -- Floating value bindings
                   Maybe CoreBind) -- Just bind' <=> returned new bind; no float
                                   -- Nothing <=> added bind' to floats instead
cpeBind top_lvl env (NonRec bndr rhs)
  | not (isJoinId bndr)
  = do { (env1, bndr1) <- cpCloneBndr env bndr
       ; let dmd         = idDemandInfo bndr
             is_unlifted = isUnliftedType (idType bndr)
       ; (floats, rhs1) <- cpePair top_lvl NonRecursive
                                   dmd is_unlifted
                                   env bndr1 rhs
       -- See Note [Inlining in CorePrep]
       ; let triv_rhs = exprIsTrivial rhs1
             env2    | triv_rhs  = extendCorePrepEnvExpr env1 bndr rhs1
                     | otherwise = env1
             floats1 | triv_rhs, isInternalName (idName bndr)
                     = floats
                     | otherwise
                     = snocFloat floats new_float

             new_float = mkNonRecFloat env dmd is_unlifted bndr1 rhs1

       ; return (env2, floats1, Nothing) }

  | otherwise -- A join point; see Note [Join points and floating]
  = assert (not (isTopLevel top_lvl)) $ -- can't have top-level join point
    do { (_, bndr1) <- cpCloneBndr env bndr
       ; (bndr2, rhs1) <- cpeJoinPair env bndr1 rhs
       ; return (extendCorePrepEnv env bndr bndr2,
                 emptyFloats,
                 Just (NonRec bndr2 rhs1)) }

cpeBind top_lvl env (Rec pairs)
  | not (isJoinId (head bndrs))
  = do { (env, bndrs1) <- cpCloneBndrs env bndrs
       ; let env' = enterRecGroupRHSs env bndrs1
       ; stuff <- zipWithM (cpePair top_lvl Recursive topDmd False env')
                           bndrs1 rhss

       ; let (zipManyFloats -> floats, rhss1) = unzip stuff
             -- Glom all floats into the Rec, *except* FloatStrings; see
             -- see Note [ANF-ising literal string arguments], Wrinkle (FS1)
             is_lit (Float (NonRec _ rhs) CaseBound TopLvlFloatable) = exprIsTickedString rhs
             is_lit _                                                = False
             (string_floats, top) = partitionOL is_lit (fs_binds floats)
                 -- Strings will *always* be in `top_floats` (we made sure of
                 -- that in `snocOL`), so that's the only field we need to
                 -- partition.
             floats'   = floats { fs_binds = top }
             all_pairs = foldrOL add_float (bndrs1 `zip` rhss1) (getFloats floats')
       -- use env below, so that we reset cpe_rec_ids
       ; return (extendCorePrepEnvList env (bndrs `zip` bndrs1),
                 snocFloat (emptyFloats { fs_binds = string_floats })
                           (Float (Rec all_pairs) LetBound TopLvlFloatable),
                 Nothing) }

  | otherwise -- See Note [Join points and floating]
  = do { (env, bndrs1) <- cpCloneBndrs env bndrs
       ; let env' = enterRecGroupRHSs env bndrs1
       ; pairs1 <- zipWithM (cpeJoinPair env') bndrs1 rhss

       ; let bndrs2 = map fst pairs1
       -- use env below, so that we reset cpe_rec_ids
       ; return (extendCorePrepEnvList env (bndrs `zip` bndrs2),
                 emptyFloats,
                 Just (Rec pairs1)) }
  where
    (bndrs, rhss) = unzip pairs

    -- Flatten all the floats, and the current
    -- group into a single giant Rec
    add_float (Float bind bound _) prs2
      | bound /= CaseBound
      || all (definitelyLiftedType . idType) (bindersOf bind)
           -- The latter check is hit in -O0 (i.e., flavours quick, devel2)
           -- for dictionary args which haven't been floated out yet, #24102.
           -- They are preferably CaseBound, but since they are lifted we may
           -- just as well put them in the Rec, in contrast to lifted bindings.
      = case bind of
          NonRec x e -> (x,e) : prs2
          Rec prs1 -> prs1 ++ prs2
    add_float f _ = pprPanic "cpeBind" (ppr f)


---------------
cpePair :: TopLevelFlag -> RecFlag -> Demand -> Bool
        -> CorePrepEnv -> OutId -> CoreExpr
        -> UniqSM (Floats, CpeRhs)
-- Used for all bindings
-- The binder is already cloned, hence an OutId
cpePair top_lvl is_rec dmd is_unlifted env bndr rhs
  = assert (not (isJoinId bndr)) $ -- those should use cpeJoinPair
    do { (floats1, rhs1) <- cpeRhsE env rhs

       -- See if we are allowed to float this stuff out of the RHS
       ; let dec = want_float_from_rhs floats1 rhs1
       ; (floats2, rhs2) <- executeFloatDecision dec floats1 rhs1

       -- Make the arity match up
       ; (floats3, rhs3)
            <- if manifestArity rhs1 <= arity
               then return (floats2, cpeEtaExpand arity rhs2)
               else warnPprTrace True "CorePrep: silly extra arguments:" (ppr bndr) $
                               -- Note [Silly extra arguments]
                    (do { v <- newVar (idType bndr)
                        ; let float = mkNonRecFloat env topDmd False v rhs2
                        ; return ( snocFloat floats2 float
                                 , cpeEtaExpand arity (Var v)) })

        -- Wrap floating ticks
       ; let (floats4, rhs4) = wrapTicks floats3 rhs3

       ; return (floats4, rhs4) }
  where
    arity = idArity bndr        -- We must match this arity

    want_float_from_rhs floats rhs
      | isTopLevel top_lvl = wantFloatTop floats
      | otherwise          = wantFloatLocal is_rec dmd is_unlifted floats rhs

{- Note [Silly extra arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we had this
        f{arity=1} = \x\y. e
We *must* match the arity on the Id, so we have to generate
        f' = \x\y. e
        f  = \x. f' x

It's a bizarre case: why is the arity on the Id wrong?  Reason
(in the days of __inline_me__):
        f{arity=0} = __inline_me__ (let v = expensive in \xy. e)
When InlineMe notes go away this won't happen any more.  But
it seems good for CorePrep to be robust.
-}

---------------
cpeJoinPair :: CorePrepEnv -> JoinId -> CoreExpr
            -> UniqSM (JoinId, CpeRhs)
-- Used for all join bindings
-- No eta-expansion: see Note [Do not eta-expand join points] in GHC.Core.Opt.Simplify.Utils
cpeJoinPair env bndr rhs
  = assert (isJoinId bndr) $
    do { let JoinPoint join_arity = idJoinPointHood bndr
             (bndrs, body)        = collectNBinders join_arity rhs

       ; (env', bndrs') <- cpCloneBndrs env bndrs

       ; body' <- cpeBodyNF env' body -- Will let-bind the body if it starts
                                      -- with a lambda

       ; let rhs'  = mkCoreLams bndrs' body'
             bndr' = bndr `setIdUnfolding` evaldUnfolding
                          `setIdArity` count isId bndrs
                            -- See Note [Arity and join points]

       ; return (bndr', rhs') }

{-
Note [Arity and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Up to now, we've allowed a join point to have an arity greater than its join
arity (minus type arguments), since this is what's useful for eta expansion.
However, for code gen purposes, its arity must be exactly the number of value
arguments it will be called with, and it must have exactly that many value
lambdas. Hence if there are extra lambdas we must let-bind the body of the RHS:

  join j x y z = \w -> ... in ...
    =>
  join j x y z = (let f = \w -> ... in f) in ...

This is also what happens with Note [Silly extra arguments]. Note that it's okay
for us to mess with the arity because a join point is never exported.
-}

-- ---------------------------------------------------------------------------
--              CpeRhs: produces a result satisfying CpeRhs
-- ---------------------------------------------------------------------------

cpeRhsE :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- If
--      e  ===>  (bs, e')
-- then
--      e = let bs in e'        (semantically, that is!)
--
-- For example
--      f (g x)   ===>   ([v = g x], f v)

cpeRhsE env (Type ty)
  = return (emptyFloats, Type (cpSubstTy env ty))
cpeRhsE env (Coercion co)
  = return (emptyFloats, Coercion (cpSubstCo env co))
cpeRhsE env expr@(Lit (LitNumber nt i))
   = case cp_convertNumLit (cpe_config env) nt i of
      Nothing -> return (emptyFloats, expr)
      Just e  -> cpeRhsE env e
cpeRhsE _env expr@(Lit {}) = return (emptyFloats, expr)
cpeRhsE env expr@(Var {})  = cpeApp env expr
cpeRhsE env expr@(App {})  = cpeApp env expr

cpeRhsE env (Let bind body)
  = do { (env', bind_floats, maybe_bind') <- cpeBind NotTopLevel env bind
       ; (body_floats, body') <- cpeRhsE env' body
       ; let expr' = case maybe_bind' of Just bind' -> Let bind' body'
                                         Nothing    -> body'
       ; return (bind_floats `appFloats` body_floats, expr') }

cpeRhsE env (Tick tickish expr)
  -- Pull out ticks if they are allowed to be floated.
  | tickishFloatable tickish
  = do { (floats, body) <- cpeRhsE env expr
         -- See [Floating Ticks in CorePrep]
       ; return (FloatTick tickish `consFloat` floats, body) }
  | otherwise
  = do { body <- cpeBodyNF env expr
       ; return (emptyFloats, mkTick tickish' body) }
  where
    tickish' | Breakpoint ext n fvs modl <- tickish
             -- See also 'substTickish'
             = Breakpoint ext n (map (getIdFromTrivialExpr . lookupCorePrepEnv env) fvs) modl
             | otherwise
             = tickish

cpeRhsE env (Cast expr co)
   = do { (floats, expr') <- cpeRhsE env expr
        ; return (floats, Cast expr' (cpSubstCo env co)) }

cpeRhsE env expr@(Lam {})
   = do { let (bndrs,body) = collectBinders expr
        ; (env', bndrs') <- cpCloneBndrs env bndrs
        ; body' <- cpeBodyNF env' body
        ; return (emptyFloats, mkLams bndrs' body') }

cpeRhsE env (Case scrut bndr _ alts@[Alt con [covar] _])
  -- See (U3) in Note [Implementing unsafeCoerce]
  -- We need make the Case float, otherwise we get
  --   let x = case ... of UnsafeRefl co ->
  --           let y = expr in
  --           K y
  --   in f x
  -- instead of
  --   case ... of UnsafeRefl co ->
  --   let y = expr in
  --   let x = K y
  --   in f x
  -- Note that `x` is a value here. This is visible in the GHCi debugger tests
  -- (such as `print003`).
  | Just rhs <- isUnsafeEqualityCase scrut bndr alts
  = do { (floats_scrut, scrut) <- cpeBody env scrut

       ; (env, bndr')  <- cpCloneBndr env bndr
       ; (env, covar') <- cpCloneCoVarBndr env covar
                          -- Important: here we clone the CoVar
                          -- See Note [Cloning CoVars and TyVars]

         -- Up until here this should do exactly the same as the regular code
         -- path of `cpeRhsE Case{}`.
       ; (floats_rhs, rhs) <- cpeBody env rhs
         -- ... but we want to float `floats_rhs` as in (U3) so that rhs' might
         -- become a value
       ; let case_float = UnsafeEqualityCase scrut bndr' con [covar']
         -- NB: It is OK to "evaluate" the proof eagerly.
         --     Usually there's the danger that we float the unsafeCoerce out of
         --     a branching Case alt. Not so here, because the regular code path
         --     for `cpeRhsE Case{}` will not float out of alts.
             floats = snocFloat floats_scrut case_float `appFloats` floats_rhs
       ; return (floats, rhs) }

cpeRhsE env (Case scrut bndr ty alts)
  = do { (floats, scrut') <- cpeBody env scrut
       ; (env', bndr2) <- cpCloneBndr env bndr
       ; let alts'
               | cp_catchNonexhaustiveCases $ cpe_config env
               , not (altsAreExhaustive alts)
               = addDefault alts (Just err)
               | otherwise = alts
               where err = mkImpossibleExpr ty "cpeRhsE: missing case alternative"
       ; alts'' <- mapM (sat_alt env') alts'

       ; return (floats, Case scrut' bndr2 (cpSubstTy env ty) alts'') }
  where
    sat_alt env (Alt con bs rhs)
       = do { (env2, bs') <- cpCloneBndrs env bs
            ; rhs' <- cpeBodyNF env2 rhs
            ; return (Alt con bs' rhs') }

-- ---------------------------------------------------------------------------
--              CpeBody: produces a result satisfying CpeBody
-- ---------------------------------------------------------------------------

-- | Convert a 'CoreExpr' so it satisfies 'CpeBody', without
-- producing any floats (any generated floats are immediately
-- let-bound using 'wrapBinds').  Generally you want this, esp.
-- when you've reached a binding form (e.g., a lambda) and
-- floating any further would be incorrect.
cpeBodyNF :: CorePrepEnv -> CoreExpr -> UniqSM CpeBody
cpeBodyNF env expr
  = do { (floats, body) <- cpeBody env expr
       ; return (wrapBinds floats body) }

-- | Convert a 'CoreExpr' so it satisfies 'CpeBody'; also produce
-- a list of 'Floats' which are being propagated upwards.  In
-- fact, this function is used in only two cases: to
-- implement 'cpeBodyNF' (which is what you usually want),
-- and in the case when a let-binding is in a case scrutinee--here,
-- we can always float out:
--
--      case (let x = y in z) of ...
--      ==> let x = y in case z of ...
--
cpeBody :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeBody)
cpeBody env expr
  = do { (floats1, rhs) <- cpeRhsE env expr
       ; (floats2, body) <- rhsToBody rhs
       ; return (floats1 `appFloats` floats2, body) }

--------
rhsToBody :: CpeRhs -> UniqSM (Floats, CpeBody)
-- Remove top level lambdas by let-binding

rhsToBody (Tick t expr)
  | tickishScoped t == NoScope  -- only float out of non-scoped annotations
  = do { (floats, expr') <- rhsToBody expr
       ; return (floats, mkTick t expr') }

rhsToBody (Cast e co)
        -- You can get things like
        --      case e of { p -> coerce t (\s -> ...) }
  = do { (floats, e') <- rhsToBody e
       ; return (floats, Cast e' co) }

rhsToBody expr@(Lam {})   -- See Note [No eta reduction needed in rhsToBody]
  | all isTyVar bndrs           -- Type lambdas are ok
  = return (emptyFloats, expr)
  | otherwise                   -- Some value lambdas
  = do { let rhs = cpeEtaExpand (exprArity expr) expr
       ; fn <- newVar (exprType rhs)
       ; let float = Float (NonRec fn rhs) LetBound TopLvlFloatable
       ; return (unitFloat float, Var fn) }
  where
    (bndrs,_) = collectBinders expr

rhsToBody expr = return (emptyFloats, expr)


{- Note [No eta reduction needed in rhsToBody]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Historical note.  In the olden days we used to have a Prep-specific
eta-reduction step in rhsToBody:
  rhsToBody expr@(Lam {})
    | Just no_lam_result <- tryEtaReducePrep bndrs body
    = return (emptyFloats, no_lam_result)

The goal was to reduce
        case x of { p -> \xs. map f xs }
    ==> case x of { p -> map f }

to avoid allocating a lambda.  Of course, we'd allocate a PAP
instead, which is hardly better, but that's the way it was.

Now we simply don't bother with this. It doesn't seem to be a win,
and it's extra work.
-}

-- ---------------------------------------------------------------------------
--              CpeApp: produces a result satisfying CpeApp
-- ---------------------------------------------------------------------------

data ArgInfo = CpeApp  CoreArg
             | CpeCast Coercion
             | CpeTick CoreTickish

instance Outputable ArgInfo where
  ppr (CpeApp arg) = text "app" <+> ppr arg
  ppr (CpeCast co) = text "cast" <+> ppr co
  ppr (CpeTick tick) = text "tick" <+> ppr tick

{- Note [Ticks and mandatory eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Something like
    `foo x = ({-# SCC foo #-} tagToEnum#) x :: Bool`
caused a compiler panic in #20938. Why did this happen?
The simplifier will eta-reduce the rhs giving us a partial
application of tagToEnum#. The tick is then pushed inside the
type argument. That is we get
    `(Tick<foo> tagToEnum#) @Bool`
CorePrep would go on to see a undersaturated tagToEnum# application
and eta expand the expression under the tick. Giving us:
    (Tick<scc> (\forall a. x -> tagToEnum# @a x) @Bool
Suddenly tagToEnum# is applied to a polymorphic type and the code generator
panics as it needs a concrete type to determine the representation.

The problem in my eyes was that the tick covers a partial application
of a primop. There is no clear semantic for such a construct as we can't
partially apply a primop since they do not have bindings.
We fix this by expanding the scope of such ticks slightly to cover the body
of the eta-expanded expression.

We do this by:
* Checking if an application is headed by a primOpish thing.
* If so we collect floatable ticks and usually but also profiling ticks
  along with regular arguments.
* When rebuilding the application we check if any profiling ticks appear
  before the primop is fully saturated.
* If the primop isn't fully satured we eta expand the primop application
  and scope the tick to scope over the body of the saturated expression.

Going back to #20938 this means starting with
    `(Tick<foo> tagToEnum#) @Bool`
we check if the function head is a primop (yes). This means we collect the
profiling tick like if it was floatable. Giving us
    (tagToEnum#, [CpeTick foo, CpeApp @Bool]).
cpe_app filters out the tick as a underscoped tick on the expression
`tagToEnum# @Bool`. During eta expansion we then put that tick back onto the
body of the eta-expansion lambdas. Giving us `\x -> Tick<foo> (tagToEnum# @Bool x)`.
-}
cpeApp :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- May return a CpeRhs because of saturating primops
cpeApp top_env expr
  = do { let (terminal, args) = collect_args expr
      --  ; pprTraceM "cpeApp" $ (ppr expr)
       ; cpe_app top_env terminal args
       }

  where
    -- We have a nested data structure of the form
    -- e `App` a1 `App` a2 ... `App` an, convert it into
    -- (e, [CpeApp a1, CpeApp a2, ..., CpeApp an], depth)
    -- We use 'ArgInfo' because we may also need to
    -- record casts and ticks.  Depth counts the number
    -- of arguments that would consume strictness information
    -- (so, no type or coercion arguments.)
    collect_args :: CoreExpr -> (CoreExpr, [ArgInfo])
    collect_args e = go e []
      where
        go (App fun arg)      as
            = go fun (CpeApp arg : as)
        go (Cast fun co)      as
            = go fun (CpeCast co : as)
        go (Tick tickish fun) as
            -- Profiling ticks are slightly less strict so we expand their scope
            -- if they cover partial applications of things like primOps.
            -- See Note [Ticks and mandatory eta expansion]
            -- Here we look inside `fun` before we make the final decision about
            -- floating the tick which isn't optimal for perf. But this only makes
            -- a difference if we have a non-floatable tick which is somewhat rare.
            | Var vh <- head
            , Var head' <- lookupCorePrepEnv top_env vh
            , etaExpansionTick head' tickish
            = (head,as')
            where
              (head,as') = go fun (CpeTick tickish : as)

        -- Terminal could still be an app if it's wrapped by a tick.
        -- E.g. Tick<foo> (f x) can give us (f x) as terminal.
        go terminal as = (terminal, as)

    cpe_app :: CorePrepEnv
            -> CoreExpr -- The thing we are calling
            -> [ArgInfo]
            -> UniqSM (Floats, CpeRhs)
    cpe_app env (Var f) (CpeApp Type{} : CpeApp arg : args)
        | f `hasKey` lazyIdKey          -- Replace (lazy a) with a, and
            -- See Note [lazyId magic] in GHC.Types.Id.Make
       || f `hasKey` noinlineIdKey || f `hasKey` noinlineConstraintIdKey
            -- Replace (noinline a) with a
            -- See Note [noinlineId magic] in GHC.Types.Id.Make
       || f `hasKey` nospecIdKey        -- Replace (nospec a) with a
            -- See Note [nospecId magic] in GHC.Types.Id.Make

        -- Consider the code:
        --
        --      lazy (f x) y
        --
        -- We need to make sure that we need to recursively collect arguments on
        -- "f x", otherwise we'll float "f x" out (it's not a variable) and
        -- end up with this awful -ddump-prep:
        --
        --      case f x of f_x {
        --        __DEFAULT -> f_x y
        --      }
        --
        -- rather than the far superior "f x y".  Test case is par01.
        = let (terminal, args') = collect_args arg
          in cpe_app env terminal (args' ++ args)

    -- runRW# magic
    cpe_app env (Var f) (CpeApp _runtimeRep@Type{} : CpeApp _type@Type{} : CpeApp arg : rest)
        | f `hasKey` runRWKey
        -- N.B. While it may appear that n == 1 in the case of runRW#
        -- applications, keep in mind that we may have applications that return
        , has_value_arg (CpeApp arg : rest)
        -- See Note [runRW magic]
        -- Replace (runRW# f) by (f realWorld#), beta reducing if possible (this
        -- is why we return a CorePrepEnv as well)
        = case arg of
            Lam s body -> cpe_app (extendCorePrepEnv env s realWorldPrimId) body rest
            _          -> cpe_app env arg (CpeApp (Var realWorldPrimId) : rest)
             -- TODO: What about casts?
        where
          has_value_arg [] = False
          has_value_arg (CpeApp arg:_rest)
            | not (isTyCoArg arg) = True
          has_value_arg (_:rest) = has_value_arg rest

    cpe_app env (Var v) args
      = do { v1 <- fiddleCCall v
           ; let e2 = lookupCorePrepEnv env v1
                 hd = getIdFromTrivialExpr_maybe e2
                 -- Determine number of required arguments. See Note [Ticks and mandatory eta expansion]
                 min_arity = case hd of
                   Just v_hd -> if hasNoBinding v_hd then Just $! (idArity v_hd) else Nothing
                   Nothing -> Nothing
          --  ; pprTraceM "cpe_app:stricts:" (ppr v <+> ppr args $$ ppr stricts $$ ppr (idCbvMarks_maybe v))
           ; (app, floats, unsat_ticks) <- rebuild_app env args e2 emptyFloats stricts min_arity
           ; mb_saturate hd app floats unsat_ticks depth }
        where
          depth = val_args args
          stricts = case idDmdSig v of
                            DmdSig (DmdType _ demands)
                              | listLengthCmp demands depth /= GT -> demands
                                    -- length demands <= depth
                              | otherwise                         -> []
                -- If depth < length demands, then we have too few args to
                -- satisfy strictness  info so we have to  ignore all the
                -- strictness info, e.g. + (error "urk")
                -- Here, we can't evaluate the arg strictly, because this
                -- partial application might be seq'd

        -- We inlined into something that's not a var and has no args.
        -- Bounce it back up to cpeRhsE.
    cpe_app env fun [] = cpeRhsE env fun

    -- Here we get:
    -- N-variable fun, better let-bind it
    -- This case covers literals, apps, lams or let expressions applied to arguments.
    -- Basically things we want to ANF before applying to arguments.
    cpe_app env fun args
      = do { (fun_floats, fun') <- cpeArg env evalDmd fun
                          -- If evalDmd says that it's sure to be evaluated,
                          -- we'll end up case-binding it
           ; (app, floats,unsat_ticks) <- rebuild_app env args fun' fun_floats [] Nothing
           ; mb_saturate Nothing app floats unsat_ticks (val_args args) }

    -- Count the number of value arguments *and* coercions (since we don't eliminate the later in STG)
    val_args :: [ArgInfo] -> Int
    val_args args = go args 0
      where
        go [] !n = n
        go (info:infos) n =
          case info of
            CpeCast {} -> go infos n
            CpeTick tickish
              | tickishFloatable tickish                 -> go infos n
              -- If we can't guarantee a tick will be floated out of the application
              -- we can't guarantee the value args following it will be applied.
              | otherwise                             -> n
            CpeApp e                                  -> go infos n'
              where
                !n'
                  | isTypeArg e = n
                  | otherwise   = n+1

    -- Saturate if necessary
    mb_saturate head app floats unsat_ticks depth =
       case head of
         Just fn_id -> do { sat_app <- maybeSaturate fn_id app depth unsat_ticks
                          ; return (floats, sat_app) }
         _other     -> do { massert (null unsat_ticks)
                          ; return (floats, app) }

    -- Deconstruct and rebuild the application, floating any non-atomic
    -- arguments to the outside.  We collect the type of the expression,
    -- the head of the application, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.
    rebuild_app
        :: CorePrepEnv
        -> [ArgInfo]                  -- The arguments (inner to outer)
        -> CpeApp                     -- The function
        -> Floats                     -- INVARIANT: These floats don't bind anything that is in the CpeApp!
                                      -- Just stuff floated out from the head of the application.
        -> [Demand]
        -> Maybe Arity
        -> UniqSM (CpeApp
                  ,Floats
                  ,[CoreTickish] -- Underscoped ticks. See Note [Ticks and mandatory eta expansion]
                  )
    rebuild_app env args app floats ss req_depth =
      rebuild_app' env args app floats ss [] (fromMaybe 0 req_depth)

    rebuild_app'
        :: CorePrepEnv
        -> [ArgInfo] -- The arguments (inner to outer)
        -> CpeApp
        -> Floats
        -> [Demand]
        -> [CoreTickish]
        -> Int -- Number of arguments required to satisfy minimal tick scopes.
        -> UniqSM (CpeApp, Floats, [CoreTickish])
    rebuild_app' _ [] app floats ss rt_ticks !_req_depth
      = assertPpr (null ss) (ppr ss)-- make sure we used all the strictness info
        return (app, floats, rt_ticks)

    rebuild_app' env (a : as) fun' floats ss rt_ticks req_depth = case a of
      -- See Note [Ticks and mandatory eta expansion]
      _
        | not (null rt_ticks)
        , req_depth <= 0
        ->
            let tick_fun = foldr mkTick fun' rt_ticks
            in rebuild_app' env (a : as) tick_fun floats ss rt_ticks req_depth

      CpeApp (Type arg_ty)
        -> rebuild_app' env as (App fun' (Type arg_ty')) floats ss rt_ticks req_depth
        where
           arg_ty' = cpSubstTy env arg_ty

      CpeApp (Coercion co)
        -> rebuild_app' env as (App fun' (Coercion co')) floats (drop 1 ss) rt_ticks req_depth
        where
           co' = cpSubstCo env co

      CpeApp arg -> do
        let (ss1, ss_rest)  -- See Note [lazyId magic] in GHC.Types.Id.Make
               = case (ss, isLazyExpr arg) of
                   (_   : ss_rest, True)  -> (topDmd, ss_rest)
                   (ss1 : ss_rest, False) -> (ss1,    ss_rest)
                   ([],            _)     -> (topDmd, [])
        (fs, arg') <- cpeArg top_env ss1 arg
        rebuild_app' env as (App fun' arg') (fs `zipFloats` floats) ss_rest rt_ticks (req_depth-1)

      CpeCast co
        -> rebuild_app' env as (Cast fun' co') floats ss rt_ticks req_depth
        where
           co' = cpSubstCo env co

      -- See Note [Ticks and mandatory eta expansion]
      CpeTick tickish
        | tickishPlace tickish == PlaceRuntime
        , req_depth > 0
        -> assert (isProfTick tickish) $
           rebuild_app' env as fun' floats ss (tickish:rt_ticks) req_depth
        | otherwise
        -- See [Floating Ticks in CorePrep]
        -> rebuild_app' env as fun' (snocFloat floats (FloatTick tickish)) ss rt_ticks req_depth

isLazyExpr :: CoreExpr -> Bool
-- See Note [lazyId magic] in GHC.Types.Id.Make
isLazyExpr (Cast e _)              = isLazyExpr e
isLazyExpr (Tick _ e)              = isLazyExpr e
isLazyExpr (Var f `App` _ `App` _) = f `hasKey` lazyIdKey
isLazyExpr _                       = False

{- Note [runRW magic]
~~~~~~~~~~~~~~~~~~~~~
Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB-}
              (_, s'') = fill_in_array_or_something a x s'
          in freezeArray# a s''

And now if we allow the @newArray#@ binding to float out to become a CAF,
we end up with a result that is totally and utterly wrong:

    f = let (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
        in \ x ->
            let (_, s'') = fill_in_array_or_something a x s'
            in freezeArray# a s''

All calls to @f@ will share a {\em single} array! Clearly this is nonsense and
must be prevented.

This is what @runRW#@ gives us: by being inlined extremely late in the
optimization (right before lowering to STG, in CorePrep), we can ensure that
no further floating will occur. This allows us to safely inline things like
@runST@, which are otherwise needlessly expensive (see #10678 and #5916).

'runRW' has a variety of quirks:

 * 'runRW' is known-key with a NOINLINE definition in
   GHC.Magic. This definition is used in cases where runRW is curried.

 * In addition to its normal Haskell definition in GHC.Magic, we give it
   a special late inlining here in CorePrep and GHC.StgToByteCode, avoiding
   the incorrect sharing due to float-out noted above.

 * It is levity-polymorphic:

    runRW# :: forall (r1 :: RuntimeRep). (o :: TYPE r)
           => (State# RealWorld -> (# State# RealWorld, o #))
           -> (# State# RealWorld, o #)

 * It has some special simplification logic to allow unboxing of results when
   runRW# appears in a strict context. See Note [Simplification of runRW#]
   below.

 * Since its body is inlined, we allow runRW#'s argument to contain jumps to
   join points. That is, the following is allowed:

    join j x = ...
    in runRW# @_ @_ (\s -> ... jump j 42 ...)

   The Core Linter knows about this. See Note [Linting of runRW#] in
   GHC.Core.Lint for details.

   The occurrence analyser and SetLevels also know about this, as described in
   Note [Simplification of runRW#].

Other relevant Notes:

 * Note [Simplification of runRW#] below, describing a transformation of runRW
   applications in strict contexts performed by the simplifier.
 * Note [Linting of runRW#] in GHC.Core.Lint
 * Note [runRW arg] below, describing a non-obvious case where the
   late-inlining could go wrong.

Note [runRW arg]
~~~~~~~~~~~~~~~~~~~
Consider the Core program (from #11291),

   runRW# (case bot of {})

The late inlining logic in cpe_app would transform this into:

   (case bot of {}) realWorld#

Which would rise to a panic in CoreToStg.myCollectArgs, which expects only
variables in function position.

However, as runRW#'s strictness signature captures the fact that it will call
its argument this can't happen: the simplifier will transform the bottoming
application into simply (case bot of {}).

Note that this reasoning does *not* apply to non-bottoming continuations like:

    hello :: Bool -> Int
    hello n =
      runRW# (
          case n of
            True -> \s -> 23
            _    -> \s -> 10)

Why? The difference is that (case bot of {}) is considered by okCpeArg to be
trivial, consequently cpeArg (which the catch-all case of cpe_app calls on both
the function and the arguments) will forgo binding it to a variable. By
contrast, in the non-bottoming case of `hello` above  the function will be
deemed non-trivial and consequently will be case-bound.

Note [Simplification of runRW#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the program,

    case runRW# (\s -> I# 42#) of
      I# n# -> f n#

There is no reason why we should allocate an I# constructor given that we
immediately destructure it.

To avoid this the simplifier has a special transformation rule, specific to
runRW#, that pushes a strict context into runRW#'s continuation.  See the
`runRW#` guard in `GHC.Core.Opt.Simplify.rebuildCall`.  That is, it transforms

    K[ runRW# @r @ty cont ]
              ~>
    runRW# @r @ty (\s -> K[cont s])

This has a few interesting implications. Consider, for instance, this program:

    join j = ...
    in case runRW# @r @ty cont of
         result -> jump j result

Performing the transform described above would result in:

    join j x = ...
    in runRW# @r @ty (\s ->
         case cont of in
           result -> jump j result
       )

If runRW# were a "normal" function this call to join point j would not be
allowed in its continuation argument. However, since runRW# is inlined (as
described in Note [runRW magic] above), such join point occurrences are
completely fine. Both occurrence analysis (see the runRW guard in occAnalApp)
and Core Lint (see the App case of lintCoreExpr) have special treatment for
runRW# applications. See Note [Linting of runRW#] for details on the latter.

Moreover, it's helpful to ensure that runRW's continuation isn't floated out
For instance, if we have

    runRW# (\s -> do_something)

where do_something contains only top-level free variables, we may be tempted to
float the argument to the top-level. However, we must resist this urge as since
doing so would then require that runRW# produce an allocation and call, e.g.:

    let lvl = \s -> do_somethign
    in
    ....(runRW# lvl)....

whereas without floating the inlining of the definition of runRW would result
in straight-line code. Consequently, GHC.Core.Opt.SetLevels.lvlApp has special
treatment for runRW# applications, ensure the arguments are not floated as
MFEs.

Now that we float evaluation context into runRW#, we also have to give runRW# a
special higher-order CPR transformer lest we risk #19822. E.g.,

  case runRW# (\s -> doThings) of x -> Data.Text.Text x something something'
      ~>
  runRW# (\s -> case doThings s of x -> Data.Text.Text x something something')

The former had the CPR property, and so should the latter.

Other considered designs
------------------------

One design that was rejected was to *require* that runRW#'s continuation be
headed by a lambda. However, this proved to be quite fragile. For instance,
SetLevels is very eager to float bottoming expressions. For instance given
something of the form,

    runRW# @r @ty (\s -> case expr of x -> undefined)

SetLevels will see that the body the lambda is bottoming and will consequently
float it to the top-level (assuming expr has no free coercion variables which
prevent this). We therefore end up with

    runRW# @r @ty (\s -> lvl s)

Which the simplifier will beta reduce, leaving us with

    runRW# @r @ty lvl

Breaking our desired invariant. Ultimately we decided to simply accept that
the continuation may not be a manifest lambda.


-- ---------------------------------------------------------------------------
--      CpeArg: produces a result satisfying CpeArg
-- ---------------------------------------------------------------------------

Note [ANF-ising literal string arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a Core program like,

    data Foo = Foo Addr#
    foo = Foo "turtle"#

String literals are non-trivial, see 'GHC.Types.Literal.litIsTrivial', hence
they are non-atomic in STG.
With -O1, FloatOut is likely to have floated most of these strings to top-level,
not least to give CSE a chance to deduplicate strings early (before the
linker, that is).
(Notable exceptions seem to be applications of 'unpackAppendCString#'.)
But with -O0, there is no FloatOut, so CorePrep must do the ANFisation to

    s = "turtle"#
    foo = Foo s

(String literals are the only kind of binding allowed at top-level and hence
their `FloatInfo` is `TopLvlFloatable`.)

This appears to lead to bad code if the arg is under a lambda, because CorePrep
doesn't float out of RHSs, e.g., (T23270)

    foo x = ... patError "turtle"# ...
==> foo x = ... case "turtle"# of s { __DEFAULT -> petError s } ...

This looks bad because it evals an HNF on every call.
But actually, it doesn't, because "turtle"# is already an HNF. Here is the Cmm:

  [section ""cstring" . cB4_str" {
       cB4_str:
           I8[] "turtle"
   }
  ...
  _sAG::I64 = cB4_str;
  R2 = _sAG::I64;
  Sp = Sp + 8;
  call Control.Exception.Base.patError_info(R2) args: 8, res: 0, upd: 8;

Wrinkles:

(FS1) We detect string literals in `cpeBind Rec{}` and float them out anyway;
      otherwise we'd try to bind a string literal in a letrec, violating
      Note [Core letrec invariant]. Since we know that literals don't have
      free variables, we float further.
      Arguably, we could just as well relax the letrec invariant for
      string literals, or anthing that is a value (lifted or not).
      This is tracked in #24036.
-}

-- This is where we arrange that a non-trivial argument is let-bound
cpeArg :: CorePrepEnv -> Demand
       -> CoreArg -> UniqSM (Floats, CpeArg)
cpeArg env dmd arg
  = do { (floats1, arg1) <- cpeRhsE env arg     -- arg1 can be a lambda
       ; let arg_ty      = exprType arg1
             is_unlifted = isUnliftedType arg_ty
             dec         = wantFloatLocal NonRecursive dmd is_unlifted floats1 arg1
       ; (floats2, arg2) <- executeFloatDecision dec floats1 arg1
                -- Else case: arg1 might have lambdas, and we can't
                --            put them inside a wrapBinds

       -- Now ANF-ise any non-trivial argument
       -- NB: "non-trivial" includes string literals;
       -- see Note [ANF-ising literal string arguments]
       ; if exprIsTrivial arg2
         then return (floats2, arg2)
         else do { v <- newVar arg_ty
                 -- See Note [Eta expansion of arguments in CorePrep]
                 ; let arity = cpeArgArity env dec arg2
                       arg3  = cpeEtaExpand arity arg2
                       arg_float = mkNonRecFloat env dmd is_unlifted v arg3
                 ; return (snocFloat floats2 arg_float, varToCoreExpr v) }
       }

cpeArgArity :: CorePrepEnv -> FloatDecision -> CoreArg -> Arity
-- ^ See Note [Eta expansion of arguments in CorePrep]
-- Returning 0 means "no eta-expansion"; see cpeEtaExpand
cpeArgArity env float_decision arg
  | FloatNone <- float_decision
  = 0    -- Crucial short-cut
         -- See wrinkle (EA2) in Note [Eta expansion of arguments in CorePrep]

  | Just ao <- cp_arityOpts (cpe_config env) -- Just <=> -O1 or -O2
  , not (has_join_in_tail_context arg)
            -- See Wrinkle (EA1) of Note [Eta expansion of arguments in CorePrep]
  = case exprEtaExpandArity ao arg of
      Nothing -> 0
      Just at -> arityTypeArity at

  | otherwise
  = exprArity arg -- this is cheap enough for -O0

has_join_in_tail_context :: CoreExpr -> Bool
-- ^ Identify the cases where we'd generate invalid `CpeApp`s as described in
-- Wrinkle (EA1) of Note [Eta expansion of arguments in CorePrep]
has_join_in_tail_context (Let bs e)            = isJoinBind bs || has_join_in_tail_context e
has_join_in_tail_context (Lam b e) | isTyVar b = has_join_in_tail_context e
has_join_in_tail_context (Cast e _)            = has_join_in_tail_context e
has_join_in_tail_context (Tick _ e)            = has_join_in_tail_context e
has_join_in_tail_context (Case _ _ _ alts)     = any has_join_in_tail_context (rhssOfAlts alts)
has_join_in_tail_context _                     = False

maybeSaturate :: Id -> CpeApp -> Int -> [CoreTickish] -> UniqSM CpeRhs
maybeSaturate fn expr n_args unsat_ticks
  | hasNoBinding fn        -- There's no binding
    -- See Note [Eta expansion of hasNoBinding things in CorePrep]
  = return $ wrapLamBody (\body -> foldr mkTick body unsat_ticks) sat_expr

  | mark_arity > 0 -- A call-by-value function. See Note [CBV Function Ids]
  , not applied_marks
  = assertPpr
      ( not (isJoinId fn)) -- See Note [Do not eta-expand join points]
      ( ppr fn $$ text "expr:" <+> ppr expr $$ text "n_args:" <+> ppr n_args $$
          text "marks:" <+> ppr (idCbvMarks_maybe fn) $$
          text "join_arity" <+> ppr (idJoinPointHood fn) $$
          text "fn_arity" <+> ppr fn_arity
       ) $
    -- pprTrace "maybeSat"
    --   ( ppr fn $$ text "expr:" <+> ppr expr $$ text "n_args:" <+> ppr n_args $$
    --       text "marks:" <+> ppr (idCbvMarks_maybe fn) $$
    --       text "join_arity" <+> ppr (isJoinId_maybe fn) $$
    --       text "fn_arity" <+> ppr fn_arity $$
    --       text "excess_arity" <+> ppr excess_arity $$
    --       text "mark_arity" <+> ppr mark_arity
    --    ) $
    return sat_expr

  | otherwise
  = assert (null unsat_ticks) $
    return expr
  where
    mark_arity    = idCbvMarkArity fn
    fn_arity      = idArity fn
    excess_arity  = (max fn_arity mark_arity) - n_args
    sat_expr      = cpeEtaExpand excess_arity expr
    applied_marks = n_args >= (length . dropWhile (not . isMarkedCbv) .
                               reverse . expectJust "maybeSaturate" $ (idCbvMarks_maybe fn))
    -- For join points we never eta-expand (See Note [Do not eta-expand join points])
    -- so we assert all arguments that need to be passed cbv are visible so that the
    -- backend can evalaute them if required..

{- Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~
Eta expand to match the arity claimed by the binder Remember,
CorePrep must not change arity

Eta expansion might not have happened already, because it is done by
the simplifier only when there at least one lambda already.

NB1:we could refrain when the RHS is trivial (which can happen
    for exported things).  This would reduce the amount of code
    generated (a little) and make things a little worse for
    code compiled without -O.  The case in point is data constructor
    wrappers.

NB2: we have to be careful that the result of etaExpand doesn't
   invalidate any of the assumptions that CorePrep is attempting
   to establish.  One possible cause is eta expanding inside of
   an SCC note - we're now careful in etaExpand to make sure the
   SCC is pushed inside any new lambdas that are generated.

Note [Eta expansion of hasNoBinding things in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maybeSaturate deals with eta expanding to saturate things that can't deal
with unsaturated applications (identified by 'hasNoBinding', currently
foreign calls, unboxed tuple/sum constructors, and representation-polymorphic
primitives such as 'coerce' and 'unsafeCoerce#').

Historical Note: Note that eta expansion in CorePrep used to be very fragile
due to the "prediction" of CAFfyness that we used to make during tidying.  We
previously saturated primop applications here as well but due to this
fragility (see #16846) we now deal with this another way, as described in
Note [Primop wrappers] in GHC.Builtin.PrimOps.

Note [Eta expansion and the CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It turns out to be much much easier to do eta expansion
*after* the main CorePrep stuff.  But that places constraints
on the eta expander: given a CpeRhs, it must return a CpeRhs.

For example here is what we do not want:
                f = /\a -> g (h 3)      -- h has arity 2
After ANFing we get
                f = /\a -> let s = h 3 in g s
and now we do NOT want eta expansion to give
                f = /\a -> \ y -> (let s = h 3 in g s) y

Instead GHC.Core.Opt.Arity.etaExpand gives
                f = /\a -> \y -> let s = h 3 in g s y

Note [Eta expansion of arguments in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose `g = \x y. blah` and consider the expression `f (g x)`; we ANFise to

  let t = g x
  in f t

We really don't want that `t` to be a thunk! That just wastes runtime, updating
a thunk with a PAP etc. The code generator could in principle allocate a PAP,
but in fact it does not know how to do that -- it's easier just to eta-expand:

  let t = \y. g x y
  in f t

To what arity should we eta-expand the argument? `cpeArg` uses two strategies,
governed by the presence of `-fdo-clever-arg-eta-expansion` (implied by -O):

  1. Cheap, with -O0: just use `exprArity`.
  2. More clever but expensive, with -O1 -O2: use `exprEtaExpandArity`,
     same function the Simplifier uses to eta expand RHSs and lambda bodies.

The only reason for using (1) rather than (2) is to keep compile times down.
Using (2) in -O0 bumped up compiler allocations by 2-3% in tests T4801 and
T5321*. However, Plan (2) catches cases that (1) misses.
For example (#23083, assuming -fno-pedantic-bottoms):

  let t = case z of __DEFAULT -> g x
  in f t

to

  let t = \y -> case z of __DEFAULT -> g x y
  in f t

Note that there is a missed opportunity in eta expanding `t` earlier, in the
Simplifier: It would allow us to inline `g`, potentially enabling further
simplification. But then we could have inlined `g` into the PAP to begin with,
and that is discussed in #23150; hence we needn't worry about that in CorePrep.

There is a nasty Wrinkle:

(EA1) When eta expanding an argument headed by a join point, we might get
      "crap", as Note [Eta expansion for join points] in GHC.Core.Opt.Arity puts
      it.
      Consider

        f (join j x = rhs in ...(j 1)...(j 2)...)

      where the argument has arity 1. We might be tempted to eta expand, to

        f (\y -> (join j x = rhs in ...(j 1)...(j 2)...) y)

      Why hasn't the App to `y` been pushed into the join point? That's exactly
      the crap of Note [Eta expansion for join points], so we have to put up
      with it here.
      In our case, (join j x = rhs in ...(j 1)...(j 2)...) is not a valid
      `CpeApp` (see Note [CorePrep invariants]) and we'd get a crash in the App
      case of `coreToStgExpr`.
      Hence we simply check for the cases where an intervening join point
      binding in the tail context of the argument would lead to the introduction
      of such crap via `has_join_in_tail_context`, in which case we abstain from
      eta expansion.

      This scenario occurs rarely; hence it's OK to generate sub-optimal code.
      The alternative would be to fix Note [Eta expansion for join points], but
      that's quite challenging due to unfoldings of (recursive) join points.

(EA2) In cpeArgArity, if float_decision = FloatNone) the `arg` will look like
           let <binds> in rhs
      where <binds> is non-empty and can't be floated out of a lazy context (see
      `wantFloatLocal`). So we can't eta-expand it anyway, so we can return 0
      forthwith.  Without this short-cut we will call exprEtaExpandArity on the
      `arg`, and <binds> might be enormous. exprEtaExpandArity be very expensive
      on this: it uses arityType, and may look at <binds>.

      On the other hand, if float_decision = FloatAll, there will be no
      let-bindings around 'arg'; they will have floated out.  So
      exprEtaExpandArity is cheap.

      This can make a huge difference on deeply nested expressions like
         f (f (f (f (f  ...))))
      #24471 is a good example, where Prep took 25% of compile time!
-}

cpeEtaExpand :: Arity -> CpeRhs -> CpeRhs
cpeEtaExpand arity expr
  | arity == 0 = expr
  | otherwise  = etaExpand arity expr

{-
************************************************************************
*                                                                      *
                Floats
*                                                                      *
************************************************************************

Note [Pin demand info on floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pin demand info on floated lets, so that we can see the one-shot thunks.

Note [Speculative evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since call-by-value is much cheaper than call-by-need, we case-bind arguments
that are either

  1. Strictly evaluated anyway, according to the DmdSig of the callee, or
  2. ok-for-spec, according to 'exprOkForSpeculation'.
     This includes DFuns `$fEqList a`, for example.
     (Could identify more in the future; see reference to !1866 below.)

While (1) is a no-brainer and always beneficial, (2) is a bit
more subtle, as the careful haddock for 'exprOkForSpeculation'
points out. Still, by case-binding the argument we don't need
to allocate a thunk for it, whose closure must be retained as
long as the callee might evaluate it. And if it is evaluated on
most code paths anyway, we get to turn the unknown eval in the
callee into a known call at the call site.

Very Nasty Wrinkle

We must be very careful not to speculate recursive calls!  Doing so
might well change termination behavior.

That comes up in practice for DFuns, which are considered ok-for-spec,
because they always immediately return a constructor.
See Note [NON-BOTTOM-DICTS invariant] in GHC.Core.

But not so if you speculate the recursive call, as #20836 shows:

  class Foo m => Foo m where
    runFoo :: m a -> m a
  newtype Trans m a = Trans { runTrans :: m a }
  instance Monad m => Foo (Trans m) where
    runFoo = id

(NB: class Foo m => Foo m` looks weird and needs -XUndecidableSuperClasses. The
example in #20836 is more compelling, but boils down to the same thing.)
This program compiles to the following DFun for the `Trans` instance:

  Rec {
  $fFooTrans
    = \ @m $dMonad -> C:Foo ($fFooTrans $dMonad) (\ @a -> id)
  end Rec }

Note that the DFun immediately terminates and produces a dictionary, just
like DFuns ought to, but it calls itself recursively to produce the `Foo m`
dictionary. But alas, if we treat `$fFooTrans` as always-terminating, so
that we can speculate its calls, and hence use call-by-value, we get:

  $fFooTrans
    = \ @m $dMonad -> case ($fFooTrans $dMonad) of sc ->
                      C:Foo sc (\ @a -> id)

and that's an infinite loop!
Note that this bad-ness only happens in `$fFooTrans`'s own RHS. In the
*body* of the letrec, it's absolutely fine to use call-by-value on
`foo ($fFooTrans d)`.

Our solution is this: we track in cpe_rec_ids the set of enclosing
recursively-bound Ids, the RHSs of which we are currently transforming and then
in 'exprOkForSpecEval' (a special entry point to 'exprOkForSpeculation',
basically) we'll say that any binder in this set is not ok-for-spec.

Note if we have a letrec group `Rec { f1 = rhs1; ...; fn = rhsn }`, and we
prep up `rhs1`, we have to include not only `f1`, but all binders of the group
`f1..fn` in this set, otherwise our fix is not robust wrt. mutual recursive
DFuns.

NB: If at some point we decide to have a termination analysis for general
functions (#8655, !1866), we need to take similar precautions for (guarded)
recursive functions:

  repeat x = x : repeat x

Same problem here: As written, repeat evaluates rapidly to WHNF. So `repeat x`
is a cheap call that we are willing to speculate, but *not* in repeat's RHS.
Fortunately, pce_rec_ids already has all the information we need in that case.

The problem is very similar to Note [Eta reduction in recursive RHSs].
Here as well as there it is *unsound* to change the termination properties
of the very function whose termination properties we are exploiting.

It is also similar to Note [Do not strictify a DFun's parameter dictionaries],
where marking recursive DFuns (of undecidable *instances*) strict in dictionary
*parameters* leads to quite the same change in termination as above.

Note [BindInfo and FloatInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `BindInfo` of a `Float` describes whether it will be case-bound or
let-bound:

  * `LetBound`: A let binding `let x = rhs in ...`, can be Rec or NonRec.
  * `CaseBound`: A case binding `case rhs of x -> { __DEFAULT -> .. }`.
                 (So always NonRec.)
                 Some case-bound things (string literals, lifted bindings)
                 can float to top-level (but not all), hence it is similar
                 to, but not the same as `StrictContextFloatable :: FloatInfo`
                 described below.

This info is used in `wrapBinds` to pick the corresponding binding form.

We want to case-bind iff the binding is (non-recursive, and) either

  * ok-for-spec-eval (and perhaps lifted, see Note [Speculative evaluation]), or
  * unlifted, or
  * strictly used

The `FloatInfo` of a `Float` describes how far it can float without
(a) violating Core invariants and (b) changing semantics.

  * Any binding is at least `StrictContextFloatable`, meaning we may float it
    out of a strict context such as `f <>` where `f` is strict.

  * A binding is `LazyContextFloatable` if we may float it out of a lazy context
    such as `let x = <> in Just x`.
    Counterexample: A strict or unlifted binding that isn't ok-for-spec-eval
                    such as `case divInt# x y of r -> { __DEFAULT -> I# r }`.
                    Here, we may not foat out the strict `r = divInt# x y`.

  * A binding is `TopLvlFloatable` if it is `LazyContextFloatable` and also can
    be bound at the top level.
    Counterexample: A strict or unlifted binding (ok-for-spec-eval or not)
                    such as `case x +# y of r -> { __DEFAULT -> I# r }`.

This meaning of "at least" is encoded in `floatsAtLeastAsFarAs`.
Note that today, `LetBound` implies `TopLvlFloatable`, so we could make do with
the the following enum (check `mkNonRecFloat` for whether this is up to date):

   LetBoundTopLvlFloatable          (lifted or boxed values)
  CaseBoundTopLvlFloatable          (strings, ok-for-spec-eval and lifted)
  CaseBoundLazyContextFloatable     (ok-for-spec-eval and unlifted)
  CaseBoundStrictContextFloatable   (not ok-for-spec-eval and unlifted)

Although there is redundancy in the current encoding, SG thinks it is cleaner
conceptually.

See also Note [Floats and FloatDecision] for how we maintain whole groups of
floats and how far they go.

Note [Floats and FloatDecision]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have a special datatype `Floats` for modelling a telescope of `FloatingBind`
and caching its "maximum" `FloatInfo`, according to `floatsAtLeastAsFarAs`
(see Note [BindInfo and FloatInfo] for the ordering).
There are several operations for creating and combining `Floats` that maintain
scoping and the cached `FloatInfo`.

When deciding whether we want to float out a `Floats` out of a binding context
such as `let x = <> in e` (let), `f <>` (app), or `x = <>; ...` (top-level),
we consult the cached `FloatInfo` of the `Floats`:

  * If we want to float to the top-level (`x = <>; ...`), we check whether
    we may float-at-least-as-far-as `TopLvlFloatable`, in which case we
    respond with `FloatAll :: FloatDecision`; otherwise we say `FloatNone`.
  * If we want to float locally (let or app), then the floating decision is
    described in Note [wantFloatLocal].

`executeFloatDecision` is then used to act on the particular `FloatDecision`.
-}

-- See Note [BindInfo and FloatInfo]
data BindInfo
  = CaseBound -- ^ A strict binding
  | LetBound  -- ^ A lazy or value binding
  deriving Eq

-- See Note [BindInfo and FloatInfo]
data FloatInfo
  = TopLvlFloatable
  -- ^ Anything that can be bound at top-level, such as arbitrary lifted
  -- bindings or anything that responds True to `exprIsHNF`, such as literals or
  -- saturated DataCon apps where unlifted or strict args are values.

  | LazyContextFloatable
  -- ^ Anything that can be floated out of a lazy context.
  -- In addition to any 'TopLvlFloatable' things, this includes (unlifted)
  -- bindings that are ok-for-spec that we intend to case-bind.

  | StrictContextFloatable
  -- ^ Anything that can be floated out of a strict evaluation context.
  -- That is possible for all bindings; this is the Top element of 'FloatInfo'.

  deriving Eq

instance Outputable BindInfo where
  ppr CaseBound = text "Case"
  ppr LetBound  = text "Let"

instance Outputable FloatInfo where
  ppr TopLvlFloatable = text "top-lvl"
  ppr LazyContextFloatable = text "lzy-ctx"
  ppr StrictContextFloatable = text "str-ctx"

-- See Note [Floating in CorePrep]
-- and Note [BindInfo and FloatInfo]
data FloatingBind
  = Float !CoreBind !BindInfo !FloatInfo    -- Never a join-point binding
  | UnsafeEqualityCase !CoreExpr !CoreBndr !AltCon ![CoreBndr]
  | FloatTick CoreTickish

-- See Note [Floats and FloatDecision]
data Floats
  = Floats
  { fs_info  :: !FloatInfo
  , fs_binds :: !(OrdList FloatingBind)
  }

instance Outputable FloatingBind where
  ppr (Float b bi fi) = ppr bi <+> ppr fi <+> ppr b
  ppr (FloatTick t) = ppr t
  ppr (UnsafeEqualityCase scrut b k bs) = text "case" <+> ppr scrut
                                <+> text "of"<+> ppr b <> text "@"
                                <> case bs of
                                   [] -> ppr k
                                   _  -> parens (ppr k <+> ppr bs)

instance Outputable Floats where
  ppr (Floats info binds) = text "Floats" <> brackets (ppr info) <> braces (ppr binds)

lubFloatInfo :: FloatInfo -> FloatInfo -> FloatInfo
lubFloatInfo StrictContextFloatable _                      = StrictContextFloatable
lubFloatInfo _                      StrictContextFloatable = StrictContextFloatable
lubFloatInfo LazyContextFloatable   _                      = LazyContextFloatable
lubFloatInfo _                      LazyContextFloatable   = LazyContextFloatable
lubFloatInfo TopLvlFloatable        TopLvlFloatable        = TopLvlFloatable

floatsAtLeastAsFarAs :: FloatInfo -> FloatInfo -> Bool
-- See Note [Floats and FloatDecision]
floatsAtLeastAsFarAs l r = l `lubFloatInfo` r == r

emptyFloats :: Floats
emptyFloats = Floats TopLvlFloatable nilOL

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats _ b) = isNilOL b

getFloats :: Floats -> OrdList FloatingBind
getFloats = fs_binds

unitFloat :: FloatingBind -> Floats
unitFloat = snocFloat emptyFloats

floatInfo :: FloatingBind -> FloatInfo
floatInfo (Float _ _ info)     = info
floatInfo UnsafeEqualityCase{} = LazyContextFloatable -- See Note [Floating in CorePrep]
floatInfo FloatTick{}          = TopLvlFloatable      -- We filter these out in cpePair,
                                                      -- see Note [Floating Ticks in CorePrep]

-- | Append a `FloatingBind` `b` to a `Floats` telescope `bs` that may reference any
-- binding of the 'Floats'.
snocFloat :: Floats -> FloatingBind -> Floats
snocFloat floats fb =
  Floats { fs_info  = lubFloatInfo (fs_info floats) (floatInfo fb)
         , fs_binds = fs_binds floats `snocOL` fb }

-- | Cons a `FloatingBind` `b` to a `Floats` telescope `bs` which scopes over
-- `b`.
consFloat :: FloatingBind -> Floats -> Floats
consFloat fb floats =
  Floats { fs_info  = lubFloatInfo (fs_info floats) (floatInfo fb)
         , fs_binds = fb `consOL`  fs_binds floats }

-- | Append two telescopes, nesting the right inside the left.
appFloats :: Floats -> Floats -> Floats
appFloats outer inner =
  Floats { fs_info  = lubFloatInfo (fs_info outer) (fs_info inner)
         , fs_binds = fs_binds outer `appOL` fs_binds inner }

-- | Zip up two `Floats`, none of which scope over the other
zipFloats :: Floats -> Floats -> Floats
-- We may certainly just nest one telescope in the other, so appFloats is a
-- valid implementation strategy.
zipFloats = appFloats

-- | `zipFloats` a bunch of independent telescopes.
zipManyFloats :: [Floats] -> Floats
zipManyFloats = foldr zipFloats emptyFloats

mkNonRecFloat :: CorePrepEnv -> Demand -> Bool -> Id -> CpeRhs -> FloatingBind
mkNonRecFloat env dmd is_unlifted bndr rhs
  = -- pprTrace "mkNonRecFloat" (ppr bndr <+> ppr (bound,info)
    --                             <+> ppr is_lifted <+> ppr is_strict
    --                             <+> ppr ok_for_spec
    --                           $$ ppr rhs) $
    Float (NonRec bndr' rhs) bound info
  where
    bndr' = setIdDemandInfo bndr dmd -- See Note [Pin demand info on floats]
    (bound,info)
      | is_lifted, is_hnf        = (LetBound, TopLvlFloatable)
          -- is_lifted: We currently don't allow unlifted values at the
          --            top-level or inside letrecs
          --            (but SG thinks that in principle, we should)
      | is_data_con bndr         = (LetBound, TopLvlFloatable)
          -- We need this special case for unlifted DataCon workers/wrappers
          -- until #17521 is fixed
      | exprIsTickedString rhs   = (CaseBound, TopLvlFloatable)
          -- String literals are unboxed (so must be case-bound) and float to
          -- the top-level
      | is_unlifted, ok_for_spec = (CaseBound, LazyContextFloatable)
      | is_lifted,   ok_for_spec = (CaseBound, TopLvlFloatable)
          -- See Note [Speculative evaluation]
          -- Ok-for-spec-eval things will be case-bound, lifted or not.
          -- But when it's lifted we are ok with floating it to top-level
          -- (where it is actually bound lazily).
      | is_unlifted || is_strict = (CaseBound, StrictContextFloatable)
          -- These will never be floated out of a lazy RHS context
      | otherwise                = assertPpr is_lifted (ppr rhs) $
                                   (LetBound, TopLvlFloatable)
          -- And these float freely but can't be speculated, hence LetBound

    is_lifted   = not is_unlifted
    is_hnf      = exprIsHNF rhs
    is_strict   = isStrUsedDmd dmd
    ok_for_spec = exprOkForSpecEval (not . is_rec_call) rhs
    is_rec_call = (`elemUnVarSet` cpe_rec_ids env)
    is_data_con = isJust . isDataConId_maybe

-- | Wrap floats around an expression
wrapBinds :: Floats -> CpeBody -> CpeBody
wrapBinds floats body
  = -- pprTraceWith "wrapBinds" (\res -> ppr floats $$ ppr body $$ ppr res) $
    foldrOL mk_bind body (getFloats floats)
  where
    -- See Note [BindInfo and FloatInfo] on whether we pick Case or Let here
    mk_bind f@(Float bind CaseBound _) body
      | NonRec bndr rhs <- bind
      = mkDefaultCase rhs bndr body
      | otherwise
      = pprPanic "wrapBinds" (ppr f)
    mk_bind (Float bind _ _) body
      = Let bind body
    mk_bind (UnsafeEqualityCase scrut b con bs) body
      = mkSingleAltCase scrut b con bs body
    mk_bind (FloatTick tickish) body
      = mkTick tickish body

-- | Put floats at top-level
deFloatTop :: Floats -> [CoreBind]
-- Precondition: No Strict or LazyContextFloatable 'FloatInfo', no ticks!
deFloatTop floats
  = foldrOL get [] (getFloats floats)
  where
    get (Float b _ TopLvlFloatable) bs
      = get_bind b : bs
    get b _  = pprPanic "corePrepPgm" (ppr b)

    -- See Note [Dead code in CorePrep]
    get_bind (NonRec x e) = NonRec x (occurAnalyseExpr e)
    get_bind (Rec xes)    = Rec [(x, occurAnalyseExpr e) | (x, e) <- xes]

---------------------------------------------------------------------------

{- Note [wantFloatLocal]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let x = let y = e1 in e2
  in e
Similarly for `(\x. e) (let y = e1 in e2)`.
Do we want to float out `y` out of `x`?
(This is discussed in detail in the paper
"Let-floating: moving bindings to give faster programs".)

`wantFloatLocal` is concerned with answering this question.
It considers the Demand on `x`, whether or not `e2` is unlifted and the
`FloatInfo` of the `y` binding (e.g., it might itself be unlifted, a value,
strict, or ok-for-spec).

We float out if ...
  1. ... the binding context is strict anyway, so either `x` is used strictly
     or has unlifted type.
     Doing so is trivially sound and won`t increase allocations, so we
     return `FloatAll`.
     This might happen while ANF-ising `f (g (h 13))` where `f`,`g` are strict:
       f (g (h 13))
       ==> { ANF }
       case (case h 13 of r -> g r) of r2 -> f r2
       ==> { Float }
       case h 13 of r -> case g r of r2 -> f r2
     The latter is easier to read and grows less stack.
  2. ... `e2` becomes a value in doing so, in which case we won't need to
     allocate a thunk for `x`/the arg that closes over the FVs of `e1`.
     In general, this is only sound if `y=e1` is `LazyContextFloatable`.
     (See Note [BindInfo and FloatInfo].)
     Nothing is won if `x` doesn't become a value
     (i.e., `let x = let sat = f 14 in g sat in e`),
     so we return `FloatNone` if there is any float that is
     `StrictContextFloatable`, and return `FloatAll` otherwise.

To elaborate on (2), consider the case when the floated binding is
`e1 = divInt# a b`, e.g., not `LazyContextFloatable`:
  let x = I# (a `divInt#` b)
  in e
this ANFises to
  let x = case a `divInt#` b of r { __DEFAULT -> I# r }
  in e
If `x` is used lazily, we may not float `r` further out.
A float binding `x +# y` is OK, though, and so every ok-for-spec-eval
binding is `LazyContextFloatable`.

Wrinkles:

 (W1) When the outer binding is a letrec, i.e.,
        letrec x = case a +# b of r { __DEFAULT -> f y r }
               y = [x]
        in e
      we don't want to float `LazyContextFloatable` bindings such as `r` either
      and require `TopLvlFloatable` instead.
      The reason is that we don't track FV of FloatBindings, so we would need
      to park them in the letrec,
        letrec r = a +# b -- NB: r`s RHS might scope over x and y
               x = f y r
               y = [x]
        in e
      and now we have violated Note [Core letrec invariant].
      So we preempt this case in `wantFloatLocal`, responding `FloatNone` unless
      all floats are `TopLvlFloatable`.
-}

data FloatDecision
  = FloatNone
  | FloatAll

executeFloatDecision :: FloatDecision -> Floats -> CpeRhs -> UniqSM (Floats, CpeRhs)
executeFloatDecision dec floats rhs
  = case dec of
      FloatAll                 -> return (floats, rhs)
      FloatNone
        | isEmptyFloats floats -> return (emptyFloats, rhs)
        | otherwise            -> do { (floats', body) <- rhsToBody rhs
                                     ; return (emptyFloats, wrapBinds floats $
                                                            wrapBinds floats' body) }
            -- FloatNone case: `rhs` might have lambdas, and we can't
            -- put them inside a wrapBinds, which expects a `CpeBody`.

wantFloatTop :: Floats -> FloatDecision
wantFloatTop fs
  | fs_info fs `floatsAtLeastAsFarAs` TopLvlFloatable = FloatAll
  | otherwise                                         = FloatNone

wantFloatLocal :: RecFlag -> Demand -> Bool -> Floats -> CpeRhs -> FloatDecision
-- See Note [wantFloatLocal]
wantFloatLocal is_rec rhs_dmd rhs_is_unlifted floats rhs
  |  isEmptyFloats floats -- Well yeah...
  || isStrUsedDmd rhs_dmd -- Case (1) of Note [wantFloatLocal]
  || rhs_is_unlifted      -- dito
  || (fs_info floats `floatsAtLeastAsFarAs` max_float_info && exprIsHNF rhs)
                          -- Case (2) of Note [wantFloatLocal]
  = FloatAll

  | otherwise
  = FloatNone
  where
    max_float_info | isRec is_rec = TopLvlFloatable
                   | otherwise    = LazyContextFloatable
                    -- See Note [wantFloatLocal], Wrinkle (W1)
                    -- for 'is_rec'

{-
************************************************************************
*                                                                      *
                Cloning
*                                                                      *
************************************************************************
-}

-- ---------------------------------------------------------------------------
--                      The environment
-- ---------------------------------------------------------------------------

{- Note [Inlining in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a subtle but important invariant that must be upheld in the output
of CorePrep: there are no "trivial" updatable thunks.  Thus, this Core
is impermissible:

     let x :: ()
         x = y

(where y is a reference to a GLOBAL variable).  Thunks like this are silly:
they can always be profitably replaced by inlining x with y. Consequently,
the code generator/runtime does not bother implementing this properly
(specifically, there is no implementation of stg_ap_0_upd_info, which is the
stack frame that would be used to update this thunk.  The "0" means it has
zero free variables.)

In general, the inliner is good at eliminating these let-bindings.  However,
there is one case where these trivial updatable thunks can arise: when
we are optimizing away 'lazy' (see Note [lazyId magic], and also
'cpeRhsE'.)  Then, we could have started with:

     let x :: ()
         x = lazy @ () y

which is a perfectly fine, non-trivial thunk, but then CorePrep will
drop 'lazy', giving us 'x = y' which is trivial and impermissible.
The solution is CorePrep to have a miniature inlining pass which deals
with cases like this.  We can then drop the let-binding altogether.

Why does the removal of 'lazy' have to occur in CorePrep?
The gory details are in Note [lazyId magic] in GHC.Types.Id.Make, but the
main reason is that lazy must appear in unfoldings (optimizer
output) and it must prevent call-by-value for catch# (which
is implemented by CorePrep.)

An alternate strategy for solving this problem is to have the
inliner treat 'lazy e' as a trivial expression if 'e' is trivial.
We decided not to adopt this solution to keep the definition
of 'exprIsTrivial' simple.

There is ONE caveat however: for top-level bindings we have
to preserve the binding so that we float the (hacky) non-recursive
binding for data constructors; see Note [Data constructor workers].

Note [CorePrep inlines trivial CoreExpr not Id]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO
Why does cpe_env need to be an IdEnv CoreExpr, as opposed to an
IdEnv Id?  Naively, we might conjecture that trivial updatable thunks
as per Note [Inlining in CorePrep] always have the form
'lazy @ SomeType gbl_id'.  But this is not true: the following is
perfectly reasonable Core:

     let x :: ()
         x = lazy @ (forall a. a) y @ Bool

When we inline 'x' after eliminating 'lazy', we need to replace
occurrences of 'x' with 'y @ bool', not just 'y'.  Situations like
this can easily arise with higher-rank types; thus, cpe_env must
map to CoreExprs, not Ids.

-}

data CorePrepConfig = CorePrepConfig
  { cp_catchNonexhaustiveCases :: !Bool
  -- ^ Whether to generate a default alternative with ``error`` in these
  -- cases. This is helpful when debugging demand analysis or type
  -- checker bugs which can sometimes manifest as segmentation faults.

  , cp_convertNumLit           :: !(LitNumType -> Integer -> Maybe CoreExpr)
  -- ^ Convert some numeric literals (Integer, Natural) into their final
  -- Core form.

  , cp_arityOpts               :: !(Maybe ArityOpts)
  -- ^ Configuration for arity analysis ('exprEtaExpandArity').
  -- See Note [Eta expansion of arguments in CorePrep]
  -- When 'Nothing' (e.g., -O0, -O1), use the cheaper 'exprArity' instead
  }

data CorePrepEnv
  = CPE { cpe_config          :: !CorePrepConfig
        -- ^ This flag is intended to aid in debugging strictness
        -- analysis bugs. These are particularly nasty to chase down as
        -- they may manifest as segmentation faults. When this flag is
        -- enabled we instead produce an 'error' expression to catch
        -- the case where a function we think should bottom
        -- unexpectedly returns.

        , cpe_subst :: Subst
        -- ^ The IdEnv part of the substitution is used for three operations:
        --
        --      1. To support cloning of local Ids so that they are
        --      all unique (see Note [Cloning in CorePrep])
        --
        --      2. To support beta-reduction of runRW, see
        --      Note [runRW magic] and Note [runRW arg].
        --
        --      3. To let us inline trivial RHSs of non top-level let-bindings,
        --      see Note [lazyId magic], Note [Inlining in CorePrep]
        --      and Note [CorePrep inlines trivial CoreExpr not Id] (#12076)
        --
        -- The TyCoVar part of the substitution is used only for
        --     Note [Cloning CoVars and TyVars]

        , cpe_rec_ids         :: UnVarSet -- Faster OutIdSet; See Note [Speculative evaluation]
    }

mkInitialCorePrepEnv :: CorePrepConfig -> CorePrepEnv
mkInitialCorePrepEnv cfg = CPE
      { cpe_config        = cfg
      , cpe_subst         = emptySubst
      , cpe_rec_ids       = emptyUnVarSet
      }

extendCorePrepEnv :: CorePrepEnv -> Id -> Id -> CorePrepEnv
extendCorePrepEnv cpe@(CPE { cpe_subst = subst }) id id'
    = cpe { cpe_subst = subst2 }
    where
      subst1 = extendSubstInScope subst id'
      subst2 = extendIdSubst subst1 id (Var id')

extendCorePrepEnvList :: CorePrepEnv -> [(Id,Id)] -> CorePrepEnv
extendCorePrepEnvList cpe@(CPE { cpe_subst = subst }) prs
    = cpe { cpe_subst = subst2 }
    where
      subst1 = extendSubstInScopeList subst (map snd prs)
      subst2 = extendIdSubstList subst1 [(id, Var id') | (id,id') <- prs]

extendCorePrepEnvExpr :: CorePrepEnv -> Id -> CoreExpr -> CorePrepEnv
extendCorePrepEnvExpr cpe id expr
    = cpe { cpe_subst = extendIdSubst (cpe_subst cpe) id expr }

lookupCorePrepEnv :: CorePrepEnv -> Id -> CoreExpr
lookupCorePrepEnv cpe id
  = case lookupIdSubst_maybe (cpe_subst cpe) id of
       Just e -> e
       Nothing -> Var id
    -- Do not use GHC.Core.Subs.lookupIdSubst because that is a no-op on GblIds;
    -- and Tidy has made top-level externally-visible Ids into GblIds

enterRecGroupRHSs :: CorePrepEnv -> [OutId] -> CorePrepEnv
enterRecGroupRHSs env grp
  = env { cpe_rec_ids = extendUnVarSetList grp (cpe_rec_ids env) }

cpSubstTy :: CorePrepEnv -> Type -> Type
cpSubstTy (CPE { cpe_subst = subst }) ty = substTy subst ty
          -- substTy has a short-cut if the TCvSubst is empty

cpSubstCo :: CorePrepEnv -> Coercion -> Coercion
cpSubstCo (CPE { cpe_subst = subst }) co = substCo subst co
          -- substCo has a short-cut if the TCvSubst is empty

------------------------------------------------------------------------------
-- Cloning binders
-- ---------------------------------------------------------------------------

cpCloneBndrs :: CorePrepEnv -> [InVar] -> UniqSM (CorePrepEnv, [OutVar])
cpCloneBndrs env bs = mapAccumLM cpCloneBndr env bs

cpCloneCoVarBndr :: CorePrepEnv -> InVar -> UniqSM (CorePrepEnv, OutVar)
-- Clone the CoVar
-- See Note [Cloning CoVars and TyVars]
cpCloneCoVarBndr env@(CPE { cpe_subst = subst }) covar
  = assertPpr (isCoVar covar) (ppr covar) $
    do { uniq <- getUniqueM
       ; let covar1 = setVarUnique covar uniq
             covar2 = updateVarType (substTy subst) covar1
             subst1 = extendTCvSubstWithClone subst covar covar2
       ; return (env { cpe_subst = subst1 }, covar2) }

cpCloneBndr  :: CorePrepEnv -> InVar -> UniqSM (CorePrepEnv, OutVar)
-- See Note [Cloning in CorePrep]
cpCloneBndr env@(CPE { cpe_subst = subst }) bndr
  | isTyCoVar bndr  -- See Note [Cloning CoVars and TyVars]
  = if isEmptyTCvSubst subst    -- The common case
    then return (env { cpe_subst = extendSubstInScope subst bndr }, bndr)
    else -- No need to clone the Unique; but we must apply the substitution
         let bndr1  = updateVarType (substTy subst) bndr
             subst1 = extendTCvSubstWithClone subst bndr bndr1
         in return (env { cpe_subst = subst1 }, bndr1)

  | otherwise  -- A non-CoVar Id
  = do { bndr1 <- clone_it bndr
       ; let bndr2 = updateIdTypeAndMult (substTy subst) bndr1

       -- Drop (now-useless) rules/unfoldings
       -- See Note [Drop unfoldings and rules]
       -- and Note [Preserve evaluatedness] in GHC.Core.Tidy
       -- And force it.. otherwise the old unfolding is just retained.
       -- See #22071
       ; let !unfolding' = trimUnfolding (realIdUnfolding bndr)
                          -- Simplifier will set the Id's unfolding

             bndr3 = bndr2 `setIdUnfolding`      unfolding'
                           `setIdSpecialisation` emptyRuleInfo

       ; return (extendCorePrepEnv env bndr bndr3, bndr3) }
  where
    clone_it bndr
      | isLocalId bndr
      = do { uniq <- getUniqueM
           ; return (setVarUnique bndr uniq) }

      | otherwise   -- Top level things, which we don't want
                    -- to clone, have become GlobalIds by now
      = return bndr

{- Note [Drop unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to drop the unfolding/rules on every Id:

  - We are now past interface-file generation, and in the
    codegen pipeline, so we really don't need full unfoldings/rules

  - The unfolding/rule may be keeping stuff alive that we'd like
    to discard.  See  Note [Dead code in CorePrep]

  - Getting rid of unnecessary unfoldings reduces heap usage

  - We are changing uniques, so if we didn't discard unfoldings/rules
    we'd have to substitute in them

HOWEVER, we want to preserve evaluated-ness;
see Note [Preserve evaluatedness] in GHC.Core.Tidy.
-}

------------------------------------------------------------------------------
-- Cloning ccall Ids; each must have a unique name,
-- to give the code generator a handle to hang it on
-- ---------------------------------------------------------------------------

fiddleCCall :: Id -> UniqSM Id
fiddleCCall id
  | isFCallId id = (id `setVarUnique`) <$> getUniqueM
  | otherwise    = return id

------------------------------------------------------------------------------
-- Generating new binders
-- ---------------------------------------------------------------------------

newVar :: Type -> UniqSM Id
newVar ty
 = seqType ty `seq` mkSysLocalOrCoVarM (fsLit "sat") ManyTy ty


------------------------------------------------------------------------------
-- Floating ticks
-- ---------------------------------------------------------------------------
--
-- Note [Floating Ticks in CorePrep]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It might seem counter-intuitive to float ticks by default, given
-- that we don't actually want to move them if we can help it. On the
-- other hand, nothing gets very far in CorePrep anyway, and we want
-- to preserve the order of let bindings and tick annotations in
-- relation to each other. For example, if we just wrapped let floats
-- when they pass through ticks, we might end up performing the
-- following transformation:
--
--   src<...> let foo = bar in baz
--   ==>  let foo = src<...> bar in src<...> baz
--
-- Because the let-binding would float through the tick, and then
-- immediately materialize, achieving nothing but decreasing tick
-- accuracy. The only special case is the following scenario:
--
--   let foo = src<...> (let a = b in bar) in baz
--   ==>  let foo = src<...> bar; a = src<...> b in baz
--
-- Here we would not want the source tick to end up covering "baz" and
-- therefore refrain from pushing ticks outside. Instead, we copy them
-- into the floating binds (here "a") in cpePair. Note that where "b"
-- or "bar" are (value) lambdas we have to push the annotations
-- further inside in order to uphold our rules.
--
-- All of this is implemented below in @wrapTicks@.

-- | Like wrapFloats, but only wraps tick floats
wrapTicks :: Floats -> CoreExpr -> (Floats, CoreExpr)
wrapTicks floats expr
  | (floats1, ticks1) <- fold_fun go floats
  = (floats1, foldrOL mkTick expr ticks1)
  where fold_fun f floats =
           let (binds, ticks) = foldlOL f (nilOL,nilOL) (fs_binds floats)
           in (floats { fs_binds = binds }, ticks)
        -- Deeply nested constructors will produce long lists of
        -- redundant source note floats here. We need to eliminate
        -- those early, as relying on mkTick to spot it after the fact
        -- can yield O(n^3) complexity [#11095]
        go (flt_binds, ticks) (FloatTick t)
          = assert (tickishPlace t == PlaceNonLam)
            (flt_binds, if any (flip tickishContains t) ticks
                        then ticks else ticks `snocOL` t)
        go (flt_binds, ticks) f@UnsafeEqualityCase{}
          -- unsafe equality case will be erased; don't wrap anything!
          = (flt_binds `snocOL` f, ticks)
        go (flt_binds, ticks) f@Float{}
          = (flt_binds `snocOL` foldrOL wrap f ticks, ticks)

        wrap t (Float bind bound info) = Float (wrapBind t bind) bound info
        wrap _ f                 = pprPanic "Unexpected FloatingBind" (ppr f)
        wrapBind t (NonRec binder rhs) = NonRec binder (mkTick t rhs)
        wrapBind t (Rec pairs)         = Rec (mapSnd (mkTick t) pairs)

------------------------------------------------------------------------------
-- Numeric literals
-- ---------------------------------------------------------------------------

-- | Create a function that converts Bignum literals into their final CoreExpr
mkConvertNumLiteral
   :: Platform
   -> HomeUnit
   -> (Name -> IO TyThing)
   -> IO (LitNumType -> Integer -> Maybe CoreExpr)
mkConvertNumLiteral platform home_unit lookup_global = do
   let
      guardBignum act
         | isHomeUnitInstanceOf home_unit primUnitId
         = return $ panic "Bignum literals are not supported in ghc-prim"
         | isHomeUnitInstanceOf home_unit bignumUnitId
         = return $ panic "Bignum literals are not supported in ghc-bignum"
         | otherwise = act

      lookupBignumId n      = guardBignum (tyThingId <$> lookup_global n)

   -- The lookup is done here but the failure (panic) is reported lazily when we
   -- try to access the `bigNatFromWordList` function.
   --
   -- If we ever get built-in ByteArray# literals, we could avoid the lookup by
   -- directly using the Integer/Natural wired-in constructors for big numbers.

   bignatFromWordListId <- lookupBignumId bignatFromWordListName

   let
      convertNumLit nt i = case nt of
         LitNumBigNat  -> Just (convertBignatPrim i)
         _             -> Nothing

      convertBignatPrim i =
         let
            -- ByteArray# literals aren't supported (yet). Were they supported,
            -- we would use them directly. We would need to handle
            -- wordSize/endianness conversion between host and target
            -- wordSize  = platformWordSize platform
            -- byteOrder = platformByteOrder platform

            -- For now we build a list of Words and we produce
            -- `bigNatFromWordList# list_of_words`

            words = mkListExpr wordTy (reverse (unfoldr f i))
               where
                  f 0 = Nothing
                  f x = let low  = x .&. mask
                            high = x `shiftR` bits
                        in Just (mkConApp wordDataCon [Lit (mkLitWord platform low)], high)
                  bits = platformWordSizeInBits platform
                  mask = 2 ^ bits - 1

         in mkApps (Var bignatFromWordListId) [words]


   return convertNumLit
