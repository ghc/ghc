{-
(c) The University of Glasgow, 1994-2006


Core pass to saturate constructors and PrimOps
-}

{-# LANGUAGE BangPatterns, CPP, MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.CoreToStg.Prep (
      corePrepPgm, corePrepExpr, cvtLitInteger, cvtLitNatural,
      lookupMkIntegerName, lookupIntegerSDataConName,
      lookupMkNaturalName, lookupNaturalSDataConName
  ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core.Op.OccurAnal

import GHC.Driver.Types
import PrelNames
import MkId             ( realWorldPrimId, mkPrimOpId )
import PrimOp           ( PrimOp(TouchOp) )
import GHC.Core.Utils
import GHC.Core.Arity
import GHC.Core.FVs
import GHC.Core.Op.Monad        ( CoreToDo(..) )
import GHC.Core.Lint    ( endPassIO )
import GHC.Core
import GHC.Core.Make hiding( FloatBind(..) )   -- We use our own FloatBind here
import GHC.Core.Type
import Literal
import GHC.Core.Coercion
import TcEnv
import GHC.Core.TyCon
import Demand
import Var
import VarSet
import VarEnv
import Id
import IdInfo
import TysWiredIn
import TysPrim          ( realWorldStatePrimTy, primRepToRuntimeRep )
import GHC.Core.DataCon
import BasicTypes
import Module
import UniqSupply
import Maybes
import OrdList
import ErrUtils
import GHC.Driver.Session
import GHC.Driver.Ways
import Util
import Outputable
import FastString
import Name             ( NamedThing(..), nameSrcSpan, isInternalName )
import SrcLoc           ( SrcSpan(..), realSrcLocSpan, mkRealSrcLoc )
import Data.Bits
import MonadUtils       ( mapAccumLM )
import Control.Monad
import CostCentre       ( CostCentre, ccFromThisModule )
import qualified Data.Set as S

{-
-- ---------------------------------------------------------------------------
-- Note [CorePrep Overview]
-- ---------------------------------------------------------------------------

The goal of this pass is to prepare for code generation.

1.  Saturate constructor applications.

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

5.  [Not any more; nuked Jun 2002] Do the seq/par munging.

6.  Clone all local Ids.
    This means that all such Ids are unique, rather than the
    weaker guarantee of no clashes which the simplifier provides.
    And that is what the code generator needs.

    We don't clone TyVars or CoVars. The code gen doesn't need that,
    and doing so would be tiresome because then we'd need
    to substitute in types and coercions.

7.  Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

8.  Inject bindings for the "implicit" Ids:
        * Constructor wrappers
        * Constructor workers
    We want curried definitions for all of these in case they
    aren't inlined by some caller.

9.  Replace (lazy e) by e.  See Note [lazyId magic] in MkId.hs
    Also replace (noinline e) by e.

10. Convert (LitInteger i t) into the core representation
    for the Integer i. Normally this uses mkInteger, but if
    we are using the integer-gmp implementation then there is a
    special case where we use the S# constructor for Integers that
    are in the range of Int.

11. Same for LitNatural.

12. Uphold tick consistency while doing this: We move ticks out of
    (non-type) applications where we can, and make sure that we
    annotate according to scoping rules when floating.

13. Collect cost centres (including cost centres in unfoldings) if we're in
    profiling mode. We have to do this here beucase we won't have unfoldings
    after this pass (see `zapUnfolding` and Note [Drop unfoldings and rules].

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
              | case body of pat -> body
              | /\a. body | /\c. body
              | body |> co

    Right hand sides (only place where value lambdas can occur)
       rhs ::= /\a.rhs  |  \x.rhs  |  body

We define a synonym for each of these non-terminals.  Functions
with the corresponding name produce a result in that syntax.
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

corePrepPgm :: HscEnv -> Module -> ModLocation -> CoreProgram -> [TyCon]
            -> IO (CoreProgram, S.Set CostCentre)
corePrepPgm hsc_env this_mod mod_loc binds data_tycons =
    withTiming dflags
               (text "CorePrep"<+>brackets (ppr this_mod))
               (const ()) $ do
    us <- mkSplitUniqSupply 's'
    initialCorePrepEnv <- mkInitialCorePrepEnv dflags hsc_env

    let cost_centres
          | WayProf `S.member` ways dflags
          = collectCostCentres this_mod binds
          | otherwise
          = S.empty

        implicit_binds = mkDataConWorkers dflags mod_loc data_tycons
            -- NB: we must feed mkImplicitBinds through corePrep too
            -- so that they are suitably cloned and eta-expanded

        binds_out = initUs_ us $ do
                      floats1 <- corePrepTopBinds initialCorePrepEnv binds
                      floats2 <- corePrepTopBinds initialCorePrepEnv implicit_binds
                      return (deFloatTop (floats1 `appendFloats` floats2))

    endPassIO hsc_env alwaysQualify CorePrep binds_out []
    return (binds_out, cost_centres)
  where
    dflags = hsc_dflags hsc_env

corePrepExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
corePrepExpr dflags hsc_env expr =
    withTiming dflags (text "CorePrep [expr]") (const ()) $ do
    us <- mkSplitUniqSupply 's'
    initialCorePrepEnv <- mkInitialCorePrepEnv dflags hsc_env
    let new_expr = initUs_ us (cpeBodyNF initialCorePrepEnv expr)
    dumpIfSet_dyn dflags Opt_D_dump_prep "CorePrep" FormatCore (ppr new_expr)
    return new_expr

corePrepTopBinds :: CorePrepEnv -> [CoreBind] -> UniqSM Floats
-- Note [Floating out of top level bindings]
corePrepTopBinds initialCorePrepEnv binds
  = go initialCorePrepEnv binds
  where
    go _   []             = return emptyFloats
    go env (bind : binds) = do (env', floats, maybe_new_bind)
                                 <- cpeBind TopLevel env bind
                               MASSERT(isNothing maybe_new_bind)
                                 -- Only join points get returned this way by
                                 -- cpeBind, and no join point may float to top
                               floatss <- go env' binds
                               return (floats `appendFloats` floatss)

mkDataConWorkers :: DynFlags -> ModLocation -> [TyCon] -> [CoreBind]
-- See Note [Data constructor workers]
-- c.f. Note [Injecting implicit bindings] in GHC.Iface.Tidy
mkDataConWorkers dflags mod_loc data_tycons
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
     | debugLevel dflags == 0                = id
     | RealSrcSpan span _ <- nameSrcSpan name = tick span
     | Just file <- ml_hs_file mod_loc       = tick (span1 file)
     | otherwise                             = tick (span1 "???")
     where tick span  = Tick (SourceNote span $ showSDoc dflags (ppr name))
           span1 file = realSrcLocSpan $ mkRealSrcLoc (mkFastString file) 1 1

{-
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
   RHS to drop the dead local bindings. For that call to OccAnal, we
   disable the binder swap, else the occurrence analyser sometimes
   introduces new let bindings for cased binders, which lead to the bug
   in #5433.

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
       ; let triv_rhs = cpExprIsTrivial rhs1
             env2    | triv_rhs  = extendCorePrepEnvExpr env1 bndr rhs1
                     | otherwise = env1
             floats1 | triv_rhs, isInternalName (idName bndr)
                     = floats
                     | otherwise
                     = addFloat floats new_float

             new_float = mkFloat dmd is_unlifted bndr1 rhs1

       ; return (env2, floats1, Nothing) }

  | otherwise -- A join point; see Note [Join points and floating]
  = ASSERT(not (isTopLevel top_lvl)) -- can't have top-level join point
    do { (_, bndr1) <- cpCloneBndr env bndr
       ; (bndr2, rhs1) <- cpeJoinPair env bndr1 rhs
       ; return (extendCorePrepEnv env bndr bndr2,
                 emptyFloats,
                 Just (NonRec bndr2 rhs1)) }

cpeBind top_lvl env (Rec pairs)
  | not (isJoinId (head bndrs))
  = do { (env', bndrs1) <- cpCloneBndrs env bndrs
       ; stuff <- zipWithM (cpePair top_lvl Recursive topDmd False env')
                           bndrs1 rhss

       ; let (floats_s, rhss1) = unzip stuff
             all_pairs = foldrOL add_float (bndrs1 `zip` rhss1)
                                           (concatFloats floats_s)

       ; return (extendCorePrepEnvList env (bndrs `zip` bndrs1),
                 unitFloat (FloatLet (Rec all_pairs)),
                 Nothing) }

  | otherwise -- See Note [Join points and floating]
  = do { (env', bndrs1) <- cpCloneBndrs env bndrs
       ; pairs1 <- zipWithM (cpeJoinPair env') bndrs1 rhss

       ; let bndrs2 = map fst pairs1
       ; return (extendCorePrepEnvList env' (bndrs `zip` bndrs2),
                 emptyFloats,
                 Just (Rec pairs1)) }
  where
    (bndrs, rhss) = unzip pairs

        -- Flatten all the floats, and the current
        -- group into a single giant Rec
    add_float (FloatLet (NonRec b r)) prs2 = (b,r) : prs2
    add_float (FloatLet (Rec prs1))   prs2 = prs1 ++ prs2
    add_float b                       _    = pprPanic "cpeBind" (ppr b)

---------------
cpePair :: TopLevelFlag -> RecFlag -> Demand -> Bool
        -> CorePrepEnv -> OutId -> CoreExpr
        -> UniqSM (Floats, CpeRhs)
-- Used for all bindings
-- The binder is already cloned, hence an OutId
cpePair top_lvl is_rec dmd is_unlifted env bndr rhs
  = ASSERT(not (isJoinId bndr)) -- those should use cpeJoinPair
    do { (floats1, rhs1) <- cpeRhsE env rhs

       -- See if we are allowed to float this stuff out of the RHS
       ; (floats2, rhs2) <- float_from_rhs floats1 rhs1

       -- Make the arity match up
       ; (floats3, rhs3)
            <- if manifestArity rhs1 <= arity
               then return (floats2, cpeEtaExpand arity rhs2)
               else WARN(True, text "CorePrep: silly extra arguments:" <+> ppr bndr)
                               -- Note [Silly extra arguments]
                    (do { v <- newVar (idType bndr)
                        ; let float = mkFloat topDmd False v rhs2
                        ; return ( addFloat floats2 float
                                 , cpeEtaExpand arity (Var v)) })

        -- Wrap floating ticks
       ; let (floats4, rhs4) = wrapTicks floats3 rhs3

       ; return (floats4, rhs4) }
  where
    arity = idArity bndr        -- We must match this arity

    ---------------------
    float_from_rhs floats rhs
      | isEmptyFloats floats = return (emptyFloats, rhs)
      | isTopLevel top_lvl   = float_top    floats rhs
      | otherwise            = float_nested floats rhs

    ---------------------
    float_nested floats rhs
      | wantFloatNested is_rec dmd is_unlifted floats rhs
                  = return (floats, rhs)
      | otherwise = dontFloat floats rhs

    ---------------------
    float_top floats rhs
      | allLazyTop floats
      = return (floats, rhs)

      | Just floats <- canFloat floats rhs
      = return floats

      | otherwise
      = dontFloat floats rhs

dontFloat :: Floats -> CpeRhs -> UniqSM (Floats, CpeBody)
-- Non-empty floats, but do not want to float from rhs
-- So wrap the rhs in the floats
-- But: rhs1 might have lambdas, and we can't
--      put them inside a wrapBinds
dontFloat floats1 rhs
  = do { (floats2, body) <- rhsToBody rhs
        ; return (emptyFloats, wrapBinds floats1 $
                               wrapBinds floats2 body) }

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
-- No eta-expansion: see Note [Do not eta-expand join points] in GHC.Core.Op.Simplify.Utils
cpeJoinPair env bndr rhs
  = ASSERT(isJoinId bndr)
    do { let Just join_arity = isJoinId_maybe bndr
             (bndrs, body)   = collectNBinders join_arity rhs

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

cpeRhsE _env expr@(Type {})      = return (emptyFloats, expr)
cpeRhsE _env expr@(Coercion {})  = return (emptyFloats, expr)
cpeRhsE env (Lit (LitNumber LitNumInteger i _))
    = cpeRhsE env (cvtLitInteger (cpe_dynFlags env) (getMkIntegerId env)
                   (cpe_integerSDataCon env) i)
cpeRhsE env (Lit (LitNumber LitNumNatural i _))
    = cpeRhsE env (cvtLitNatural (cpe_dynFlags env) (getMkNaturalId env)
                   (cpe_naturalSDataCon env) i)
cpeRhsE _env expr@(Lit {}) = return (emptyFloats, expr)
cpeRhsE env expr@(Var {})  = cpeApp env expr
cpeRhsE env expr@(App {}) = cpeApp env expr

cpeRhsE env (Let bind body)
  = do { (env', bind_floats, maybe_bind') <- cpeBind NotTopLevel env bind
       ; (body_floats, body') <- cpeRhsE env' body
       ; let expr' = case maybe_bind' of Just bind' -> Let bind' body'
                                         Nothing    -> body'
       ; return (bind_floats `appendFloats` body_floats, expr') }

cpeRhsE env (Tick tickish expr)
  | tickishPlace tickish == PlaceNonLam && tickish `tickishScopesLike` SoftScope
  = do { (floats, body) <- cpeRhsE env expr
         -- See [Floating Ticks in CorePrep]
       ; return (unitFloat (FloatTick tickish) `appendFloats` floats, body) }
  | otherwise
  = do { body <- cpeBodyNF env expr
       ; return (emptyFloats, mkTick tickish' body) }
  where
    tickish' | Breakpoint n fvs <- tickish
             -- See also 'substTickish'
             = Breakpoint n (map (getIdFromTrivialExpr . lookupCorePrepEnv env) fvs)
             | otherwise
             = tickish

cpeRhsE env (Cast expr co)
   = do { (floats, expr') <- cpeRhsE env expr
        ; return (floats, Cast expr' co) }

cpeRhsE env expr@(Lam {})
   = do { let (bndrs,body) = collectBinders expr
        ; (env', bndrs') <- cpCloneBndrs env bndrs
        ; body' <- cpeBodyNF env' body
        ; return (emptyFloats, mkLams bndrs' body') }

cpeRhsE env (Case scrut bndr ty alts)
  | isUnsafeEqualityProof scrut
  , [(con, bs, rhs)] <- alts
  = do { (floats1, scrut') <- cpeBody env scrut
       ; (env1, bndr')     <- cpCloneBndr env bndr
       ; (env2, bs')       <- cpCloneBndrs env1 bs
       ; (floats2, rhs')   <- cpeBody env2 rhs
       ; let case_float = FloatCase scrut' bndr' con bs' True
             floats'    = (floats1 `addFloat` case_float)
                          `appendFloats` floats2
       ; return (floats', rhs') }

  | otherwise
  = do { (floats, scrut') <- cpeBody env scrut
       ; (env', bndr2) <- cpCloneBndr env bndr
       ; let alts'
                 -- This flag is intended to aid in debugging strictness
                 -- analysis bugs. These are particularly nasty to chase down as
                 -- they may manifest as segmentation faults. When this flag is
                 -- enabled we instead produce an 'error' expression to catch
                 -- the case where a function we think should bottom
                 -- unexpectedly returns.
               | gopt Opt_CatchBottoms (cpe_dynFlags env)
               , not (altsAreExhaustive alts)
               = addDefault alts (Just err)
               | otherwise = alts
               where err = mkRuntimeErrorApp rUNTIME_ERROR_ID ty
                                             "Bottoming expression returned"
       ; alts'' <- mapM (sat_alt env') alts'

       ; return (floats, Case scrut' bndr2 ty alts'') }
  where
    sat_alt env (con, bs, rhs)
       = do { (env2, bs') <- cpCloneBndrs env bs
            ; rhs' <- cpeBodyNF env2 rhs
            ; return (con, bs', rhs') }

cvtLitInteger :: DynFlags -> Id -> Maybe DataCon -> Integer -> CoreExpr
-- Here we convert a literal Integer to the low-level
-- representation. Exactly how we do this depends on the
-- library that implements Integer.  If it's GMP we
-- use the S# data constructor for small literals.
-- See Note [Integer literals] in Literal
cvtLitInteger dflags _ (Just sdatacon) i
  | inIntRange dflags i -- Special case for small integers
    = mkConApp sdatacon [Lit (mkLitInt dflags i)]

cvtLitInteger dflags mk_integer _ i
    = mkApps (Var mk_integer) [isNonNegative, ints]
  where isNonNegative = if i < 0 then mkConApp falseDataCon []
                                 else mkConApp trueDataCon  []
        ints = mkListExpr intTy (f (abs i))
        f 0 = []
        f x = let low  = x .&. mask
                  high = x `shiftR` bits
              in mkConApp intDataCon [Lit (mkLitInt dflags low)] : f high
        bits = 31
        mask = 2 ^ bits - 1

cvtLitNatural :: DynFlags -> Id -> Maybe DataCon -> Integer -> CoreExpr
-- Here we convert a literal Natural to the low-level
-- representation.
-- See Note [Natural literals] in Literal
cvtLitNatural dflags _ (Just sdatacon) i
  | inWordRange dflags i -- Special case for small naturals
    = mkConApp sdatacon [Lit (mkLitWord dflags i)]

cvtLitNatural dflags mk_natural _ i
    = mkApps (Var mk_natural) [words]
  where words = mkListExpr wordTy (f i)
        f 0 = []
        f x = let low  = x .&. mask
                  high = x `shiftR` bits
              in mkConApp wordDataCon [Lit (mkLitWord dflags low)] : f high
        bits = 32
        mask = 2 ^ bits - 1

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
       ; return (floats1 `appendFloats` floats2, body) }

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

rhsToBody expr@(Lam {})
  | Just no_lam_result <- tryEtaReducePrep bndrs body
  = return (emptyFloats, no_lam_result)
  | all isTyVar bndrs           -- Type lambdas are ok
  = return (emptyFloats, expr)
  | otherwise                   -- Some value lambdas
  = do { fn <- newVar (exprType expr)
       ; let rhs   = cpeEtaExpand (exprArity expr) expr
             float = FloatLet (NonRec fn rhs)
       ; return (unitFloat float, Var fn) }
  where
    (bndrs,body) = collectBinders expr

rhsToBody expr = return (emptyFloats, expr)



-- ---------------------------------------------------------------------------
--              CpeApp: produces a result satisfying CpeApp
-- ---------------------------------------------------------------------------

data ArgInfo = CpeApp  CoreArg
             | CpeCast Coercion
             | CpeTick (Tickish Id)

instance Outputable ArgInfo where
  ppr (CpeApp arg) = text "app" <+> ppr arg
  ppr (CpeCast co) = text "cast" <+> ppr co
  ppr (CpeTick tick) = text "tick" <+> ppr tick

{-
Note [runRW arg]
~~~~~~~~~~~~~~~~
If we got, say
   runRW# (case bot of {})
which happened in #11291, we do /not/ want to turn it into
   (case bot of {}) realWorldPrimId#
because that gives a panic in CoreToStg.myCollectArgs, which expects
only variables in function position.  But if we are sure to make
runRW# strict (which we do in MkId), this can't happen


Note [CorePrep handling of with#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lower with# applications to touch#. Specifically:

    with# @a @r @b x k s0

is lowered to:

    case k s of _b0 { (# y, s1 #) ->
      case touch# @a x s1 of s2 { _ ->
        (# y, s2 #)
      }
    }

-}

cpeApp :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- May return a CpeRhs because of saturating primops
cpeApp top_env expr
  = do { let (terminal, args, depth) = collect_args expr
       ; cpe_app top_env terminal args depth
       }

  where
    -- We have a nested data structure of the form
    -- e `App` a1 `App` a2 ... `App` an, convert it into
    -- (e, [CpeApp a1, CpeApp a2, ..., CpeApp an], depth)
    -- We use 'ArgInfo' because we may also need to
    -- record casts and ticks.  Depth counts the number
    -- of arguments that would consume strictness information
    -- (so, no type or coercion arguments.)
    collect_args :: CoreExpr -> (CoreExpr, [ArgInfo], Int)
    collect_args e = go e [] 0
      where
        go (App fun arg)      as !depth
            = go fun (CpeApp arg : as)
                (if isTyCoArg arg then depth else depth + 1)
        go (Cast fun co)      as depth
            = go fun (CpeCast co : as) depth
        go (Tick tickish fun) as depth
            | tickishPlace tickish == PlaceNonLam
            && tickish `tickishScopesLike` SoftScope
            = go fun (CpeTick tickish : as) depth
        go terminal as depth = (terminal, as, depth)

    cpe_app :: CorePrepEnv
            -> CoreExpr
            -> [ArgInfo]
            -> Int
            -> UniqSM (Floats, CpeRhs)
    cpe_app env (Var f) (CpeApp Type{} : CpeApp arg : args) depth
        | f `hasKey` lazyIdKey          -- Replace (lazy a) with a, and
       || f `hasKey` noinlineIdKey      -- Replace (noinline a) with a
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
        = let (terminal, args', depth') = collect_args arg
          in cpe_app env terminal (args' ++ args) (depth + depth' - 1)
    cpe_app env (Var f) [CpeApp _runtimeRep@Type{}, CpeApp _type@Type{}, CpeApp arg] 1
        | f `hasKey` runRWKey
        -- See Note [runRW magic]
        -- Replace (runRW# f) by (f realWorld#), beta reducing if possible (this
        -- is why we return a CorePrepEnv as well)
        = case arg of
            Lam s body -> cpe_app (extendCorePrepEnv env s realWorldPrimId) body [] 0
            _          -> cpe_app env arg [CpeApp (Var realWorldPrimId)] 1
    -- See Note [CorePrep handling of with#]
    cpe_app env (Var f) [CpeApp (Type argRep), CpeApp (Type argTy),
                         CpeApp (Type resultRep), CpeApp (Type resultTy),
                         CpeApp x, CpeApp k, CpeApp s0] _depth
        | f `hasKey` withKey
        = do { let voidRepTy = primRepToRuntimeRep VoidRep
             ; b0 <- newVar $ mkTyConApp (tupleTyCon Unboxed 2)
                                         [voidRepTy, resultRep, realWorldStatePrimTy, resultTy]
             ; y <- newVar resultTy
             ; s1 <- newVar realWorldStatePrimTy
             ; s2 <- newVar realWorldStatePrimTy
             ; let touchId = mkPrimOpId TouchOp

                   -- @stateResultAlt s y expr@ is a case alternative of the form,
                   --   (# s, y #) -> expr
                   stateResultAlt :: Var -> Var -> CoreExpr -> CoreAlt
                   stateResultAlt stateVar resultVar rhs =
                     (DataAlt (tupleDataCon Unboxed 2), [stateVar, resultVar], rhs)

                   expr = Case (App k s0) b0 (varType b0) [stateResultAlt s1 y rhs1]
                   rhs1 =
                     let scrut = mkApps (Var touchId) [Type argRep, Type argTy, x, Var s1]
                     in Case scrut s2 (mkTupleTy Unboxed [realWorldStatePrimTy, resultTy]) [(DEFAULT, [], rhs2)]

                   -- (# s2, y #)
                   rhs2 = mkApps (Var $ dataConWrapId $ tupleDataCon Unboxed 2) [Type voidRepTy, Type resultRep, Type realWorldStatePrimTy, Type resultTy, Var s2, Var y]
             ; cpeBody env expr
             }

    cpe_app _env (Var f) args n
        | f `hasKey` withKey
        = pprPanic "cpe_app" (ppr f $$ ppr args $$ ppr n)

    cpe_app env (Var v) args depth
      = do { v1 <- fiddleCCall v
           ; let e2 = lookupCorePrepEnv env v1
                 hd = getIdFromTrivialExpr_maybe e2
           -- NB: depth from collect_args is right, because e2 is a trivial expression
           -- and thus its embedded Id *must* be at the same depth as any
           -- Apps it is under are type applications only (c.f.
           -- exprIsTrivial).  But note that we need the type of the
           -- expression, not the id.
           ; (app, floats) <- rebuild_app args e2 (exprType e2) emptyFloats stricts
           ; mb_saturate hd app floats depth }
        where
          stricts = case idStrictness v of
                            StrictSig (DmdType _ demands _)
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
    cpe_app env fun [] _ = cpeRhsE env fun

        -- N-variable fun, better let-bind it
    cpe_app env fun args depth
      = do { (fun_floats, fun') <- cpeArg env evalDmd fun ty
                          -- The evalDmd says that it's sure to be evaluated,
                          -- so we'll end up case-binding it
           ; (app, floats) <- rebuild_app args fun' ty fun_floats []
           ; mb_saturate Nothing app floats depth }
        where
          ty = exprType fun

    -- Saturate if necessary
    mb_saturate head app floats depth =
       case head of
         Just fn_id -> do { sat_app <- maybeSaturate fn_id app depth
                          ; return (floats, sat_app) }
         _other              -> return (floats, app)

    -- Deconstruct and rebuild the application, floating any non-atomic
    -- arguments to the outside.  We collect the type of the expression,
    -- the head of the application, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.
    rebuild_app
        :: [ArgInfo]                  -- The arguments (inner to outer)
        -> CpeApp
        -> Type
        -> Floats
        -> [Demand]
        -> UniqSM (CpeApp, Floats)
    rebuild_app [] app _ floats ss = do
      MASSERT(null ss) -- make sure we used all the strictness info
      return (app, floats)
    rebuild_app (a : as) fun' fun_ty floats ss = case a of
      CpeApp arg@(Type arg_ty) ->
        rebuild_app as (App fun' arg) (piResultTy fun_ty arg_ty) floats ss
      CpeApp arg@(Coercion {}) ->
        rebuild_app as (App fun' arg) (funResultTy fun_ty) floats ss
      CpeApp arg -> do
        let (ss1, ss_rest)  -- See Note [lazyId magic] in MkId
               = case (ss, isLazyExpr arg) of
                   (_   : ss_rest, True)  -> (topDmd, ss_rest)
                   (ss1 : ss_rest, False) -> (ss1,    ss_rest)
                   ([],            _)     -> (topDmd, [])
            (arg_ty, res_ty) =
              case splitFunTy_maybe fun_ty of
                Just as -> as
                Nothing -> pprPanic "cpeBody" (ppr fun_ty $$ ppr expr)
        (fs, arg') <- cpeArg top_env ss1 arg arg_ty
        rebuild_app as (App fun' arg') res_ty (fs `appendFloats` floats) ss_rest
      CpeCast co ->
        let ty2 = coercionRKind co
        in rebuild_app as (Cast fun' co) ty2 floats ss
      CpeTick tickish ->
        -- See [Floating Ticks in CorePrep]
        rebuild_app as fun' fun_ty (addFloat floats (FloatTick tickish)) ss

isLazyExpr :: CoreExpr -> Bool
-- See Note [lazyId magic] in MkId
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

'runRW' is defined (for historical reasons) in GHC.Magic, with a NOINLINE
pragma.  It is levity-polymorphic.

    runRW# :: forall (r1 :: RuntimeRep). (o :: TYPE r)
           => (State# RealWorld -> (# State# RealWorld, o #))
                              -> (# State# RealWorld, o #)

It needs no special treatment in GHC except this special inlining here
in CorePrep (and in GHC.CoreToByteCode).

-- ---------------------------------------------------------------------------
--      CpeArg: produces a result satisfying CpeArg
-- ---------------------------------------------------------------------------

Note [ANF-ising literal string arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a program like,

    data Foo = Foo Addr#

    foo = Foo "turtle"#

When we go to ANFise this we might think that we want to float the string
literal like we do any other non-trivial argument. This would look like,

    foo = u\ [] case "turtle"# of s { __DEFAULT__ -> Foo s }

However, this 1) isn't necessary since strings are in a sense "trivial"; and 2)
wreaks havoc on the CAF annotations that we produce here since we the result
above is caffy since it is updateable. Ideally at some point in the future we
would like to just float the literal to the top level as suggested in #11312,

    s = "turtle"#
    foo = Foo s

However, until then we simply add a special case excluding literals from the
floating done by cpeArg.
-}

-- | Is an argument okay to CPE?
okCpeArg :: CoreExpr -> Bool
-- Don't float literals. See Note [ANF-ising literal string arguments].
okCpeArg (Lit _) = False
-- Do not eta expand a trivial argument
okCpeArg expr    = not (cpExprIsTrivial expr)

cpExprIsTrivial :: CoreExpr -> Bool
cpExprIsTrivial e
  | Tick t e <- e
  , not (tickishIsCode t)
  = cpExprIsTrivial e
  | Case scrut _ _ alts <- e
  , isUnsafeEqualityProof scrut
  , [(_,_,rhs)] <- alts
  = cpExprIsTrivial rhs
  | otherwise
  = exprIsTrivial e

isUnsafeEqualityProof :: CoreExpr -> Bool
-- See (U3) and (U4) in
-- Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
isUnsafeEqualityProof e
  | Var v `App` Type _ `App` Type _ `App` Type _ <- e
  = idName v == unsafeEqualityProofName
  | otherwise
  = False

-- This is where we arrange that a non-trivial argument is let-bound
cpeArg :: CorePrepEnv -> Demand
       -> CoreArg -> Type -> UniqSM (Floats, CpeArg)
cpeArg env dmd arg arg_ty
  = do { (floats1, arg1) <- cpeRhsE env arg     -- arg1 can be a lambda
       ; (floats2, arg2) <- if want_float floats1 arg1
                            then return (floats1, arg1)
                            else dontFloat floats1 arg1
                -- Else case: arg1 might have lambdas, and we can't
                --            put them inside a wrapBinds

       ; if okCpeArg arg2
         then do { v <- newVar arg_ty
                 ; let arg3      = cpeEtaExpand (exprArity arg2) arg2
                       arg_float = mkFloat dmd is_unlifted v arg3
                 ; return (addFloat floats2 arg_float, varToCoreExpr v) }
         else return (floats2, arg2)
       }
  where
    is_unlifted = isUnliftedType arg_ty
    want_float  = wantFloatNested NonRecursive dmd is_unlifted

{-
Note [Floating unlifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    C (let v* = expensive in v)

where the "*" indicates "will be demanded".  Usually v will have been
inlined by now, but let's suppose it hasn't (see #2756).  Then we
do *not* want to get

     let v* = expensive in C v

because that has different strictness.  Hence the use of 'allLazy'.
(NB: the let v* turns into a FloatCase, in mkLocalNonRec.)


------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

Note [Eta expansion of hasNoBinding things in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maybeSaturate deals with eta expanding to saturate things that can't deal with
unsaturated applications (identified by 'hasNoBinding', currently just
foreign calls and unboxed tuple/sum constructors).

Note that eta expansion in CorePrep is very fragile due to the "prediction" of
CAFfyness made during tidying (see Note [CAFfyness inconsistencies due to eta
expansion in CorePrep] in GHC.Iface.Tidy for details.  We previously saturated primop
applications here as well but due to this fragility (see #16846) we now deal
with this another way, as described in Note [Primop wrappers] in PrimOp.

It's quite likely that eta expansion of constructor applications will
eventually break in a similar way to how primops did. We really should
eliminate this case as well.
-}

maybeSaturate :: Id -> CpeApp -> Int -> UniqSM CpeRhs
maybeSaturate fn expr n_args
  | hasNoBinding fn        -- There's no binding
  = return sat_expr

  | otherwise
  = return expr
  where
    fn_arity     = idArity fn
    excess_arity = fn_arity - n_args
    sat_expr     = cpeEtaExpand excess_arity expr

{-
************************************************************************
*                                                                      *
                Simple GHC.Core operations
*                                                                      *
************************************************************************
-}

{-
-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~~
Eta expand to match the arity claimed by the binder Remember,
CorePrep must not change arity

Eta expansion might not have happened already, because it is done by
the simplifier only when there at least one lambda already.

NB1:we could refrain when the RHS is trivial (which can happen
    for exported things).  This would reduce the amount of code
    generated (a little) and make things a little words for
    code compiled without -O.  The case in point is data constructor
    wrappers.

NB2: we have to be careful that the result of etaExpand doesn't
   invalidate any of the assumptions that CorePrep is attempting
   to establish.  One possible cause is eta expanding inside of
   an SCC note - we're now careful in etaExpand to make sure the
   SCC is pushed inside any new lambdas that are generated.

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

Instead GHC.Core.Arity.etaExpand gives
                f = /\a -> \y -> let s = h 3 in g s y

-}

cpeEtaExpand :: Arity -> CpeRhs -> CpeRhs
cpeEtaExpand arity expr
  | arity == 0 = expr
  | otherwise  = etaExpand arity expr

{-
-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Why try eta reduction?  Hasn't the simplifier already done eta?
But the simplifier only eta reduces if that leaves something
trivial (like f, or f Int).  But for deLam it would be enough to
get to a partial application:
        case x of { p -> \xs. map f xs }
    ==> case x of { p -> map f }
-}

-- When updating this function, make sure it lines up with
-- GHC.Core.Utils.tryEtaReduce!
tryEtaReducePrep :: [CoreBndr] -> CoreExpr -> Maybe CoreExpr
tryEtaReducePrep bndrs expr@(App _ _)
  | ok_to_eta_reduce f
  , n_remaining >= 0
  , and (zipWith ok bndrs last_args)
  , not (any (`elemVarSet` fvs_remaining) bndrs)
  , exprIsHNF remaining_expr   -- Don't turn value into a non-value
                               -- else the behaviour with 'seq' changes
  = Just remaining_expr
  where
    (f, args) = collectArgs expr
    remaining_expr = mkApps f remaining_args
    fvs_remaining = exprFreeVars remaining_expr
    (remaining_args, last_args) = splitAt n_remaining args
    n_remaining = length args - length bndrs

    ok bndr (Var arg) = bndr == arg
    ok _    _         = False

    -- We can't eta reduce something which must be saturated.
    ok_to_eta_reduce (Var f) = not (hasNoBinding f)
    ok_to_eta_reduce _       = False -- Safe. ToDo: generalise


tryEtaReducePrep bndrs (Tick tickish e)
  | tickishFloatable tickish
  = fmap (mkTick tickish) $ tryEtaReducePrep bndrs e

tryEtaReducePrep _ _ = Nothing

{-
************************************************************************
*                                                                      *
                Floats
*                                                                      *
************************************************************************

Note [Pin demand info on floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pin demand info on floated lets, so that we can see the one-shot thunks.
-}

data FloatingBind
  = FloatLet CoreBind    -- Rhs of bindings are CpeRhss
                         -- They are always of lifted type;
                         -- unlifted ones are done with FloatCase

 | FloatCase
      CpeBody         -- Always ok-for-speculation
      Id              -- Case binder
      AltCon [Var]    -- Single alternative
      Bool            -- Ok-for-speculation; False of a strict,
                      -- but lifted binding

 -- | See Note [Floating Ticks in CorePrep]
 | FloatTick (Tickish Id)

data Floats = Floats OkToSpec (OrdList FloatingBind)

instance Outputable FloatingBind where
  ppr (FloatLet b) = ppr b
  ppr (FloatCase r b k bs ok) = text "case" <> braces (ppr ok) <+> ppr r
                                <+> text "of"<+> ppr b <> text "@"
                                <> case bs of
                                   [] -> ppr k
                                   _  -> parens (ppr k <+> ppr bs)
  ppr (FloatTick t) = ppr t

instance Outputable Floats where
  ppr (Floats flag fs) = text "Floats" <> brackets (ppr flag) <+>
                         braces (vcat (map ppr (fromOL fs)))

instance Outputable OkToSpec where
  ppr OkToSpec    = text "OkToSpec"
  ppr IfUnboxedOk = text "IfUnboxedOk"
  ppr NotOkToSpec = text "NotOkToSpec"

-- Can we float these binds out of the rhs of a let?  We cache this decision
-- to avoid having to recompute it in a non-linear way when there are
-- deeply nested lets.
data OkToSpec
   = OkToSpec           -- Lazy bindings of lifted type
   | IfUnboxedOk        -- A mixture of lazy lifted bindings and n
                        -- ok-to-speculate unlifted bindings
   | NotOkToSpec        -- Some not-ok-to-speculate unlifted bindings

mkFloat :: Demand -> Bool -> Id -> CpeRhs -> FloatingBind
mkFloat dmd is_unlifted bndr rhs
  | is_strict
  , not is_hnf  = FloatCase rhs bndr DEFAULT [] (exprOkForSpeculation rhs)
    -- Don't make a case for a HNF binding, even if it's strict
    -- Otherwise we get  case (\x -> e) of ...!

  | is_unlifted = ASSERT2( exprOkForSpeculation rhs, ppr rhs )
                  FloatCase rhs bndr DEFAULT [] True
  | is_hnf    = FloatLet (NonRec bndr                       rhs)
  | otherwise = FloatLet (NonRec (setIdDemandInfo bndr dmd) rhs)
                   -- See Note [Pin demand info on floats]
  where
    is_hnf    = exprIsHNF rhs
    is_strict = isStrictDmd dmd

emptyFloats :: Floats
emptyFloats = Floats OkToSpec nilOL

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats _ bs) = isNilOL bs

wrapBinds :: Floats -> CpeBody -> CpeBody
wrapBinds (Floats _ binds) body
  = foldrOL mk_bind body binds
  where
    mk_bind (FloatCase rhs bndr con bs _) body = Case rhs bndr (exprType body) [(con,bs,body)]
    mk_bind (FloatLet bind)               body = Let bind body
    mk_bind (FloatTick tickish)           body = mkTick tickish body

addFloat :: Floats -> FloatingBind -> Floats
addFloat (Floats ok_to_spec floats) new_float
  = Floats (combine ok_to_spec (check new_float)) (floats `snocOL` new_float)
  where
    check (FloatLet {})  = OkToSpec
    check (FloatCase _ _ _ _ ok_for_spec)
      | ok_for_spec = IfUnboxedOk
      | otherwise   = NotOkToSpec
    check FloatTick{}    = OkToSpec
        -- The ok-for-speculation flag says that it's safe to
        -- float this Case out of a let, and thereby do it more eagerly
        -- We need the top-level flag because it's never ok to float
        -- an unboxed binding to the top level

unitFloat :: FloatingBind -> Floats
unitFloat = addFloat emptyFloats

appendFloats :: Floats -> Floats -> Floats
appendFloats (Floats spec1 floats1) (Floats spec2 floats2)
  = Floats (combine spec1 spec2) (floats1 `appOL` floats2)

concatFloats :: [Floats] -> OrdList FloatingBind
concatFloats = foldr (\ (Floats _ bs1) bs2 -> appOL bs1 bs2) nilOL

combine :: OkToSpec -> OkToSpec -> OkToSpec
combine NotOkToSpec _ = NotOkToSpec
combine _ NotOkToSpec = NotOkToSpec
combine IfUnboxedOk _ = IfUnboxedOk
combine _ IfUnboxedOk = IfUnboxedOk
combine _ _           = OkToSpec

deFloatTop :: Floats -> [CoreBind]
-- For top level only; we don't expect any FloatCases
deFloatTop (Floats _ floats)
  = foldrOL get [] floats
  where
    get (FloatLet b) bs = occurAnalyseRHSs b : bs
    get (FloatCase body var _ _ _) bs
      = occurAnalyseRHSs (NonRec var body) : bs
    get b _ = pprPanic "corePrepPgm" (ppr b)

    -- See Note [Dead code in CorePrep]
    occurAnalyseRHSs (NonRec x e) = NonRec x (occurAnalyseExpr_NoBinderSwap e)
    occurAnalyseRHSs (Rec xes)    = Rec [(x, occurAnalyseExpr_NoBinderSwap e) | (x, e) <- xes]

---------------------------------------------------------------------------

canFloat :: Floats -> CpeRhs -> Maybe (Floats, CpeRhs)
canFloat (Floats ok_to_spec fs) rhs
  | OkToSpec <- ok_to_spec           -- Worth trying
  , Just fs' <- go nilOL (fromOL fs)
  = Just (Floats OkToSpec fs', rhs)
  | otherwise
  = Nothing
  where
    go :: OrdList FloatingBind -> [FloatingBind]
       -> Maybe (OrdList FloatingBind)

    go (fbs_out) [] = Just fbs_out

    go fbs_out (fb@(FloatLet _) : fbs_in)
      = go (fbs_out `snocOL` fb) fbs_in

    go fbs_out (ft@FloatTick{} : fbs_in)
      = go (fbs_out `snocOL` ft) fbs_in

    go _ (FloatCase{} : _) = Nothing


wantFloatNested :: RecFlag -> Demand -> Bool -> Floats -> CpeRhs -> Bool
wantFloatNested is_rec dmd is_unlifted floats rhs
  =  isEmptyFloats floats
  || isStrictDmd dmd
  || is_unlifted
  || (allLazyNested is_rec floats && exprIsHNF rhs)
        -- Why the test for allLazyNested?
        --      v = f (x `divInt#` y)
        -- we don't want to float the case, even if f has arity 2,
        -- because floating the case would make it evaluated too early

allLazyTop :: Floats -> Bool
allLazyTop (Floats OkToSpec _) = True
allLazyTop _                   = False

allLazyNested :: RecFlag -> Floats -> Bool
allLazyNested _      (Floats OkToSpec    _) = True
allLazyNested _      (Floats NotOkToSpec _) = False
allLazyNested is_rec (Floats IfUnboxedOk _) = isNonRec is_rec

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
The gory details are in Note [lazyId magic] in MkId, but the
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

data CorePrepEnv
  = CPE { cpe_dynFlags        :: DynFlags
        , cpe_env             :: IdEnv CoreExpr   -- Clone local Ids
        -- ^ This environment is used for three operations:
        --
        --      1. To support cloning of local Ids so that they are
        --      all unique (see item (6) of CorePrep overview).
        --
        --      2. To support beta-reduction of runRW, see
        --      Note [runRW magic] and Note [runRW arg].
        --
        --      3. To let us inline trivial RHSs of non top-level let-bindings,
        --      see Note [lazyId magic], Note [Inlining in CorePrep]
        --      and Note [CorePrep inlines trivial CoreExpr not Id] (#12076)
        , cpe_mkIntegerId     :: Id
        , cpe_mkNaturalId     :: Id
        , cpe_integerSDataCon :: Maybe DataCon
        , cpe_naturalSDataCon :: Maybe DataCon
    }

lookupMkIntegerName :: DynFlags -> HscEnv -> IO Id
lookupMkIntegerName dflags hsc_env
    = guardIntegerUse dflags $ liftM tyThingId $
      lookupGlobal hsc_env mkIntegerName

lookupMkNaturalName :: DynFlags -> HscEnv -> IO Id
lookupMkNaturalName dflags hsc_env
    = guardNaturalUse dflags $ liftM tyThingId $
      lookupGlobal hsc_env mkNaturalName

-- See Note [The integer library] in PrelNames
lookupIntegerSDataConName :: DynFlags -> HscEnv -> IO (Maybe DataCon)
lookupIntegerSDataConName dflags hsc_env = case integerLibrary dflags of
    IntegerGMP -> guardIntegerUse dflags $ liftM (Just . tyThingDataCon) $
                  lookupGlobal hsc_env integerSDataConName
    IntegerSimple -> return Nothing

lookupNaturalSDataConName :: DynFlags -> HscEnv -> IO (Maybe DataCon)
lookupNaturalSDataConName dflags hsc_env = case integerLibrary dflags of
    IntegerGMP -> guardNaturalUse dflags $ liftM (Just . tyThingDataCon) $
                  lookupGlobal hsc_env naturalSDataConName
    IntegerSimple -> return Nothing

-- | Helper for 'lookupMkIntegerName', 'lookupIntegerSDataConName'
guardIntegerUse :: DynFlags -> IO a -> IO a
guardIntegerUse dflags act
  | thisPackage dflags == primUnitId
  = return $ panic "Can't use Integer in ghc-prim"
  | thisPackage dflags == integerUnitId
  = return $ panic "Can't use Integer in integer-*"
  | otherwise = act

-- | Helper for 'lookupMkNaturalName', 'lookupNaturalSDataConName'
--
-- Just like we can't use Integer literals in `integer-*`, we can't use Natural
-- literals in `base`. If we do, we get interface loading error for GHC.Natural.
guardNaturalUse :: DynFlags -> IO a -> IO a
guardNaturalUse dflags act
  | thisPackage dflags == primUnitId
  = return $ panic "Can't use Natural in ghc-prim"
  | thisPackage dflags == integerUnitId
  = return $ panic "Can't use Natural in integer-*"
  | thisPackage dflags == baseUnitId
  = return $ panic "Can't use Natural in base"
  | otherwise = act

mkInitialCorePrepEnv :: DynFlags -> HscEnv -> IO CorePrepEnv
mkInitialCorePrepEnv dflags hsc_env
    = do mkIntegerId <- lookupMkIntegerName dflags hsc_env
         mkNaturalId <- lookupMkNaturalName dflags hsc_env
         integerSDataCon <- lookupIntegerSDataConName dflags hsc_env
         naturalSDataCon <- lookupNaturalSDataConName dflags hsc_env
         return $ CPE {
                      cpe_dynFlags = dflags,
                      cpe_env = emptyVarEnv,
                      cpe_mkIntegerId = mkIntegerId,
                      cpe_mkNaturalId = mkNaturalId,
                      cpe_integerSDataCon = integerSDataCon,
                      cpe_naturalSDataCon = naturalSDataCon
                  }

extendCorePrepEnv :: CorePrepEnv -> Id -> Id -> CorePrepEnv
extendCorePrepEnv cpe id id'
    = cpe { cpe_env = extendVarEnv (cpe_env cpe) id (Var id') }

extendCorePrepEnvExpr :: CorePrepEnv -> Id -> CoreExpr -> CorePrepEnv
extendCorePrepEnvExpr cpe id expr
    = cpe { cpe_env = extendVarEnv (cpe_env cpe) id expr }

extendCorePrepEnvList :: CorePrepEnv -> [(Id,Id)] -> CorePrepEnv
extendCorePrepEnvList cpe prs
    = cpe { cpe_env = extendVarEnvList (cpe_env cpe)
                        (map (\(id, id') -> (id, Var id')) prs) }

lookupCorePrepEnv :: CorePrepEnv -> Id -> CoreExpr
lookupCorePrepEnv cpe id
  = case lookupVarEnv (cpe_env cpe) id of
        Nothing  -> Var id
        Just exp -> exp

getMkIntegerId :: CorePrepEnv -> Id
getMkIntegerId = cpe_mkIntegerId

getMkNaturalId :: CorePrepEnv -> Id
getMkNaturalId = cpe_mkNaturalId

------------------------------------------------------------------------------
-- Cloning binders
-- ---------------------------------------------------------------------------

cpCloneBndrs :: CorePrepEnv -> [InVar] -> UniqSM (CorePrepEnv, [OutVar])
cpCloneBndrs env bs = mapAccumLM cpCloneBndr env bs

cpCloneBndr  :: CorePrepEnv -> InVar -> UniqSM (CorePrepEnv, OutVar)
cpCloneBndr env bndr
  | not (isId bndr)
  = return (env, bndr)

  | otherwise
  = do { bndr' <- clone_it bndr

       -- Drop (now-useless) rules/unfoldings
       -- See Note [Drop unfoldings and rules]
       -- and Note [Preserve evaluatedness] in GHC.Core.Op.Tidy
       ; let unfolding' = zapUnfolding (realIdUnfolding bndr)
                          -- Simplifier will set the Id's unfolding

             bndr'' = bndr' `setIdUnfolding`      unfolding'
                            `setIdSpecialisation` emptyRuleInfo

       ; return (extendCorePrepEnv env bndr bndr'', bndr'') }
  where
    clone_it bndr
      | isLocalId bndr, not (isCoVar bndr)
      = do { uniq <- getUniqueM; return (setVarUnique bndr uniq) }
      | otherwise   -- Top level things, which we don't want
                    -- to clone, have become GlobalIds by now
                    -- And we don't clone tyvars, or coercion variables
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
see Note [Preserve evaluatedness] in GHC.Core.Op.Tidy.
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
 = seqType ty `seq` do
     uniq <- getUniqueM
     return (mkSysLocalOrCoVar (fsLit "sat") uniq ty)


------------------------------------------------------------------------------
-- Floating ticks
-- ---------------------------------------------------------------------------
--
-- Note [Floating Ticks in CorePrep]
--
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
wrapTicks (Floats flag floats0) expr =
    (Floats flag (toOL $ reverse floats1), foldr mkTick expr (reverse ticks1))
  where (floats1, ticks1) = foldlOL go ([], []) $ floats0
        -- Deeply nested constructors will produce long lists of
        -- redundant source note floats here. We need to eliminate
        -- those early, as relying on mkTick to spot it after the fact
        -- can yield O(n^3) complexity [#11095]
        go (floats, ticks) (FloatTick t)
          = ASSERT(tickishPlace t == PlaceNonLam)
            (floats, if any (flip tickishContains t) ticks
                     then ticks else t:ticks)
        go (floats, ticks) f
          = (foldr wrap f (reverse ticks):floats, ticks)

        wrap t (FloatLet bind)           = FloatLet (wrapBind t bind)
        wrap t (FloatCase r b con bs ok) = FloatCase (mkTick t r) b con bs ok
        wrap _ other                     = pprPanic "wrapTicks: unexpected float!"
                                             (ppr other)
        wrapBind t (NonRec binder rhs) = NonRec binder (mkTick t rhs)
        wrapBind t (Rec pairs)         = Rec (mapSnd (mkTick t) pairs)

------------------------------------------------------------------------------
-- Collecting cost centres
-- ---------------------------------------------------------------------------

-- | Collect cost centres defined in the current module, including those in
-- unfoldings.
collectCostCentres :: Module -> CoreProgram -> S.Set CostCentre
collectCostCentres mod_name
  = foldl' go_bind S.empty
  where
    go cs e = case e of
      Var{} -> cs
      Lit{} -> cs
      App e1 e2 -> go (go cs e1) e2
      Lam _ e -> go cs e
      Let b e -> go (go_bind cs b) e
      Case scrt _ _ alts -> go_alts (go cs scrt) alts
      Cast e _ -> go cs e
      Tick (ProfNote cc _ _) e ->
        go (if ccFromThisModule cc mod_name then S.insert cc cs else cs) e
      Tick _ e -> go cs e
      Type{} -> cs
      Coercion{} -> cs

    go_alts = foldl' (\cs (_con, _bndrs, e) -> go cs e)

    go_bind :: S.Set CostCentre -> CoreBind -> S.Set CostCentre
    go_bind cs (NonRec b e) =
      go (maybe cs (go cs) (get_unf b)) e
    go_bind cs (Rec bs) =
      foldl' (\cs' (b, e) -> go (maybe cs' (go cs') (get_unf b)) e) cs bs

    -- Unfoldings may have cost centres that in the original definion are
    -- optimized away, see #5889.
    get_unf = maybeUnfoldingTemplate . realIdUnfolding
