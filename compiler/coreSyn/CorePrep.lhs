%
% (c) The University of Glasgow, 1994-2006
%

Core pass to saturate constructors and PrimOps

\begin{code}
{-# LANGUAGE BangPatterns, CPP #-}

module CorePrep (
      corePrepPgm, corePrepExpr, cvtLitInteger,
      lookupMkIntegerName,
  ) where

#include "HsVersions.h"

import OccurAnal

import HscTypes
import PrelNames
import CoreUtils
import CoreArity
import CoreFVs
import CoreMonad        ( endPass, CoreToDo(..) )
import CoreSyn
import CoreSubst
import MkCore hiding( FloatBind(..) )   -- We use our own FloatBind here
import Type
import Literal
import Coercion
import TcEnv
import TcRnMonad
import TyCon
import Demand
import Var
import VarSet
import VarEnv
import Id
import IdInfo
import TysWiredIn
import DataCon
import PrimOp
import BasicTypes
import Module
import UniqSupply
import Maybes
import OrdList
import ErrUtils
import DynFlags
import Util
import Pair
import Outputable
import Platform
import FastString
import Config
import Data.Bits
import Data.List        ( mapAccumL )
import Control.Monad
\end{code}

-- ---------------------------------------------------------------------------
-- Overview
-- ---------------------------------------------------------------------------

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

9.  Replace (lazy e) by e.  See Note [lazyId magic] in MkId.lhs

10. Convert (LitInteger i t) into the core representation
    for the Integer i. Normally this uses mkInteger, but if
    we are using the integer-gmp implementation then there is a
    special case where we use the S# constructor for Integers that
    are in the range of Int.

This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.


Invariants
~~~~~~~~~~
Here is the syntax of the Core produced by CorePrep:

    Trivial expressions
       triv ::= lit |  var
              | triv ty  |  /\a. triv
              | truv co  |  /\c. triv  |  triv |> co

    Applications
       app ::= lit  |  var  |  app triv  |  app ty  | app co | app |> co

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

\begin{code}
type CpeTriv = CoreExpr    -- Non-terminal 'triv'
type CpeApp  = CoreExpr    -- Non-terminal 'app'
type CpeBody = CoreExpr    -- Non-terminal 'body'
type CpeRhs  = CoreExpr    -- Non-terminal 'rhs'
\end{code}

%************************************************************************
%*                                                                      *
                Top level stuff
%*                                                                      *
%************************************************************************

\begin{code}
corePrepPgm :: DynFlags -> HscEnv -> CoreProgram -> [TyCon] -> IO CoreProgram
corePrepPgm dflags hsc_env binds data_tycons = do
    showPass dflags "CorePrep"
    us <- mkSplitUniqSupply 's'
    initialCorePrepEnv <- mkInitialCorePrepEnv dflags hsc_env

    let implicit_binds = mkDataConWorkers data_tycons
            -- NB: we must feed mkImplicitBinds through corePrep too
            -- so that they are suitably cloned and eta-expanded

        binds_out = initUs_ us $ do
                      floats1 <- corePrepTopBinds initialCorePrepEnv binds
                      floats2 <- corePrepTopBinds initialCorePrepEnv implicit_binds
                      return (deFloatTop (floats1 `appendFloats` floats2))

    endPass hsc_env CorePrep binds_out []
    return binds_out

corePrepExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
corePrepExpr dflags hsc_env expr = do
    showPass dflags "CorePrep"
    us <- mkSplitUniqSupply 's'
    initialCorePrepEnv <- mkInitialCorePrepEnv dflags hsc_env
    let new_expr = initUs_ us (cpeBodyNF initialCorePrepEnv expr)
    dumpIfSet_dyn dflags Opt_D_dump_prep "CorePrep" (ppr new_expr)
    return new_expr

corePrepTopBinds :: CorePrepEnv -> [CoreBind] -> UniqSM Floats
-- Note [Floating out of top level bindings]
corePrepTopBinds initialCorePrepEnv binds
  = go initialCorePrepEnv binds
  where
    go _   []             = return emptyFloats
    go env (bind : binds) = do (env', bind') <- cpeBind TopLevel env bind
                               binds' <- go env' binds
                               return (bind' `appendFloats` binds')

mkDataConWorkers :: [TyCon] -> [CoreBind]
-- See Note [Data constructor workers]
-- c.f. Note [Injecting implicit bindings] in TidyPgm
mkDataConWorkers data_tycons
  = [ NonRec id (Var id)        -- The ice is thin here, but it works
    | tycon <- data_tycons,     -- CorePrep will eta-expand it
      data_con <- tyConDataCons tycon,
      let id = dataConWorkId data_con ]
\end{code}

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

Note [CafInfo and floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
What happens when we try to float bindings to the top level?  At this
point all the CafInfo is supposed to be correct, and we must make certain
that is true of the new top-level bindings.  There are two cases
to consider

a) The top-level binding is marked asCafRefs.  In that case we are
   basically fine.  The floated bindings had better all be lazy lets,
   so they can float to top level, but they'll all have HasCafRefs
   (the default) which is safe.

b) The top-level binding is marked NoCafRefs.  This really happens
   Example.  CoreTidy produces
      $fApplicativeSTM [NoCafRefs] = D:Alternative retry# ...blah...
   Now CorePrep has to eta-expand to
      $fApplicativeSTM = let sat = \xy. retry x y
                         in D:Alternative sat ...blah...
   So what we *want* is
      sat [NoCafRefs] = \xy. retry x y
      $fApplicativeSTM [NoCafRefs] = D:Alternative sat ...blah...

   So, gruesomely, we must set the NoCafRefs flag on the sat bindings,
   *and* substutite the modified 'sat' into the old RHS.

   It should be the case that 'sat' is itself [NoCafRefs] (a value, no
   cafs) else the original top-level binding would not itself have been
   marked [NoCafRefs].  The DEBUG check in CoreToStg for
   consistentCafInfo will find this.

This is all very gruesome and horrible. It would be better to figure
out CafInfo later, after CorePrep.  We'll do that in due course.
Meanwhile this horrible hack works.


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
Imagine that we got an input program like this (see Trac #4962):

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


%************************************************************************
%*                                                                      *
                The main code
%*                                                                      *
%************************************************************************

\begin{code}
cpeBind :: TopLevelFlag -> CorePrepEnv -> CoreBind
        -> UniqSM (CorePrepEnv, Floats)
cpeBind top_lvl env (NonRec bndr rhs)
  = do { (_, bndr1) <- cpCloneBndr env bndr
       ; let dmd         = idDemandInfo bndr
             is_unlifted = isUnLiftedType (idType bndr)
       ; (floats, bndr2, rhs2) <- cpePair top_lvl NonRecursive
                                          dmd 
                                          is_unlifted
                                          env bndr1 rhs
       ; let new_float = mkFloat dmd is_unlifted bndr2 rhs2

        -- We want bndr'' in the envt, because it records
        -- the evaluated-ness of the binder
       ; return (extendCorePrepEnv env bndr bndr2,
                 addFloat floats new_float) }

cpeBind top_lvl env (Rec pairs)
  = do { let (bndrs,rhss) = unzip pairs
       ; (env', bndrs1) <- cpCloneBndrs env (map fst pairs)
       ; stuff <- zipWithM (cpePair top_lvl Recursive topDmd False env') bndrs1 rhss

       ; let (floats_s, bndrs2, rhss2) = unzip3 stuff
             all_pairs = foldrOL add_float (bndrs2 `zip` rhss2)
                                           (concatFloats floats_s)
       ; return (extendCorePrepEnvList env (bndrs `zip` bndrs2),
                 unitFloat (FloatLet (Rec all_pairs))) }
  where
        -- Flatten all the floats, and the currrent
        -- group into a single giant Rec
    add_float (FloatLet (NonRec b r)) prs2 = (b,r) : prs2
    add_float (FloatLet (Rec prs1))   prs2 = prs1 ++ prs2
    add_float b                       _    = pprPanic "cpeBind" (ppr b)

---------------
cpePair :: TopLevelFlag -> RecFlag -> Demand -> Bool
        -> CorePrepEnv -> Id -> CoreExpr
        -> UniqSM (Floats, Id, CpeRhs)
-- Used for all bindings
cpePair top_lvl is_rec dmd is_unlifted env bndr rhs
  = do { (floats1, rhs1) <- cpeRhsE env rhs

       -- See if we are allowed to float this stuff out of the RHS
       ; (floats2, rhs2) <- float_from_rhs floats1 rhs1

       -- Make the arity match up
       ; (floats3, rhs')
            <- if manifestArity rhs1 <= arity
               then return (floats2, cpeEtaExpand arity rhs2)
               else WARN(True, text "CorePrep: silly extra arguments:" <+> ppr bndr)
                               -- Note [Silly extra arguments]
                    (do { v <- newVar (idType bndr)
                        ; let float = mkFloat topDmd False v rhs2
                        ; return ( addFloat floats2 float
                                 , cpeEtaExpand arity (Var v)) })

        -- Record if the binder is evaluated
        -- and otherwise trim off the unfolding altogether
        -- It's not used by the code generator; getting rid of it reduces
        -- heap usage and, since we may be changing uniques, we'd have
        -- to substitute to keep it right
       ; let bndr' | exprIsHNF rhs' = bndr `setIdUnfolding` evaldUnfolding
                   | otherwise      = bndr `setIdUnfolding` noUnfolding

       ; return (floats3, bndr', rhs') }
  where
    is_strict_or_unlifted = (isStrictDmd dmd) || is_unlifted

    platform = targetPlatform (cpe_dynFlags env)

    arity = idArity bndr        -- We must match this arity

    ---------------------
    float_from_rhs floats rhs
      | isEmptyFloats floats = return (emptyFloats, rhs)
      | isTopLevel top_lvl    = float_top    floats rhs
      | otherwise             = float_nested floats rhs

    ---------------------
    float_nested floats rhs
      | wantFloatNested is_rec is_strict_or_unlifted floats rhs
                  = return (floats, rhs)
      | otherwise = dont_float floats rhs

    ---------------------
    float_top floats rhs        -- Urhgh!  See Note [CafInfo and floating]
      | mayHaveCafRefs (idCafInfo bndr)
      , allLazyTop floats
      = return (floats, rhs)

      -- So the top-level binding is marked NoCafRefs
      | Just (floats', rhs') <- canFloatFromNoCaf platform floats rhs
      = return (floats', rhs')

      | otherwise
      = dont_float floats rhs

    ---------------------
    dont_float floats rhs
      -- Non-empty floats, but do not want to float from rhs
      -- So wrap the rhs in the floats
      -- But: rhs1 might have lambdas, and we can't
      --      put them inside a wrapBinds
      = do { body <- rhsToBodyNF rhs
           ; return (emptyFloats, wrapBinds floats body) }

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
cpeRhsE env (Lit (LitInteger i _))
    = cpeRhsE env (cvtLitInteger (cpe_dynFlags env) (getMkIntegerId env) i)
cpeRhsE _env expr@(Lit {}) = return (emptyFloats, expr)
cpeRhsE env expr@(Var {})  = cpeApp env expr

cpeRhsE env (Var f `App` _ `App` arg)
  | f `hasKey` lazyIdKey          -- Replace (lazy a) by a
  = cpeRhsE env arg               -- See Note [lazyId magic] in MkId

cpeRhsE env expr@(App {}) = cpeApp env expr

cpeRhsE env (Let bind expr)
  = do { (env', new_binds) <- cpeBind NotTopLevel env bind
       ; (floats, body) <- cpeRhsE env' expr
       ; return (new_binds `appendFloats` floats, body) }

cpeRhsE env (Tick tickish expr)
  | ignoreTickish tickish
  = cpeRhsE env expr
  | otherwise         -- Just SCCs actually
  = do { body <- cpeBodyNF env expr
       ; return (emptyFloats, Tick tickish' body) }
  where
    tickish' | Breakpoint n fvs <- tickish
             = Breakpoint n (map (lookupCorePrepEnv env) fvs)
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
  = do { (floats, scrut') <- cpeBody env scrut
       ; let bndr1 = bndr `setIdUnfolding` evaldUnfolding
            -- Record that the case binder is evaluated in the alternatives
       ; (env', bndr2) <- cpCloneBndr env bndr1
       ; alts' <- mapM (sat_alt env') alts
       ; return (floats, Case scrut' bndr2 ty alts') }
  where
    sat_alt env (con, bs, rhs)
       = do { (env2, bs') <- cpCloneBndrs env bs
            ; rhs' <- cpeBodyNF env2 rhs
            ; return (con, bs', rhs') }

cvtLitInteger :: DynFlags -> Id -> Integer -> CoreExpr
-- Here we convert a literal Integer to the low-level
-- represenation. Exactly how we do this depends on the
-- library that implements Integer.  If it's GMP we
-- use the S# data constructor for small literals.
-- See Note [Integer literals] in Literal
cvtLitInteger dflags mk_integer i
  | cIntegerLibraryType == IntegerGMP
  , inIntRange dflags i       -- Special case for small integers in GMP
    = mkConApp integerGmpSDataCon [Lit (mkMachInt dflags i)]

  | otherwise
    = mkApps (Var mk_integer) [isNonNegative, ints]
  where isNonNegative = if i < 0 then mkConApp falseDataCon []
                                 else mkConApp trueDataCon  []
        ints = mkListExpr intTy (f (abs i))
        f 0 = []
        f x = let low  = x .&. mask
                  high = x `shiftR` bits
              in mkConApp intDataCon [Lit (mkMachInt dflags low)] : f high
        bits = 31
        mask = 2 ^ bits - 1

-- ---------------------------------------------------------------------------
--              CpeBody: produces a result satisfying CpeBody
-- ---------------------------------------------------------------------------

cpeBodyNF :: CorePrepEnv -> CoreExpr -> UniqSM CpeBody
cpeBodyNF env expr
  = do { (floats, body) <- cpeBody env expr
       ; return (wrapBinds floats body) }

--------
cpeBody :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeBody)
cpeBody env expr
  = do { (floats1, rhs) <- cpeRhsE env expr
       ; (floats2, body) <- rhsToBody rhs
       ; return (floats1 `appendFloats` floats2, body) }

--------
rhsToBodyNF :: CpeRhs -> UniqSM CpeBody
rhsToBodyNF rhs = do { (floats,body) <- rhsToBody rhs
                     ; return (wrapBinds floats body) }

--------
rhsToBody :: CpeRhs -> UniqSM (Floats, CpeBody)
-- Remove top level lambdas by let-binding

rhsToBody (Tick t expr)
  | not (tickishScoped t)  -- we can only float out of non-scoped annotations
  = do { (floats, expr') <- rhsToBody expr
       ; return (floats, Tick t expr') }

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

cpeApp :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- May return a CpeRhs because of saturating primops
cpeApp env expr
  = do { (app, (head,depth), _, floats, ss) <- collect_args expr 0
       ; MASSERT(null ss)       -- make sure we used all the strictness info

        -- Now deal with the function
       ; case head of
           Var fn_id -> do { sat_app <- maybeSaturate fn_id app depth
                           ; return (floats, sat_app) }
           _other    -> return (floats, app) }

  where
    -- Deconstruct and rebuild the application, floating any non-atomic
    -- arguments to the outside.  We collect the type of the expression,
    -- the head of the application, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.

    collect_args
        :: CoreExpr
        -> Int                     -- Current app depth
        -> UniqSM (CpeApp,         -- The rebuilt expression
                   (CoreExpr,Int), -- The head of the application,
                                   -- and no. of args it was applied to
                   Type,           -- Type of the whole expr
                   Floats,         -- Any floats we pulled out
                   [Demand])       -- Remaining argument demands

    collect_args (App fun arg@(Type arg_ty)) depth
      = do { (fun',hd,fun_ty,floats,ss) <- collect_args fun depth
           ; return (App fun' arg, hd, applyTy fun_ty arg_ty, floats, ss) }

    collect_args (App fun arg@(Coercion arg_co)) depth
      = do { (fun',hd,fun_ty,floats,ss) <- collect_args fun depth
           ; return (App fun' arg, hd, applyCo fun_ty arg_co, floats, ss) }

    collect_args (App fun arg) depth
      = do { (fun',hd,fun_ty,floats,ss) <- collect_args fun (depth+1)
           ; let
              (ss1, ss_rest)   = case ss of
                                   (ss1:ss_rest)             -> (ss1,     ss_rest)
                                   []                        -> (topDmd, [])
              (arg_ty, res_ty) = expectJust "cpeBody:collect_args" $
                                 splitFunTy_maybe fun_ty

           ; (fs, arg') <- cpeArg env ss1 arg arg_ty
           ; return (App fun' arg', hd, res_ty, fs `appendFloats` floats, ss_rest) }

    collect_args (Var v) depth
      = do { v1 <- fiddleCCall v
           ; let v2 = lookupCorePrepEnv env v1
           ; return (Var v2, (Var v2, depth), idType v2, emptyFloats, stricts) }
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

    collect_args (Cast fun co) depth
      = do { let Pair _ty1 ty2 = coercionKind co
           ; (fun', hd, _, floats, ss) <- collect_args fun depth
           ; return (Cast fun' co, hd, ty2, floats, ss) }

    collect_args (Tick tickish fun) depth
      | ignoreTickish tickish   -- Drop these notes altogether
      = collect_args fun depth  -- They aren't used by the code generator

        -- N-variable fun, better let-bind it
    collect_args fun depth
      = do { (fun_floats, fun') <- cpeArg env evalDmd fun ty
                          -- The evalDmd says that it's sure to be evaluated,
                          -- so we'll end up case-binding it
           ; return (fun', (fun', depth), ty, fun_floats, []) }
        where
          ty = exprType fun

-- ---------------------------------------------------------------------------
--      CpeArg: produces a result satisfying CpeArg
-- ---------------------------------------------------------------------------

-- This is where we arrange that a non-trivial argument is let-bound
cpeArg :: CorePrepEnv -> Demand 
       -> CoreArg -> Type -> UniqSM (Floats, CpeTriv)
cpeArg env dmd arg arg_ty
  = do { (floats1, arg1) <- cpeRhsE env arg     -- arg1 can be a lambda
       ; (floats2, arg2) <- if want_float floats1 arg1
                            then return (floats1, arg1)
                            else do { body1 <- rhsToBodyNF arg1
                                    ; return (emptyFloats, wrapBinds floats1 body1) }
                -- Else case: arg1 might have lambdas, and we can't
                --            put them inside a wrapBinds

       ; if cpe_ExprIsTrivial arg2    -- Do not eta expand a trivial argument
         then return (floats2, arg2)
         else do
       { v <- newVar arg_ty
       ; let arg3      = cpeEtaExpand (exprArity arg2) arg2
             arg_float = mkFloat dmd is_unlifted v arg3
       ; return (addFloat floats2 arg_float, varToCoreExpr v) } }
  where
    is_unlifted = isUnLiftedType arg_ty
    is_strict   = isStrictDmd dmd
    want_float  = wantFloatNested NonRecursive (is_strict || is_unlifted)
\end{code}

Note [Floating unlifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    C (let v* = expensive in v)

where the "*" indicates "will be demanded".  Usually v will have been
inlined by now, but let's suppose it hasn't (see Trac #2756).  Then we
do *not* want to get

     let v* = expensive in C v

because that has different strictness.  Hence the use of 'allLazy'.
(NB: the let v* turns into a FloatCase, in mkLocalNonRec.)


------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

maybeSaturate deals with saturating primops and constructors
The type is the type of the entire application

\begin{code}
maybeSaturate :: Id -> CpeApp -> Int -> UniqSM CpeRhs
maybeSaturate fn expr n_args
  | Just DataToTagOp <- isPrimOpId_maybe fn     -- DataToTag must have an evaluated arg
                                                -- A gruesome special case
  = saturateDataToTag sat_expr

  | hasNoBinding fn        -- There's no binding
  = return sat_expr

  | otherwise
  = return expr
  where
    fn_arity     = idArity fn
    excess_arity = fn_arity - n_args
    sat_expr     = cpeEtaExpand excess_arity expr

-------------
saturateDataToTag :: CpeApp -> UniqSM CpeApp
-- See Note [dataToTag magic]
saturateDataToTag sat_expr
  = do { let (eta_bndrs, eta_body) = collectBinders sat_expr
       ; eta_body' <- eval_data2tag_arg eta_body
       ; return (mkLams eta_bndrs eta_body') }
  where
    eval_data2tag_arg :: CpeApp -> UniqSM CpeBody
    eval_data2tag_arg app@(fun `App` arg)
        | exprIsHNF arg         -- Includes nullary constructors
        = return app            -- The arg is evaluated
        | otherwise                     -- Arg not evaluated, so evaluate it
        = do { arg_id <- newVar (exprType arg)
             ; let arg_id1 = setIdUnfolding arg_id evaldUnfolding
             ; return (Case arg arg_id1 (exprType app)
                            [(DEFAULT, [], fun `App` Var arg_id1)]) }

    eval_data2tag_arg (Tick t app)    -- Scc notes can appear
        = do { app' <- eval_data2tag_arg app
             ; return (Tick t app') }

    eval_data2tag_arg other     -- Should not happen
        = pprPanic "eval_data2tag" (ppr other)
\end{code}

Note [dataToTag magic]
~~~~~~~~~~~~~~~~~~~~~~
Horrid: we must ensure that the arg of data2TagOp is evaluated
  (data2tag x) -->  (case x of y -> data2tag y)
(yuk yuk) take into account the lambdas we've now introduced

How might it not be evaluated?  Well, we might have floated it out
of the scope of a `seq`, or dropped the `seq` altogether.


%************************************************************************
%*                                                                      *
                Simple CoreSyn operations
%*                                                                      *
%************************************************************************

\begin{code}
-- we don't ignore any Tickishes at the moment.
ignoreTickish :: Tickish Id -> Bool
ignoreTickish _ = False

cpe_ExprIsTrivial :: CoreExpr -> Bool
-- Version that doesn't consider an scc annotation to be trivial.
cpe_ExprIsTrivial (Var _)        = True
cpe_ExprIsTrivial (Type _)       = True
cpe_ExprIsTrivial (Coercion _)   = True
cpe_ExprIsTrivial (Lit _)        = True
cpe_ExprIsTrivial (App e arg)    = isTypeArg arg && cpe_ExprIsTrivial e
cpe_ExprIsTrivial (Tick t e)     = not (tickishIsCode t) && cpe_ExprIsTrivial e
cpe_ExprIsTrivial (Cast e _)     = cpe_ExprIsTrivial e
cpe_ExprIsTrivial (Lam b body) | isTyVar b = cpe_ExprIsTrivial body
cpe_ExprIsTrivial _              = False
\end{code}

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

Instead CoreArity.etaExpand gives
                f = /\a -> \y -> let s = h 3 in g s y

\begin{code}
cpeEtaExpand :: Arity -> CpeRhs -> CpeRhs
cpeEtaExpand arity expr
  | arity == 0 = expr
  | otherwise  = etaExpand arity expr
\end{code}

-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Why try eta reduction?  Hasn't the simplifier already done eta?
But the simplifier only eta reduces if that leaves something
trivial (like f, or f Int).  But for deLam it would be enough to
get to a partial application:
        case x of { p -> \xs. map f xs }
    ==> case x of { p -> map f }

\begin{code}
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

tryEtaReducePrep bndrs (Let bind@(NonRec _ r) body)
  | not (any (`elemVarSet` fvs) bndrs)
  = case tryEtaReducePrep bndrs body of
        Just e -> Just (Let bind e)
        Nothing -> Nothing
  where
    fvs = exprFreeVars r

tryEtaReducePrep _ _ = Nothing
\end{code}


%************************************************************************
%*                                                                      *
                Floats
%*                                                                      *
%************************************************************************

Note [Pin demand info on floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pin demand info on floated lets so that we can see the one-shot thunks.

\begin{code}
data FloatingBind
  = FloatLet CoreBind    -- Rhs of bindings are CpeRhss
                         -- They are always of lifted type;
                         -- unlifted ones are done with FloatCase

 | FloatCase
      Id CpeBody
      Bool              -- The bool indicates "ok-for-speculation"

data Floats = Floats OkToSpec (OrdList FloatingBind)

instance Outputable FloatingBind where
  ppr (FloatLet b) = ppr b
  ppr (FloatCase b r ok) = brackets (ppr ok) <+> ppr b <+> equals <+> ppr r

instance Outputable Floats where
  ppr (Floats flag fs) = ptext (sLit "Floats") <> brackets (ppr flag) <+>
                         braces (vcat (map ppr (fromOL fs)))

instance Outputable OkToSpec where
  ppr OkToSpec    = ptext (sLit "OkToSpec")
  ppr IfUnboxedOk = ptext (sLit "IfUnboxedOk")
  ppr NotOkToSpec = ptext (sLit "NotOkToSpec")

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
  | use_case  = FloatCase bndr rhs (exprOkForSpeculation rhs)
  | is_hnf    = FloatLet (NonRec bndr                       rhs)
  | otherwise = FloatLet (NonRec (setIdDemandInfo bndr dmd) rhs)
                   -- See Note [Pin demand info on floats]
  where
    is_hnf    = exprIsHNF rhs
    is_strict = isStrictDmd dmd
    use_case  = is_unlifted || is_strict && not is_hnf
                -- Don't make a case for a value binding,
                -- even if it's strict.  Otherwise we get
                --      case (\x -> e) of ...!

emptyFloats :: Floats
emptyFloats = Floats OkToSpec nilOL

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats _ bs) = isNilOL bs

wrapBinds :: Floats -> CpeBody -> CpeBody
wrapBinds (Floats _ binds) body
  = foldrOL mk_bind body binds
  where
    mk_bind (FloatCase bndr rhs _) body = Case rhs bndr (exprType body) [(DEFAULT, [], body)]
    mk_bind (FloatLet bind)        body = Let bind body

addFloat :: Floats -> FloatingBind -> Floats
addFloat (Floats ok_to_spec floats) new_float
  = Floats (combine ok_to_spec (check new_float)) (floats `snocOL` new_float)
  where
    check (FloatLet _) = OkToSpec
    check (FloatCase _ _ ok_for_spec)
        | ok_for_spec  =  IfUnboxedOk
        | otherwise    =  NotOkToSpec
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
    get b            _  = pprPanic "corePrepPgm" (ppr b)

    -- See Note [Dead code in CorePrep]
    occurAnalyseRHSs (NonRec x e) = NonRec x (occurAnalyseExpr_NoBinderSwap e)
    occurAnalyseRHSs (Rec xes)    = Rec [(x, occurAnalyseExpr_NoBinderSwap e) | (x, e) <- xes]

---------------------------------------------------------------------------

canFloatFromNoCaf :: Platform -> Floats -> CpeRhs -> Maybe (Floats, CpeRhs)
       -- Note [CafInfo and floating]
canFloatFromNoCaf platform (Floats ok_to_spec fs) rhs
  | OkToSpec <- ok_to_spec           -- Worth trying
  , Just (subst, fs') <- go (emptySubst, nilOL) (fromOL fs)
  = Just (Floats OkToSpec fs', subst_expr subst rhs)
  | otherwise
  = Nothing
  where
    subst_expr = substExpr (text "CorePrep")

    go :: (Subst, OrdList FloatingBind) -> [FloatingBind]
       -> Maybe (Subst, OrdList FloatingBind)

    go (subst, fbs_out) [] = Just (subst, fbs_out)

    go (subst, fbs_out) (FloatLet (NonRec b r) : fbs_in)
      | rhs_ok r
      = go (subst', fbs_out `snocOL` new_fb) fbs_in
      where
        (subst', b') = set_nocaf_bndr subst b
        new_fb = FloatLet (NonRec b' (subst_expr subst r))

    go (subst, fbs_out) (FloatLet (Rec prs) : fbs_in)
      | all rhs_ok rs
      = go (subst', fbs_out `snocOL` new_fb) fbs_in
      where
        (bs,rs) = unzip prs
        (subst', bs') = mapAccumL set_nocaf_bndr subst bs
        rs' = map (subst_expr subst') rs
        new_fb = FloatLet (Rec (bs' `zip` rs'))

    go _ _ = Nothing      -- Encountered a caffy binding

    ------------
    set_nocaf_bndr subst bndr
      = (extendIdSubst subst bndr (Var bndr'), bndr')
      where
        bndr' = bndr `setIdCafInfo` NoCafRefs

    ------------
    rhs_ok :: CoreExpr -> Bool
    -- We can only float to top level from a NoCaf thing if
    -- the new binding is static. However it can't mention
    -- any non-static things or it would *already* be Caffy
    rhs_ok = rhsIsStatic platform (\_ -> False)

wantFloatNested :: RecFlag -> Bool -> Floats -> CpeRhs -> Bool
wantFloatNested is_rec strict_or_unlifted floats rhs
  =  isEmptyFloats floats
  || strict_or_unlifted
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
\end{code}


%************************************************************************
%*                                                                      *
                Cloning
%*                                                                      *
%************************************************************************

\begin{code}
-- ---------------------------------------------------------------------------
--                      The environment
-- ---------------------------------------------------------------------------

data CorePrepEnv = CPE {
                       cpe_dynFlags    :: DynFlags,
                       cpe_env         :: (IdEnv Id), -- Clone local Ids
                       cpe_mkIntegerId :: Id
                   }

lookupMkIntegerName :: DynFlags -> HscEnv -> IO Id
lookupMkIntegerName dflags hsc_env
    = if thisPackage dflags == primPackageKey
      then return $ panic "Can't use Integer in ghc-prim"
      else if thisPackage dflags == integerPackageKey
      then return $ panic "Can't use Integer in integer"
      else liftM tyThingId
         $ initTcForLookup hsc_env (tcLookupGlobal mkIntegerName)

mkInitialCorePrepEnv :: DynFlags -> HscEnv -> IO CorePrepEnv
mkInitialCorePrepEnv dflags hsc_env
    = do mkIntegerId <- lookupMkIntegerName dflags hsc_env
         return $ CPE {
                      cpe_dynFlags = dflags,
                      cpe_env = emptyVarEnv,
                      cpe_mkIntegerId = mkIntegerId
                  }

extendCorePrepEnv :: CorePrepEnv -> Id -> Id -> CorePrepEnv
extendCorePrepEnv cpe id id'
    = cpe { cpe_env = extendVarEnv (cpe_env cpe) id id' }

extendCorePrepEnvList :: CorePrepEnv -> [(Id,Id)] -> CorePrepEnv
extendCorePrepEnvList cpe prs
    = cpe { cpe_env = extendVarEnvList (cpe_env cpe) prs }

lookupCorePrepEnv :: CorePrepEnv -> Id -> Id
lookupCorePrepEnv cpe id
  = case lookupVarEnv (cpe_env cpe) id of
        Nothing  -> id
        Just id' -> id'

getMkIntegerId :: CorePrepEnv -> Id
getMkIntegerId = cpe_mkIntegerId

------------------------------------------------------------------------------
-- Cloning binders
-- ---------------------------------------------------------------------------

cpCloneBndrs :: CorePrepEnv -> [Var] -> UniqSM (CorePrepEnv, [Var])
cpCloneBndrs env bs = mapAccumLM cpCloneBndr env bs

cpCloneBndr  :: CorePrepEnv -> Var -> UniqSM (CorePrepEnv, Var)
cpCloneBndr env bndr
  | isLocalId bndr, not (isCoVar bndr)
  = do bndr' <- setVarUnique bndr <$> getUniqueM

       -- We are going to OccAnal soon, so drop (now-useless) rules/unfoldings
       -- so that we can drop more stuff as dead code.
       -- See also Note [Dead code in CorePrep]
       let bndr'' = bndr' `setIdUnfolding` noUnfolding
                          `setIdSpecialisation` emptySpecInfo
       return (extendCorePrepEnv env bndr bndr'', bndr'')

  | otherwise   -- Top level things, which we don't want
                -- to clone, have become GlobalIds by now
                -- And we don't clone tyvars, or coercion variables
  = return (env, bndr)


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
     return (mkSysLocal (fsLit "sat") uniq ty)
\end{code}
