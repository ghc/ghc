{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
--

--------------------------------------------------------------
-- Converting Core to STG Syntax
--------------------------------------------------------------

-- And, as we have the info in hand, we may convert some lets to
-- let-no-escapes.

module GHC.CoreToStg ( CoreToStgOpts (..), coreToStg ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Utils
import GHC.Core.Opt.Arity   ( manifestArity )
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.DataCon

import GHC.Stg.Syntax
import GHC.Stg.Debug
import GHC.Stg.Make
import GHC.Stg.Utils (allowTopLevelConApp)

import GHC.Types.RepType
import GHC.Types.Id.Make ( coercionTokenId )
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.CostCentre
import GHC.Types.Tickish
import GHC.Types.Var.Env
import GHC.Types.Name   ( isExternalName )
import GHC.Types.Basic  ( Arity, TypeOrConstraint(..) )
import GHC.Types.Literal
import GHC.Types.ForeignCall
import GHC.Types.IPE

import GHC.Unit.Module
import GHC.Platform        ( Platform )
import GHC.Platform.Ways
import GHC.Builtin.PrimOps

import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Utils.Panic

import Control.Monad (ap)

-- Note [Live vs free]
-- ~~~~~~~~~~~~~~~~~~~
--
-- The two are not the same. Liveness is an operational property rather
-- than a semantic one. A variable is live at a particular execution
-- point if it can be referred to directly again. In particular, a dead
-- variable's stack slot (if it has one):
--
--           - should be stubbed to avoid space leaks, and
--           - may be reused for something else.
--
-- There ought to be a better way to say this. Here are some examples:
--
--         let v = [q] \[x] -> e
--         in
--         ...v...  (but no q's)
--
-- Just after the `in', v is live, but q is dead. If the whole of that
-- let expression was enclosed in a case expression, thus:
--
--         case (let v = [q] \[x] -> e in ...v...) of
--                 alts[...q...]
--
-- (ie `alts' mention `q'), then `q' is live even after the `in'; because
-- we'll return later to the `alts' and need it.
--
-- Let-no-escapes make this a bit more interesting:
--
--         let-no-escape v = [q] \ [x] -> e
--         in
--         ...v...
--
-- Here, `q' is still live at the `in', because `v' is represented not by
-- a closure but by the current stack state.  In other words, if `v' is
-- live then so is `q'. Furthermore, if `e' mentions an enclosing
-- let-no-escaped variable, then its free variables are also live if `v' is.

-- Note [What are these SRTs all about?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Consider the Core program,
--
--     fibs = go 1 1
--       where go a b = let c = a + c
--                      in c : go b c
--     add x = map (\y -> x*y) fibs
--
-- In this case we have a CAF, 'fibs', which is quite large after evaluation and
-- has only one possible user, 'add'. Consequently, we want to ensure that when
-- all references to 'add' die we can garbage collect any bit of 'fibs' that we
-- have evaluated.
--
-- However, how do we know whether there are any references to 'fibs' still
-- around? Afterall, the only reference to it is buried in the code generated
-- for 'add'. The answer is that we record the CAFs referred to by a definition
-- in its info table, namely a part of it known as the Static Reference Table
-- (SRT).
--
-- Since SRTs are so common, we use a special compact encoding for them in: we
-- produce one table containing a list of CAFs in a module and then include a
-- bitmap in each info table describing which entries of this table the closure
-- references.
--
-- See also: commentary/rts/storage/gc/CAFs on the GHC Wiki.

-- Note [What is a non-escaping let]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- NB: Nowadays this is recognized by the occurrence analyser by turning a
-- "non-escaping let" into a join point. The following is then an operational
-- account of join points.
--
-- Consider:
--
--     let x = fvs \ args -> e
--     in
--         if ... then x else
--            if ... then x else ...
--
-- `x' is used twice (so we probably can't unfold it), but when it is
-- entered, the stack is deeper than it was when the definition of `x'
-- happened.  Specifically, if instead of allocating a closure for `x',
-- we saved all `x's fvs on the stack, and remembered the stack depth at
-- that moment, then whenever we enter `x' we can simply set the stack
-- pointer(s) to these remembered (compile-time-fixed) values, and jump
-- to the code for `x'.
--
-- All of this is provided x is:
--   1. non-updatable;
--   2. guaranteed to be entered before the stack retreats -- ie x is not
--      buried in a heap-allocated closure, or passed as an argument to
--      something;
--   3. all the enters have exactly the right number of arguments,
--      no more no less;
--   4. all the enters are tail calls; that is, they return to the
--      caller enclosing the definition of `x'.
--
-- Under these circumstances we say that `x' is non-escaping.
--
-- An example of when (4) does not hold:
--
--     let x = ...
--     in case x of ...alts...
--
-- Here, `x' is certainly entered only when the stack is deeper than when
-- `x' is defined, but here it must return to ...alts... So we can't just
-- adjust the stack down to `x''s recalled points, because that would lost
-- alts' context.
--
-- Things can get a little more complicated.  Consider:
--
--     let y = ...
--     in let x = fvs \ args -> ...y...
--     in ...x...
--
-- Now, if `x' is used in a non-escaping way in ...x..., and `y' is used in a
-- non-escaping way in ...y..., then `y' is non-escaping.
--
-- `x' can even be recursive!  Eg:
--
--     letrec x = [y] \ [v] -> if v then x True else ...
--     in
--         ...(x b)...

-- Note [Cost-centre initialization plan]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Previously `coreToStg` was initializing cost-centre stack fields as `noCCS`,
-- and the fields were then fixed by a separate pass `stgMassageForProfiling`.
-- We now initialize these correctly. The initialization works like this:
--
--   - For non-top level bindings always use `currentCCS`.
--
--   - For top-level bindings, check if the binding is a CAF
--
--     - CAF:      If -fcaf-all is enabled, create a new CAF just for this CAF
--                 and use it. Note that these new cost centres need to be
--                 collected to be able to generate cost centre initialization
--                 code, so `coreToTopStgRhs` now returns `CollectedCCs`.
--
--                 If -fcaf-all is not enabled, use "all CAFs" cost centre.
--
--     - Non-CAF:  Top-level (static) data is not counted in heap profiles; nor
--                 do we set CCCS from it; so we just slam in
--                 dontCareCostCentre.

-- Note [Coercion tokens]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- In coreToStgArgs, we drop type arguments completely, but we replace
-- coercions with a special coercionToken# placeholder. Why? Consider:
--
--   f :: forall a. Int ~# Bool -> a
--   f = /\a. \(co :: Int ~# Bool) -> error "impossible"
--
-- If we erased the coercion argument completely, we’d end up with just
-- f = error "impossible", but then f `seq` () would be ⊥!
--
-- This is an artificial example, but back in the day we *did* treat
-- coercion lambdas like type lambdas, and we had bug reports as a
-- result. So now we treat coercion lambdas like value lambdas, but we
-- treat coercions themselves as zero-width arguments — coercionToken#
-- has representation VoidRep — which gets the best of both worlds.
--
-- (For the gory details, see also the (unpublished) paper, “Practical
-- aspects of evidence-based compilation in System FC.”)

-- --------------------------------------------------------------
-- Setting variable info: top-level, binds, RHSs
-- --------------------------------------------------------------


coreToStg :: CoreToStgOpts -> Module -> ModLocation -> CoreProgram
          -> ([StgTopBinding], InfoTableProvMap, CollectedCCs)
coreToStg opts@CoreToStgOpts
  { coreToStg_ways = ways
  , coreToStg_AutoSccsOnIndividualCafs = opt_AutoSccsOnIndividualCafs
  , coreToStg_InfoTableMap = opt_InfoTableMap
  , coreToStg_stgDebugOpts = stgDebugOpts
  } this_mod ml pgm
  = (pgm'', denv, final_ccs)
  where
    (_, (local_ccs, local_cc_stacks), pgm')
      = coreTopBindsToStg opts this_mod emptyVarEnv emptyCollectedCCs pgm

    -- See Note [Mapping Info Tables to Source Positions]
    (!pgm'', !denv)
      | opt_InfoTableMap
      = collectDebugInformation stgDebugOpts ml pgm'
      | otherwise = (pgm', emptyInfoTableProvMap)

    prof = hasWay ways WayProf

    final_ccs
      | prof && opt_AutoSccsOnIndividualCafs
      = (local_ccs,local_cc_stacks)  -- don't need "all CAFs" CC
      | prof
      = (all_cafs_cc:local_ccs, all_cafs_ccs:local_cc_stacks)
      | otherwise
      = emptyCollectedCCs

    (all_cafs_cc, all_cafs_ccs) = getAllCAFsCC this_mod

coreTopBindsToStg
    :: CoreToStgOpts
    -> Module
    -> IdEnv HowBound           -- environment for the bindings
    -> CollectedCCs
    -> CoreProgram
    -> (IdEnv HowBound, CollectedCCs, [StgTopBinding])

coreTopBindsToStg _      _        env ccs []
  = (env, ccs, [])
coreTopBindsToStg opts this_mod env ccs (b:bs)
  | NonRec _ rhs <- b, isTyCoArg rhs
  = coreTopBindsToStg opts this_mod env1 ccs1 bs
  | otherwise
  = (env2, ccs2, b':bs')
  where
    (env1, ccs1, b' ) = coreTopBindToStg opts this_mod env ccs b
    (env2, ccs2, bs') = coreTopBindsToStg opts this_mod env1 ccs1 bs

coreTopBindToStg
        :: CoreToStgOpts
        -> Module
        -> IdEnv HowBound
        -> CollectedCCs
        -> CoreBind
        -> (IdEnv HowBound, CollectedCCs, StgTopBinding)

coreTopBindToStg _ _ env ccs (NonRec id e)
  | Just str <- exprIsTickedString_maybe e
  -- top-level string literal
  -- See Note [Core top-level string literals] in GHC.Core
  = let
        env' = extendVarEnv env id how_bound
        how_bound = LetBound TopLet 0
    in (env', ccs, StgTopStringLit id str)

coreTopBindToStg opts@CoreToStgOpts
  { coreToStg_platform = platform
  } this_mod env ccs (NonRec id rhs)
  = let
        env'      = extendVarEnv env id how_bound
        how_bound = LetBound TopLet $! manifestArity rhs

        (ccs', (id', stg_rhs)) =
            initCts platform env $
              coreToTopStgRhs opts this_mod ccs (id,rhs)

        bind = StgTopLifted $ StgNonRec id' stg_rhs
    in
      -- NB: previously the assertion printed 'rhs' and 'bind'
      --     as well as 'id', but that led to a black hole
      --     where printing the assertion error tripped the
      --     assertion again!
    (env', ccs', bind)

coreTopBindToStg opts@CoreToStgOpts
  { coreToStg_platform = platform
  } this_mod env ccs (Rec pairs)
  = assert (not (null pairs)) $
    let
        extra_env' = [ (b, LetBound TopLet $! manifestArity rhs)
                     | (b, rhs) <- pairs ]
        env' = extendVarEnvList env extra_env'

        -- generate StgTopBindings and CAF cost centres created for CAFs
        (ccs', stg_rhss)
          = initCts platform env' $ mapAccumLM (coreToTopStgRhs opts this_mod) ccs pairs
        bind = StgTopLifted $ StgRec stg_rhss
    in
    (env', ccs', bind)

coreToTopStgRhs
        :: CoreToStgOpts
        -> Module
        -> CollectedCCs
        -> (Id,CoreExpr)
        -> CtsM (CollectedCCs, (Id, StgRhs))

coreToTopStgRhs opts this_mod ccs (bndr, rhs)
  = do { new_rhs <- coreToMkStgRhs bndr rhs

       ; let (stg_rhs, ccs') =
               mkTopStgRhs (allowTopLevelConApp (coreToStg_platform opts) (coreToStg_ExternalDynamicRefs opts))
                           (coreToStg_AutoSccsOnIndividualCafs opts)
                           this_mod ccs bndr new_rhs
             stg_arity =
               stgRhsArity stg_rhs

       ; pure (ccs', (bndr, assertPpr (arity_ok stg_arity) (mk_arity_msg stg_arity) stg_rhs)) }
  where
        -- It's vital that the arity on a top-level Id matches
        -- the arity of the generated STG binding, else an importing
        -- module will use the wrong calling convention
        --      (#2844 was an example where this happened)
        -- NB1: we can't move the assertion further out without
        --      blocking the "knot" tied in coreTopBindsToStg
        -- NB2: the arity check is only needed for Ids with External
        --      Names, because they are externally visible.  The CorePrep
        --      pass introduces "sat" things with Local Names and does
        --      not bother to set their Arity info, so don't fail for those
    arity_ok stg_arity
       | isExternalName (idName bndr) = id_arity == stg_arity
       | otherwise                    = True
    id_arity  = idArity bndr
    mk_arity_msg stg_arity
        = vcat [ppr bndr,
                text "Id arity:" <+> ppr id_arity,
                text "STG arity:" <+> ppr stg_arity]

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- coreToStgExpr panics if the input expression is a value lambda. CorePrep
-- ensures that value lambdas only exist as the RHS of bindings, which we
-- handle with the function coreToMkStgRhs.

coreToStgExpr
        :: HasDebugCallStack => CoreExpr
        -> CtsM StgExpr

-- The second and third components can be derived in a simple bottom up pass, not
-- dependent on any decisions about which variables will be let-no-escaped or
-- not.  The first component, that is, the decorated expression, may then depend
-- on these components, but it in turn is not scrutinised as the basis for any
-- decisions.  Hence no black holes.

-- No bignum literal should be left by the time this is called.
-- CorePrep should have converted them all to a real core representation.
coreToStgExpr (Lit (LitNumber LitNumBigNat _))  = panic "coreToStgExpr: LitNumBigNat"
coreToStgExpr (Lit l)                           = return (StgLit l)
coreToStgExpr (Var v) = coreToStgApp v [] []
coreToStgExpr (Coercion _)
  -- See Note [Coercion tokens]
  = coreToStgApp coercionTokenId [] []

coreToStgExpr expr@(App _ _)
  = case app_head of
      Var f -> coreToStgApp f args ticks -- Regular application
      Lit l | isLitRubbish l             -- If there is LitRubbish at the head,
                                         --    discard the arguments
                                         --    Recompute representation, because in
                                         --    '(RUBBISH[rep] x) :: (T :: TYPE rep2)'
                                         --    rep might not be equal to rep2
            -> return (StgLit $ LitRubbish TypeLike $ getRuntimeRep (exprType expr))

      _     -> pprPanic "coreToStgExpr - Invalid app head:" (ppr expr)
    where
      (app_head, args, ticks) = myCollectArgs expr
coreToStgExpr expr@(Lam _ _)
  = let
        (args, body) = myCollectBinders expr
    in
    case filterStgBinders args of

      [] -> coreToStgExpr body

      _ -> pprPanic "coretoStgExpr" $
        text "Unexpected value lambda:" $$ ppr expr

coreToStgExpr (Tick tick expr)
  = do
       let !stg_tick = coreToStgTick (exprType expr) tick
       !expr2 <- coreToStgExpr expr
       return (StgTick stg_tick expr2)

coreToStgExpr (Cast expr _)
  = coreToStgExpr expr

-- Cases require a little more real work.
coreToStgExpr (Case scrut bndr _ alts)
  | null alts
  -- See Note [Empty case alternatives] in GHC.Core If the case
  -- alternatives are empty, the scrutinee must diverge or raise an
  -- exception, so we can just dive into it.
  --
  -- Of course this may seg-fault if the scrutinee *does* return.  A
  -- belt-and-braces approach would be to move this case into the
  -- code generator, and put a return point anyway that calls a
  -- runtime system error function.
  = coreToStgExpr scrut

  | Just rhs <- isUnsafeEqualityCase scrut bndr alts
  -- See (U2) in Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
  = coreToStgExpr rhs

  | otherwise
  = do { scrut2 <- coreToStgExpr scrut
       ; alts2 <- extendVarEnvCts [(bndr, LambdaBound)] (mapM vars_alt alts)
       ; return (StgCase scrut2 bndr (mkStgAltType bndr alts) alts2) }
  where
    vars_alt :: CoreAlt -> CtsM StgAlt
    vars_alt (Alt con binders rhs)
      = let     -- Remove type variables
            binders' = filterStgBinders binders
        in
        extendVarEnvCts [(b, LambdaBound) | b <- binders'] $ do
        rhs2 <- coreToStgExpr rhs
        return $! GenStgAlt{ alt_con   = con
                           , alt_bndrs = binders'
                           , alt_rhs   = rhs2
                           }

coreToStgExpr (Let bind body) = coreToStgLet bind body
coreToStgExpr e               = pprPanic "coreToStgExpr" (ppr e)

mkStgAltType :: Id -> [CoreAlt] -> AltType
mkStgAltType bndr alts
  | isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty
  = MultiValAlt (length prim_reps)  -- always use MultiValAlt for unboxed tuples

  | otherwise
  = case prim_reps of
      [rep] | isGcPtrRep rep ->
        case tyConAppTyCon_maybe (unwrapType bndr_ty) of
          Just tc
            | isAbstractTyCon tc -> look_for_better_tycon
            | isAlgTyCon tc      -> AlgAlt tc
            | otherwise          -> assertPpr (_is_poly_alt_tycon tc) (ppr tc) PolyAlt
          Nothing                -> PolyAlt
      [non_gcd] -> PrimAlt non_gcd
      not_unary -> MultiValAlt (length not_unary)
  where
   bndr_ty   = idType bndr
   prim_reps = typePrimRep bndr_ty

   _is_poly_alt_tycon tc
        =  isPrimTyCon tc   -- "Any" is lifted but primitive
        || isFamilyTyCon tc -- Type family; e.g. Any, or arising from strict
                            -- function application where argument has a
                            -- type-family type

   -- Sometimes, the TyCon is a AbstractTyCon which may not have any
   -- constructors inside it.  Then we may get a better TyCon by
   -- grabbing the one from a constructor alternative
   -- if one exists.
   look_for_better_tycon
        | ((Alt (DataAlt con) _ _) : _) <- data_alts =
                AlgAlt (dataConTyCon con)
        | otherwise =
                assert (null data_alts)
                PolyAlt
        where
                (data_alts, _deflt) = findDefault alts

-- ---------------------------------------------------------------------------
-- Applications
-- ---------------------------------------------------------------------------

coreToStgApp :: Id            -- Function
             -> [CoreArg]     -- Arguments
             -> [CoreTickish] -- Debug ticks
             -> CtsM StgExpr
coreToStgApp f args ticks = do
    (args', ticks') <- coreToStgArgs args
    how_bound <- lookupVarCts f

    let
        n_val_args       = valArgCount args

        -- Mostly, the arity info of a function is in the fn's IdInfo
        -- But new bindings introduced by CoreSat may not have no
        -- arity info; it would do us no good anyway.  For example:
        --      let f = \ab -> e in f
        -- No point in having correct arity info for f!
        -- Hence the hasArity stuff below.
        -- NB: f_arity is only consulted for LetBound things
        f_arity   = stgArity f how_bound
        saturated = f_arity <= n_val_args

        res_ty = exprType (mkApps (Var f) args)
        app = case idDetails f of
                DataConWorkId dc
                  | saturated    -> if isUnboxedSumDataCon dc then
                                      StgConApp dc NoNumber args' (sumPrimReps args)
                                    else
                                      StgConApp dc NoNumber args' []

                -- Some primitive operator that might be implemented as a library call.
                -- As noted by Note [Eta expanding primops] in GHC.Builtin.PrimOps
                -- we require that primop applications be saturated.
                PrimOpId op _    -> -- assertPpr saturated (ppr f <+> ppr args) $
                                    StgOpApp (StgPrimOp op) args' res_ty

                -- A call to some primitive Cmm function.
                FCallId (CCall (CCallSpec (StaticTarget _ lbl (Just pkgId) True)
                                          PrimCallConv _))
                                 -> assert saturated $
                                    StgOpApp (StgPrimCallOp (PrimCall lbl pkgId)) args' res_ty

                -- A regular foreign call.
                FCallId call     -> assert saturated $
                                    StgOpApp (StgFCallOp call (idType f)) args' res_ty

                TickBoxOpId {}   -> pprPanic "coreToStg TickBox" $ ppr (f,args')
                _other           -> StgApp f args'

        add_tick !t !e = StgTick t e
        tapp = foldr add_tick app (map (coreToStgTick res_ty) ticks ++ ticks')

    -- Forcing these fixes a leak in the code generator, noticed while
    -- profiling for #4367
    app `seq` return tapp


-- Given Core arguments to an unboxed sum datacon, return the 'PrimRep's
-- of every alternative. For example, in (#_|#) @LiftedRep @IntRep @Int @Int# 0
-- the arguments are [Type LiftedRep, Type IntRep, Type Int, Type Int#, 0]
-- and we return the list [[LiftedRep], [IntRep]].
-- See Note [Representations in StgConApp] in GHC.Stg.Unarise.
sumPrimReps :: [CoreArg] -> [[PrimRep]]
sumPrimReps (Type ty : args) | isRuntimeRepKindedTy ty
  = runtimeRepPrimRep (text "sumPrimReps") ty : sumPrimReps args
sumPrimReps _ = []
-- ---------------------------------------------------------------------------
-- Argument lists
-- This is the guy that turns applications into A-normal form
-- ---------------------------------------------------------------------------

getStgArgFromTrivialArg :: HasDebugCallStack => CoreArg -> StgArg
-- A (non-erased) trivial CoreArg corresponds to an atomic StgArg.
-- CoreArgs may not immediately look trivial, e.g., `case e of {}` or
-- `case unsafeequalityProof of UnsafeRefl -> e` might intervene.
-- Good thing we can just call `trivial_expr_fold` here.
getStgArgFromTrivialArg e = trivial_expr_fold StgVarArg StgLitArg panic panic e
  where
    panic = pprPanic "getStgArgFromTrivialArg" (ppr e)

coreToStgArgs :: [CoreArg] -> CtsM ([StgArg], [StgTickish])
coreToStgArgs []
  = return ([], [])

coreToStgArgs (Type _ : args) = do     -- Type argument
    (args', ts) <- coreToStgArgs args
    return (args', ts)

coreToStgArgs (Coercion _ : args) -- Coercion argument; See Note [Coercion tokens]
  = do { (args', ts) <- coreToStgArgs args
       ; return (StgVarArg coercionTokenId : args', ts) }

coreToStgArgs (arg : args) = do         -- Non-type argument
    (stg_args, ticks) <- coreToStgArgs args
    -- We know that `arg` must be trivial, but it may contain Ticks.
    -- Example from test case `decodeMyStack`:
    --   $ @... ((src<decodeMyStack.hs:18:26-28> Data.Tuple.snd) @Int @[..])
    -- Note that unfortunately the Tick is not at the top.
    -- So we'll traverse the expression twice:
    --   * Once with `stripTicksT` (which collects *all* ticks from the expression)
    --   * and another time with `getStgArgFromTrivialArg`.
    -- Since the argument is trivial, the only place the Tick can occur is
    -- somehow wrapping a variable (give or take type args, as above).
    platform <- getPlatform
    let arg_ty = exprType arg
        ticks' = map (coreToStgTick arg_ty) (stripTicksT (not . tickishIsCode) arg)
        arg' = getStgArgFromTrivialArg arg
        arg_rep = typePrimRep arg_ty
        stg_arg_rep = stgArgRep arg'
        bad_args = not (primRepsCompatible platform arg_rep stg_arg_rep)

    massertPpr (length ticks' <= 1) (text "More than one Tick in trivial arg:" <+> ppr arg)
    warnPprTraceM bad_args "Dangerous-looking argument. Probable cause: bad unsafeCoerce#" (ppr arg)

    return (arg' : stg_args, ticks' ++ ticks)

coreToStgTick :: Type -- type of the ticked expression
              -> CoreTickish
              -> StgTickish
coreToStgTick _ty (HpcTick m i)                = HpcTick m i
coreToStgTick _ty (SourceNote span nm)         = SourceNote span nm
coreToStgTick _ty (ProfNote cc cnt scope)      = ProfNote cc cnt scope
coreToStgTick !ty (Breakpoint _ bid fvs modl)  = Breakpoint ty bid fvs modl

-- ---------------------------------------------------------------------------
-- The magic for lets:
-- ---------------------------------------------------------------------------

coreToStgLet
         :: CoreBind     -- bindings
         -> CoreExpr     -- body
         -> CtsM StgExpr -- new let

coreToStgLet bind body
  | NonRec _ rhs <- bind, isTyCoArg rhs
  = coreToStgExpr body

  | otherwise
  = do { (bind2, env_ext) <- vars_bind bind

          -- Do the body
         ; body2 <- extendVarEnvCts env_ext $
                    coreToStgExpr body

        -- Compute the new let-expression
        ; let new_let | isJoinBind bind
                      = StgLetNoEscape noExtFieldSilent bind2 body2
                      | otherwise
                      = StgLet noExtFieldSilent bind2 body2

        ; return new_let }
  where
    mk_binding binder rhs
        = (binder, LetBound NestedLet (manifestArity rhs))

    vars_bind :: CoreBind
              -> CtsM (StgBinding,
                       [(Id, HowBound)])  -- extension to environment

    vars_bind (NonRec binder rhs) = do
        rhs2 <- coreToStgRhs (binder,rhs)
        let
            env_ext_item = mk_binding binder rhs

        return (StgNonRec binder rhs2, [env_ext_item])

    vars_bind (Rec pairs)
      =    let
                binders = map fst pairs
                env_ext = [ mk_binding b rhs
                          | (b,rhs) <- pairs ]
           in
           extendVarEnvCts env_ext $ do
              rhss2 <- mapM coreToStgRhs pairs
              return (StgRec (binders `zip` rhss2), env_ext)

coreToStgRhs :: (Id,CoreExpr)
             -> CtsM StgRhs

coreToStgRhs (bndr, rhs) = do
    new_rhs <- coreToMkStgRhs bndr rhs
    return (mkStgRhs bndr new_rhs)

-- Convert the RHS of a binding from Core to STG. This is a wrapper around
-- coreToStgExpr that can handle value lambdas.
coreToMkStgRhs :: HasDebugCallStack => Id -> CoreExpr -> CtsM MkStgRhs
coreToMkStgRhs bndr expr = do
  let (args, body) = myCollectBinders expr
  let args'        = filterStgBinders args
  extendVarEnvCts [ (a, LambdaBound) | a <- args' ] $ do
    body' <- coreToStgExpr body
    let mk_rhs = MkStgRhs
          { rhs_args = args'
          , rhs_expr = body'
          , rhs_type = exprType body
          , rhs_is_join = isJoinId bndr
          }
    pure mk_rhs

-- ---------------------------------------------------------------------------
-- A monad for the core-to-STG pass
-- ---------------------------------------------------------------------------

-- There's a lot of stuff to pass around, so we use this CtsM
-- ("core-to-STG monad") monad to help.  All the stuff here is only passed
-- *down*.

newtype CtsM a = CtsM
    { unCtsM :: Platform -- Needed for checking for bad coercions in coreToStgArgs
             -> IdEnv HowBound
             -> a
    }
    deriving (Functor)

data HowBound
  = ImportBound         -- Used only as a response to lookupBinding; never
                        -- exists in the range of the (IdEnv HowBound)

  | LetBound            -- A let(rec) in this module
        LetInfo         -- Whether top level or nested
        Arity           -- Its arity (local Ids don't have arity info at this point)

  | LambdaBound         -- Used for both lambda and case
  deriving (Eq)

data LetInfo
  = TopLet              -- top level things
  | NestedLet
  deriving (Eq)

-- For a let(rec)-bound variable, x, we record LiveInfo, the set of
-- variables that are live if x is live.  This LiveInfo comprises
--         (a) dynamic live variables (ones with a non-top-level binding)
--         (b) static live variables (CAFs or things that refer to CAFs)
--
-- For "normal" variables (a) is just x alone.  If x is a let-no-escaped
-- variable then x is represented by a code pointer and a stack pointer
-- (well, one for each stack).  So all of the variables needed in the
-- execution of x are live if x is, and are therefore recorded in the
-- LetBound constructor; x itself *is* included.
--
-- The set of dynamic live variables is guaranteed ot have no further
-- let-no-escaped variables in it.

-- The std monad functions:

initCts :: Platform -> IdEnv HowBound -> CtsM a -> a
initCts platform env m = unCtsM m platform env



{-# INLINE thenCts #-}
{-# INLINE returnCts #-}

returnCts :: a -> CtsM a
returnCts e = CtsM $ \_ _ -> e

thenCts :: CtsM a -> (a -> CtsM b) -> CtsM b
thenCts m k = CtsM $ \platform env
  -> unCtsM (k (unCtsM m platform env)) platform env

instance Applicative CtsM where
    pure = returnCts
    (<*>) = ap

instance Monad CtsM where
    (>>=)  = thenCts

getPlatform :: CtsM Platform
getPlatform = CtsM const

-- Functions specific to this monad:

extendVarEnvCts :: [(Id, HowBound)] -> CtsM a -> CtsM a
extendVarEnvCts ids_w_howbound expr
   =    CtsM $   \platform env
   -> unCtsM expr platform (extendVarEnvList env ids_w_howbound)

lookupVarCts :: Id -> CtsM HowBound
lookupVarCts v = CtsM $ \_ env -> lookupBinding env v

lookupBinding :: IdEnv HowBound -> Id -> HowBound
lookupBinding env v = case lookupVarEnv env v of
                        Just xx -> xx
                        Nothing -> assertPpr (isGlobalId v) (ppr v) ImportBound

-- Misc.

filterStgBinders :: [Var] -> [Var]
filterStgBinders bndrs = filter isId bndrs

myCollectBinders :: Expr Var -> ([Var], Expr Var)
myCollectBinders expr
  = go [] expr
  where
    go bs (Lam b e)          = go (b:bs) e
    go bs (Cast e _)         = go bs e
    go bs e                  = (reverse bs, e)

-- | If the argument expression is (potential chain of) 'App', return the head
-- of the app chain, and collect ticks/args along the chain.
-- INVARIANT: If the app head is trivial, return the atomic Var/Lit that was
-- wrapped in casts, empty case, ticks, etc.
-- So keep in sync with 'exprIsTrivial'.
myCollectArgs :: HasDebugCallStack => CoreExpr -> (CoreExpr, [CoreArg], [CoreTickish])
myCollectArgs expr
  = go expr [] []
  where
    go h@(Var _v)       as ts = (h, as, ts)
    go (App f a)        as ts = go f (a:as) ts
    go (Tick t e)       as ts = assertPpr (not (tickishIsCode t) || all isTypeArg as)
                                          (ppr e $$ ppr as $$ ppr ts) $
                                -- See Note [Ticks in applications]
                                go e as (t:ts) -- ticks can appear in type apps
    go (Cast e _)       as ts = go e as ts
    go (Case e b _ alts) as ts  -- Just like in exprIsTrivial!
                                -- Otherwise we fall over in case we encounter
                                -- `(case f a of {}) b` in the future.
       | null alts
       = assertPpr (null as) (ppr e $$ ppr as $$ ppr expr) $
                   go e [] ts -- NB: Empty case discards arguments
       | Just rhs <- isUnsafeEqualityCase e b alts
       = go rhs as ts         -- Discards unsafeCoerce in App heads
    go (Lam b e)        as ts
       | isTyVar b            = go e (drop 1 as) ts -- Note [Collect args]
    go e                as ts = (e, as, ts)

{- Note [Collect args]
~~~~~~~~~~~~~~~~~~~~~~
This big-lambda case occurred following a rather obscure eta expansion.
It all seems a bit yukky to me.

Note [Ticks in applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can get an application like
   (tick t f) True False
via inlining in the CorePrep pass; see Note [Inlining in CorePrep]
in GHC.CoreToStg.Prep.  The tick does not satisfy tickishIsCode;
the inlining-in-CorePrep happens for cpExprIsTrivial which tests
tickishIsCode.

So we test the same thing here, pushing any non-code ticks to
the top (they don't generate any code, after all).  This showed
up in the fallout from fixing #19360.
-}

stgArity :: Id -> HowBound -> Arity
stgArity _ (LetBound _ arity) = arity
stgArity f ImportBound        = idArity f
stgArity _ LambdaBound        = 0

data CoreToStgOpts = CoreToStgOpts
  { coreToStg_platform :: Platform
  , coreToStg_ways :: Ways
  , coreToStg_AutoSccsOnIndividualCafs :: Bool
  , coreToStg_InfoTableMap :: Bool
  , coreToStg_ExternalDynamicRefs :: Bool
  , coreToStg_stgDebugOpts :: StgDebugOpts
  }
