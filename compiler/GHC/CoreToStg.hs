
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

module GHC.CoreToStg ( coreToStg ) where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Core
import GHC.Core.Utils   ( exprType, findDefault, isJoinBind
                        , exprIsTickedString_maybe )
import GHC.Core.Opt.Arity   ( manifestArity )
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.DataCon

import GHC.Stg.Syntax
import GHC.Stg.Debug

import GHC.Types.RepType
import GHC.Types.Id.Make ( coercionTokenId )
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.CostCentre
import GHC.Types.Tickish
import GHC.Types.Var.Env
import GHC.Types.Name   ( isExternalName, nameModule_maybe )
import GHC.Types.Basic  ( Arity )
import GHC.Types.Literal
import GHC.Types.ForeignCall
import GHC.Types.IPE
import GHC.Types.Demand    ( isUsedOnceDmd )
import GHC.Types.SrcLoc    ( mkGeneralSrcSpan )

import GHC.Unit.Module
import GHC.Builtin.Types ( unboxedUnitDataCon )
import GHC.Data.FastString
import GHC.Platform.Ways
import GHC.Builtin.PrimOps ( PrimCall(..), primOpWrapperId )

import GHC.Utils.Outputable
import GHC.Utils.Monad
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Trace

import Control.Monad (ap)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

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


coreToStg :: DynFlags -> Module -> ModLocation -> CoreProgram
          -> ([StgTopBinding], InfoTableProvMap, CollectedCCs)
coreToStg dflags this_mod ml pgm
  = (pgm'', denv, final_ccs)
  where
    (_, (local_ccs, local_cc_stacks), pgm')
      = coreTopBindsToStg dflags this_mod emptyVarEnv emptyCollectedCCs pgm

    -- See Note [Mapping Info Tables to Source Positions]
    (!pgm'', !denv) =
        if gopt Opt_InfoTableMap dflags
          then collectDebugInformation dflags ml pgm'
          else (pgm', emptyInfoTableProvMap)

    prof = ways dflags `hasWay` WayProf

    final_ccs
      | prof && gopt Opt_AutoSccsOnIndividualCafs dflags
      = (local_ccs,local_cc_stacks)  -- don't need "all CAFs" CC
      | prof
      = (all_cafs_cc:local_ccs, all_cafs_ccs:local_cc_stacks)
      | otherwise
      = emptyCollectedCCs

    (all_cafs_cc, all_cafs_ccs) = getAllCAFsCC this_mod

coreTopBindsToStg
    :: DynFlags
    -> Module
    -> IdEnv HowBound           -- environment for the bindings
    -> CollectedCCs
    -> CoreProgram
    -> (IdEnv HowBound, CollectedCCs, [StgTopBinding])

coreTopBindsToStg _      _        env ccs []
  = (env, ccs, [])
coreTopBindsToStg dflags this_mod env ccs (b:bs)
  | NonRec _ rhs <- b, isTyCoArg rhs
  = coreTopBindsToStg dflags this_mod env1 ccs1 bs
  | otherwise
  = (env2, ccs2, b':bs')
  where
    (env1, ccs1, b' ) = coreTopBindToStg dflags this_mod env ccs b
    (env2, ccs2, bs') = coreTopBindsToStg dflags this_mod env1 ccs1 bs

coreTopBindToStg
        :: DynFlags
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

coreTopBindToStg dflags this_mod env ccs (NonRec id rhs)
  = let
        env'      = extendVarEnv env id how_bound
        how_bound = LetBound TopLet $! manifestArity rhs

        (stg_rhs, ccs') =
            initCts dflags env $
              coreToTopStgRhs dflags ccs this_mod (id,rhs)

        bind = StgTopLifted $ StgNonRec id stg_rhs
    in
      -- NB: previously the assertion printed 'rhs' and 'bind'
      --     as well as 'id', but that led to a black hole
      --     where printing the assertion error tripped the
      --     assertion again!
    (env', ccs', bind)

coreTopBindToStg dflags this_mod env ccs (Rec pairs)
  = assert (not (null pairs)) $
    let
        binders = map fst pairs

        extra_env' = [ (b, LetBound TopLet $! manifestArity rhs)
                     | (b, rhs) <- pairs ]
        env' = extendVarEnvList env extra_env'

        -- generate StgTopBindings and CAF cost centres created for CAFs
        (ccs', stg_rhss)
          = initCts dflags env' $
              mapAccumLM (\ccs rhs -> swap <$> coreToTopStgRhs dflags ccs this_mod rhs)
                         ccs
                         pairs
        bind = StgTopLifted $ StgRec (zip binders stg_rhss)
    in
    (env', ccs', bind)

coreToTopStgRhs
        :: DynFlags
        -> CollectedCCs
        -> Module
        -> (Id,CoreExpr)
        -> CtsM (StgRhs, CollectedCCs)

coreToTopStgRhs dflags ccs this_mod (bndr, rhs)
  = do { new_rhs <- coreToPreStgRhs rhs

       ; let (stg_rhs, ccs') =
               mkTopStgRhs dflags this_mod ccs bndr new_rhs
             stg_arity =
               stgRhsArity stg_rhs

       ; return (assertPpr (arity_ok stg_arity) (mk_arity_msg stg_arity) stg_rhs,
                 ccs') }
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
-- handle with the function coreToPreStgRhs.

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
            -> return (StgLit l)         --    discard the arguments

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

{-
coreToStgExpr (Case scrut _ _ [])
  = coreToStgExpr scrut
    -- See Note [Empty case alternatives] in GHC.Core If the case
    -- alternatives are empty, the scrutinee must diverge or raise an
    -- exception, so we can just dive into it.
    --
    -- Of course this may seg-fault if the scrutinee *does* return.  A
    -- belt-and-braces approach would be to move this case into the
    -- code generator, and put a return point anyway that calls a
    -- runtime system error function.

coreToStgExpr e0@(Case scrut bndr _ [alt]) = do
  | isUnsafeEqualityProof scrut
  , isDeadBinder bndr -- We can only discard the case if the case-binder is dead
                      -- It usually is, but see #18227
  , (_,_,rhs) <- alt
  = coreToStgExpr rhs
    -- See (U2) in Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
-}

-- The normal case for case-expressions
coreToStgExpr (Case scrut bndr _ alts)
  = do { scrut2 <- coreToStgExpr scrut
       ; alts2 <- extendVarEnvCts [(bndr, LambdaBound)] (mapM vars_alt alts)
       ; return (StgCase scrut2 bndr (mkStgAltType bndr alts) alts2) }
  where
    vars_alt :: CoreAlt -> CtsM (AltCon, [Var], StgExpr)
    vars_alt (Alt con binders rhs)
      | DataAlt c <- con, c == unboxedUnitDataCon
      = -- This case is a bit smelly.
        -- See Note [Nullary unboxed tuple] in GHC.Core.Type
        -- where a nullary tuple is mapped to (State# World#)
        assert (null binders) $
        do { rhs2 <- coreToStgExpr rhs
           ; return (DEFAULT, [], rhs2)  }
      | otherwise
      = let     -- Remove type variables
            binders' = filterStgBinders binders
        in
        extendVarEnvCts [(b, LambdaBound) | b <- binders'] $ do
        rhs2 <- coreToStgExpr rhs
        return (con, binders', rhs2)

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
        =  isFunTyCon tc
        || isPrimTyCon tc   -- "Any" is lifted but primitive
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
                  | saturated    -> StgConApp dc NoNumber args'
                                      (dropRuntimeRepArgs (fromMaybe [] (tyConAppArgs_maybe res_ty)))

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
    -- profiling for trac #4367
    app `seq` return tapp

-- ---------------------------------------------------------------------------
-- Argument lists
-- This is the guy that turns applications into A-normal form
-- ---------------------------------------------------------------------------

coreToStgArgs :: [CoreArg] -> CtsM ([StgArg], [StgTickish])
coreToStgArgs []
  = return ([], [])

coreToStgArgs (Type _ : args) = do     -- Type argument
    (args', ts) <- coreToStgArgs args
    return (args', ts)

coreToStgArgs (Coercion _ : args) -- Coercion argument; See Note [Coercion tokens]
  = do { (args', ts) <- coreToStgArgs args
       ; return (StgVarArg coercionTokenId : args', ts) }

coreToStgArgs (Tick t e : args)
  = assert (not (tickishIsCode t)) $
    do { (args', ts) <- coreToStgArgs (e : args)
       ; let !t' = coreToStgTick (exprType e) t
       ; return (args', t':ts) }

coreToStgArgs (arg : args) = do         -- Non-type argument
    (stg_args, ticks) <- coreToStgArgs args
    arg' <- coreToStgExpr arg
    let
        (aticks, arg'') = stripStgTicksTop tickishFloatable arg'
        stg_arg = case arg'' of
           StgApp v []                  -> StgVarArg v
           StgConApp con _ [] _         -> StgVarArg (dataConWorkId con)
           StgOpApp (StgPrimOp op) [] _ -> StgVarArg (primOpWrapperId op)
           StgLit lit                   -> StgLitArg lit
           _ -> pprPanic "coreToStgArgs" (ppr arg $$ pprStgExpr panicStgPprOpts arg' $$ pprStgExpr panicStgPprOpts arg'')

        -- WARNING: what if we have an argument like (v `cast` co)
        --          where 'co' changes the representation type?
        --          (This really only happens if co is unsafe.)
        -- Then all the getArgAmode stuff in CgBindery will set the
        -- cg_rep of the CgIdInfo based on the type of v, rather
        -- than the type of 'co'.
        -- This matters particularly when the function is a primop
        -- or foreign call.
        -- Wanted: a better solution than this hacky warning

    platform <- targetPlatform <$> getDynFlags
    let
        arg_rep = typePrimRep (exprType arg)
        stg_arg_rep = typePrimRep (stgArgType stg_arg)
        bad_args = not (primRepsCompatible platform arg_rep stg_arg_rep)

    warnPprTrace bad_args (text "Dangerous-looking argument. Probable cause: bad unsafeCoerce#" $$ ppr arg) $
     return (stg_arg : stg_args, ticks ++ aticks)

coreToStgTick :: Type -- type of the ticked expression
              -> CoreTickish
              -> StgTickish
coreToStgTick _ty (HpcTick m i)           = HpcTick m i
coreToStgTick _ty (SourceNote span nm)    = SourceNote span nm
coreToStgTick _ty (ProfNote cc cnt scope) = ProfNote cc cnt scope
coreToStgTick !ty (Breakpoint _ bid fvs)  = Breakpoint ty bid fvs

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
    new_rhs <- coreToPreStgRhs rhs
    return (mkStgRhs bndr new_rhs)

-- Represents the RHS of a binding for use with mk(Top)StgRhs.
data PreStgRhs = PreStgRhs [Id] StgExpr -- The [Id] is empty for thunks

-- Convert the RHS of a binding from Core to STG. This is a wrapper around
-- coreToStgExpr that can handle value lambdas.
coreToPreStgRhs :: HasDebugCallStack => CoreExpr -> CtsM PreStgRhs
coreToPreStgRhs expr
  = extendVarEnvCts [ (a, LambdaBound) | a <- args' ] $
    do { body' <- coreToStgExpr body
       ; return (PreStgRhs args' body') }
  where
   (args, body) = myCollectBinders expr
   args'        = filterStgBinders args

-- Generate a top-level RHS. Any new cost centres generated for CAFs will be
-- appended to `CollectedCCs` argument.
mkTopStgRhs :: DynFlags -> Module -> CollectedCCs
            -> Id -> PreStgRhs -> (StgRhs, CollectedCCs)

mkTopStgRhs dflags this_mod ccs bndr (PreStgRhs bndrs rhs)
  | not (null bndrs)
  = -- The list of arguments is non-empty, so not CAF
    ( StgRhsClosure noExtFieldSilent
                    dontCareCCS
                    ReEntrant
                    bndrs rhs
    , ccs )

  -- After this point we know that `bndrs` is empty,
  -- so this is not a function binding
  | StgConApp con mn args _ <- unticked_rhs
  , -- Dynamic StgConApps are updatable
    not (isDllConApp dflags this_mod con args)
  = -- CorePrep does this right, but just to make sure
    assertPpr (not (isUnboxedTupleDataCon con || isUnboxedSumDataCon con))
              (ppr bndr $$ ppr con $$ ppr args)
    ( StgRhsCon dontCareCCS con mn ticks args, ccs )

  -- Otherwise it's a CAF, see Note [Cost-centre initialization plan].
  | gopt Opt_AutoSccsOnIndividualCafs dflags
  = ( StgRhsClosure noExtFieldSilent
                    caf_ccs
                    upd_flag [] rhs
    , collectCC caf_cc caf_ccs ccs )

  | otherwise
  = ( StgRhsClosure noExtFieldSilent
                    all_cafs_ccs
                    upd_flag [] rhs
    , ccs )

  where
    (ticks, unticked_rhs) = stripStgTicksTop (not . tickishIsCode) rhs

    upd_flag | isUsedOnceDmd (idDemandInfo bndr) = SingleEntry
             | otherwise                         = Updatable

    -- CAF cost centres generated for -fcaf-all
    caf_cc = mkAutoCC bndr modl
    caf_ccs = mkSingletonCCS caf_cc
           -- careful: the binder might be :Main.main,
           -- which doesn't belong to module mod_name.
           -- bug #249, tests prof001, prof002
    modl | Just m <- nameModule_maybe (idName bndr) = m
         | otherwise = this_mod

    -- default CAF cost centre
    (_, all_cafs_ccs) = getAllCAFsCC this_mod

-- Generate a non-top-level RHS. Cost-centre is always currentCCS,
-- see Note [Cost-centre initialization plan].
mkStgRhs :: Id -> PreStgRhs -> StgRhs
mkStgRhs bndr (PreStgRhs bndrs rhs)
  | not (null bndrs)
  = StgRhsClosure noExtFieldSilent
                  currentCCS
                  ReEntrant
                  bndrs rhs

  -- After this point we know that `bndrs` is empty,
  -- so this is not a function binding

  | isJoinId bndr -- Must be a nullary join point
  = -- It might have /type/ arguments (T18328),
    -- so its JoinArity might be >0
    StgRhsClosure noExtFieldSilent
                  currentCCS
                  ReEntrant -- ignored for LNE
                  [] rhs

  | StgConApp con mn args _ <- unticked_rhs
  = StgRhsCon currentCCS con mn ticks args

  | otherwise
  = StgRhsClosure noExtFieldSilent
                  currentCCS
                  upd_flag [] rhs
  where
    (ticks, unticked_rhs) = stripStgTicksTop (not . tickishIsCode) rhs

    upd_flag | isUsedOnceDmd (idDemandInfo bndr) = SingleEntry
             | otherwise                         = Updatable

  {-
    SDM: disabled.  Eval/Apply can't handle functions with arity zero very
    well; and making these into simple non-updatable thunks breaks other
    assumptions (namely that they will be entered only once).

    upd_flag | isPAP env rhs  = ReEntrant
             | otherwise      = Updatable

-- Detect thunks which will reduce immediately to PAPs, and make them
-- non-updatable.  This has several advantages:
--
--         - the non-updatable thunk behaves exactly like the PAP,
--
--         - the thunk is more efficient to enter, because it is
--           specialised to the task.
--
--         - we save one update frame, one stg_update_PAP, one update
--           and lots of PAP_enters.
--
--         - in the case where the thunk is top-level, we save building
--           a black hole and furthermore the thunk isn't considered to
--           be a CAF any more, so it doesn't appear in any SRTs.
--
-- We do it here, because the arity information is accurate, and we need
-- to do it before the SRT pass to save the SRT entries associated with
-- any top-level PAPs.

isPAP env (StgApp f args) = listLengthCmp args arity == LT -- idArity f > length args
                              where
                                 arity = stgArity f (lookupBinding env f)
isPAP env _               = False

-}

{- ToDo:
          upd = if isOnceDem dem
                    then (if isNotTop toplev
                            then SingleEntry    -- HA!  Paydirt for "dem"
                            else
                     (if debugIsOn then trace "WARNING: SE CAFs unsupported, forcing UPD instead" else id) $
                     Updatable)
                else Updatable
        -- For now we forbid SingleEntry CAFs; they tickle the
        -- ASSERT in rts/Storage.c line 215 at newCAF() re mut_link,
        -- and I don't understand why.  There's only one SE_CAF (well,
        -- only one that tickled a great gaping bug in an earlier attempt
        -- at ClosureInfo.getEntryConvention) in the whole of nofib,
        -- specifically Main.lvl6 in spectral/cryptarithm2.
        -- So no great loss.  KSW 2000-07.
-}

-- ---------------------------------------------------------------------------
-- A monad for the core-to-STG pass
-- ---------------------------------------------------------------------------

-- There's a lot of stuff to pass around, so we use this CtsM
-- ("core-to-STG monad") monad to help.  All the stuff here is only passed
-- *down*.

newtype CtsM a = CtsM
    { unCtsM :: DynFlags -- Needed for checking for bad coercions in coreToStgArgs
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

initCts :: DynFlags -> IdEnv HowBound -> CtsM a -> a
initCts dflags env m = unCtsM m dflags env



{-# INLINE thenCts #-}
{-# INLINE returnCts #-}

returnCts :: a -> CtsM a
returnCts e = CtsM $ \_ _ -> e

thenCts :: CtsM a -> (a -> CtsM b) -> CtsM b
thenCts m k = CtsM $ \dflags env
  -> unCtsM (k (unCtsM m dflags env)) dflags env

instance Applicative CtsM where
    pure = returnCts
    (<*>) = ap

instance Monad CtsM where
    (>>=)  = thenCts

instance HasDynFlags CtsM where
    getDynFlags = CtsM $ \dflags _ -> dflags

-- Functions specific to this monad:

extendVarEnvCts :: [(Id, HowBound)] -> CtsM a -> CtsM a
extendVarEnvCts ids_w_howbound expr
   =    CtsM $   \dflags env
   -> unCtsM expr dflags (extendVarEnvList env ids_w_howbound)

lookupVarCts :: Id -> CtsM HowBound
lookupVarCts v = CtsM $ \_ env -> lookupBinding env v

lookupBinding :: IdEnv HowBound -> Id -> HowBound
lookupBinding env v = case lookupVarEnv env v of
                        Just xx -> xx
                        Nothing -> assertPpr (isGlobalId v) (ppr v) ImportBound

getAllCAFsCC :: Module -> (CostCentre, CostCentreStack)
getAllCAFsCC this_mod =
    let
      span = mkGeneralSrcSpan (mkFastString "<entire-module>") -- XXX do better
      all_cafs_cc  = mkAllCafsCC this_mod span
      all_cafs_ccs = mkSingletonCCS all_cafs_cc
    in
      (all_cafs_cc, all_cafs_ccs)

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
    go (Lam b e)        as ts
       | isTyVar b            = go e as ts -- Note [Collect args]
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
