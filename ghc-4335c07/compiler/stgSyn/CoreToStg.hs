{-# LANGUAGE CPP #-}

--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
--

--------------------------------------------------------------
-- Converting Core to STG Syntax
--------------------------------------------------------------

-- And, as we have the info in hand, we may convert some lets to
-- let-no-escapes.

module CoreToStg ( coreToStg, coreExprToStg ) where

#include "HsVersions.h"

import GhcPrelude

import CoreSyn
import CoreUtils        ( exprType, findDefault, isJoinBind )
import CoreArity        ( manifestArity )
import StgSyn

import Type
import RepType
import TyCon
import MkId             ( coercionTokenId )
import Id
import IdInfo
import DataCon
import CostCentre       ( noCCS )
import VarEnv
import Module
import Name             ( isExternalName, nameOccName )
import OccName          ( occNameFS )
import BasicTypes       ( Arity )
import TysWiredIn       ( unboxedUnitDataCon )
import Literal
import Outputable
import MonadUtils
import FastString
import Util
import DynFlags
import ForeignCall
import Demand           ( isUsedOnce )
import PrimOp           ( PrimCall(..) )
import UniqFM

import Data.Maybe    (isJust, fromMaybe)
import Control.Monad (liftM, ap)

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
-- See also: Commentary/Rts/Storage/GC/CAFs on the GHC Wiki.

-- Note [Collecting live CAF info]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- In this pass we also collect information on which CAFs are live.
--
-- A top-level Id has CafInfo, which is
--
--         - MayHaveCafRefs, if it may refer indirectly to
--           one or more CAFs, or
--         - NoCafRefs if it definitely doesn't
--
-- The CafInfo has already been calculated during the CoreTidy pass.
--
-- During CoreToStg, we then pin onto each binding and case expression, a
-- list of Ids which represents the "live" CAFs at that point.  The meaning
-- of "live" here is the same as for live variables, see above (which is
-- why it's convenient to collect CAF information here rather than elsewhere).
--
-- The later SRT pass takes these lists of Ids and uses them to construct
-- the actual nested SRTs, and replaces the lists of Ids with (offset,length)
-- pairs.

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

-- --------------------------------------------------------------
-- Setting variable info: top-level, binds, RHSs
-- --------------------------------------------------------------

coreToStg :: DynFlags -> Module -> CoreProgram -> [StgTopBinding]
coreToStg dflags this_mod pgm
  = pgm'
  where (_, _, pgm') = coreTopBindsToStg dflags this_mod emptyVarEnv pgm

coreExprToStg :: CoreExpr -> StgExpr
coreExprToStg expr
  = new_expr where (new_expr,_) = initCts emptyVarEnv (coreToStgExpr expr)


coreTopBindsToStg
    :: DynFlags
    -> Module
    -> IdEnv HowBound           -- environment for the bindings
    -> CoreProgram
    -> (IdEnv HowBound, FreeVarsInfo, [StgTopBinding])

coreTopBindsToStg _      _        env [] = (env, emptyFVInfo, [])
coreTopBindsToStg dflags this_mod env (b:bs)
  = (env2, fvs2, b':bs')
  where
        -- Notice the mutually-recursive "knot" here:
        --   env accumulates down the list of binds,
        --   fvs accumulates upwards
        (env1, fvs2, b' ) = coreTopBindToStg dflags this_mod env fvs1 b
        (env2, fvs1, bs') = coreTopBindsToStg dflags this_mod env1 bs

coreTopBindToStg
        :: DynFlags
        -> Module
        -> IdEnv HowBound
        -> FreeVarsInfo         -- Info about the body
        -> CoreBind
        -> (IdEnv HowBound, FreeVarsInfo, StgTopBinding)

coreTopBindToStg _ _ env body_fvs (NonRec id (Lit (MachStr str)))
  -- top-level string literal
  = let
        env' = extendVarEnv env id how_bound
        how_bound = LetBound TopLet 0
    in (env', body_fvs, StgTopStringLit id str)

coreTopBindToStg dflags this_mod env body_fvs (NonRec id rhs)
  = let
        env'      = extendVarEnv env id how_bound
        how_bound = LetBound TopLet $! manifestArity rhs

        (stg_rhs, fvs') =
            initCts env $ do
              (stg_rhs, fvs') <- coreToTopStgRhs dflags this_mod body_fvs (id,rhs)
              return (stg_rhs, fvs')

        bind = StgTopLifted $ StgNonRec id stg_rhs
    in
    ASSERT2(consistentCafInfo id bind, ppr id )
      -- NB: previously the assertion printed 'rhs' and 'bind'
      --     as well as 'id', but that led to a black hole
      --     where printing the assertion error tripped the
      --     assertion again!
    (env', fvs' `unionFVInfo` body_fvs, bind)

coreTopBindToStg dflags this_mod env body_fvs (Rec pairs)
  = ASSERT( not (null pairs) )
    let
        binders = map fst pairs

        extra_env' = [ (b, LetBound TopLet $! manifestArity rhs)
                     | (b, rhs) <- pairs ]
        env' = extendVarEnvList env extra_env'

        (stg_rhss, fvs')
          = initCts env' $ do
               (stg_rhss, fvss') <- mapAndUnzipM (coreToTopStgRhs dflags this_mod body_fvs) pairs
               let fvs' = unionFVInfos fvss'
               return (stg_rhss, fvs')

        bind = StgTopLifted $ StgRec (zip binders stg_rhss)
    in
    ASSERT2(consistentCafInfo (head binders) bind, ppr binders)
    (env', fvs' `unionFVInfo` body_fvs, bind)


-- Assertion helper: this checks that the CafInfo on the Id matches
-- what CoreToStg has figured out about the binding's SRT.  The
-- CafInfo will be exact in all cases except when CorePrep has
-- floated out a binding, in which case it will be approximate.
consistentCafInfo :: Id -> GenStgTopBinding Var Id -> Bool
consistentCafInfo id bind
  = WARN( not (exact || is_sat_thing) , ppr id <+> ppr id_marked_caffy <+> ppr binding_is_caffy )
    safe
  where
    safe  = id_marked_caffy || not binding_is_caffy
    exact = id_marked_caffy == binding_is_caffy
    id_marked_caffy  = mayHaveCafRefs (idCafInfo id)
    binding_is_caffy = topStgBindHasCafRefs bind
    is_sat_thing = occNameFS (nameOccName (idName id)) == fsLit "sat"

coreToTopStgRhs
        :: DynFlags
        -> Module
        -> FreeVarsInfo         -- Free var info for the scope of the binding
        -> (Id,CoreExpr)
        -> CtsM (StgRhs, FreeVarsInfo)

coreToTopStgRhs dflags this_mod scope_fv_info (bndr, rhs)
  = do { (new_rhs, rhs_fvs) <- coreToStgExpr rhs

       ; let stg_rhs   = mkTopStgRhs dflags this_mod rhs_fvs bndr bndr_info new_rhs
             stg_arity = stgRhsArity stg_rhs
       ; return (ASSERT2( arity_ok stg_arity, mk_arity_msg stg_arity) stg_rhs,
                 rhs_fvs) }
  where
    bndr_info = lookupFVInfo scope_fv_info bndr

        -- It's vital that the arity on a top-level Id matches
        -- the arity of the generated STG binding, else an importing
        -- module will use the wrong calling convention
        --      (Trac #2844 was an example where this happened)
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

mkTopStgRhs :: DynFlags -> Module -> FreeVarsInfo
            -> Id -> StgBinderInfo -> StgExpr
            -> StgRhs

mkTopStgRhs dflags this_mod = mkStgRhs' con_updateable
        -- Dynamic StgConApps are updatable
  where con_updateable con args = isDllConApp dflags this_mod con args

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

coreToStgExpr
        :: CoreExpr
        -> CtsM (StgExpr,       -- Decorated STG expr
                 FreeVarsInfo)  -- Its free vars (NB free, not live)

-- The second and third components can be derived in a simple bottom up pass, not
-- dependent on any decisions about which variables will be let-no-escaped or
-- not.  The first component, that is, the decorated expression, may then depend
-- on these components, but it in turn is not scrutinised as the basis for any
-- decisions.  Hence no black holes.

-- No LitInteger's should be left by the time this is called. CorePrep
-- should have converted them all to a real core representation.
coreToStgExpr (Lit (LitInteger {})) = panic "coreToStgExpr: LitInteger"
coreToStgExpr (Lit l)      = return (StgLit l, emptyFVInfo)
coreToStgExpr (Var v)      = coreToStgApp Nothing v               [] []
coreToStgExpr (Coercion _) = coreToStgApp Nothing coercionTokenId [] []

coreToStgExpr expr@(App _ _)
  = coreToStgApp Nothing f args ticks
  where
    (f, args, ticks) = myCollectArgs expr

coreToStgExpr expr@(Lam _ _)
  = let
        (args, body) = myCollectBinders expr
        args'        = filterStgBinders args
    in
    extendVarEnvCts [ (a, LambdaBound) | a <- args' ] $ do
    (body, body_fvs) <- coreToStgExpr body
    let
        fvs             = args' `minusFVBinders` body_fvs
        result_expr | null args' = body
                    | otherwise  = StgLam args' body

    return (result_expr, fvs)

coreToStgExpr (Tick tick expr)
  = do case tick of
         HpcTick{}    -> return ()
         ProfNote{}   -> return ()
         SourceNote{} -> return ()
         Breakpoint{} -> panic "coreToStgExpr: breakpoint should not happen"
       (expr2, fvs) <- coreToStgExpr expr
       return (StgTick tick expr2, fvs)

coreToStgExpr (Cast expr _)
  = coreToStgExpr expr

-- Cases require a little more real work.

coreToStgExpr (Case scrut _ _ [])
  = coreToStgExpr scrut
    -- See Note [Empty case alternatives] in CoreSyn If the case
    -- alternatives are empty, the scrutinee must diverge or raise an
    -- exception, so we can just dive into it.
    --
    -- Of course this may seg-fault if the scrutinee *does* return.  A
    -- belt-and-braces approach would be to move this case into the
    -- code generator, and put a return point anyway that calls a
    -- runtime system error function.


coreToStgExpr (Case scrut bndr _ alts) = do
    (alts2, alts_fvs)
       <- extendVarEnvCts [(bndr, LambdaBound)] $ do
            (alts2, fvs_s) <- mapAndUnzipM vars_alt alts
            return ( alts2,
                     unionFVInfos fvs_s )
    let
        -- Determine whether the default binder is dead or not
        -- This helps the code generator to avoid generating an assignment
        -- for the case binder (is extremely rare cases) ToDo: remove.
        bndr' | bndr `elementOfFVInfo` alts_fvs = bndr
              | otherwise                       = bndr `setIdOccInfo` IAmDead

        -- Don't consider the default binder as being 'live in alts',
        -- since this is from the point of view of the case expr, where
        -- the default binder is not free.
        alts_fvs_wo_bndr  = bndr `minusFVBinder` alts_fvs

        -- We tell the scrutinee that everything
        -- live in the alts is live in it, too.
    (scrut2, scrut_fvs) <- coreToStgExpr scrut

    return (
      StgCase scrut2 bndr' (mkStgAltType bndr alts) alts2,
      scrut_fvs `unionFVInfo` alts_fvs_wo_bndr
      )
  where
    vars_alt (con, binders, rhs)
      | DataAlt c <- con, c == unboxedUnitDataCon
      = -- This case is a bit smelly.
        -- See Note [Nullary unboxed tuple] in Type.hs
        -- where a nullary tuple is mapped to (State# World#)
        ASSERT( null binders )
        do { (rhs2, rhs_fvs) <- coreToStgExpr rhs
           ; return ((DEFAULT, [], rhs2), rhs_fvs) }
      | otherwise
      = let     -- Remove type variables
            binders' = filterStgBinders binders
        in
        extendVarEnvCts [(b, LambdaBound) | b <- binders'] $ do
        (rhs2, rhs_fvs) <- coreToStgExpr rhs
        return ( (con, binders', rhs2),
                 binders' `minusFVBinders` rhs_fvs )

coreToStgExpr (Let bind body) = do
    coreToStgLet bind body

coreToStgExpr e = pprPanic "coreToStgExpr" (ppr e)

mkStgAltType :: Id -> [CoreAlt] -> AltType
mkStgAltType bndr alts
  | isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty
  = MultiValAlt (length prim_reps)  -- always use MultiValAlt for unboxed tuples

  | otherwise
  = case prim_reps of
      [LiftedRep] -> case tyConAppTyCon_maybe (unwrapType bndr_ty) of
        Just tc
          | isAbstractTyCon tc -> look_for_better_tycon
          | isAlgTyCon tc      -> AlgAlt tc
          | otherwise          -> ASSERT2( _is_poly_alt_tycon tc, ppr tc )
                                  PolyAlt
        Nothing                -> PolyAlt
      [unlifted] -> PrimAlt unlifted
      not_unary  -> MultiValAlt (length not_unary)
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
        | ((DataAlt con, _, _) : _) <- data_alts =
                AlgAlt (dataConTyCon con)
        | otherwise =
                ASSERT(null data_alts)
                PolyAlt
        where
                (data_alts, _deflt) = findDefault alts

-- ---------------------------------------------------------------------------
-- Applications
-- ---------------------------------------------------------------------------

coreToStgApp
         :: Maybe UpdateFlag            -- Just upd <=> this application is
                                        -- the rhs of a thunk binding
                                        --      x = [...] \upd [] -> the_app
                                        -- with specified update flag
        -> Id                           -- Function
        -> [CoreArg]                    -- Arguments
        -> [Tickish Id]                 -- Debug ticks
        -> CtsM (StgExpr, FreeVarsInfo)


coreToStgApp _ f args ticks = do
    (args', args_fvs, ticks') <- coreToStgArgs args
    how_bound <- lookupVarCts f

    let
        n_val_args       = valArgCount args
        not_letrec_bound = not (isLetBound how_bound)
        fun_fvs = singletonFVInfo f how_bound fun_occ
            -- e.g. (f :: a -> int) (x :: a)
            -- Here the free variables are "f", "x" AND the type variable "a"
            -- coreToStgArgs will deal with the arguments recursively

        -- Mostly, the arity info of a function is in the fn's IdInfo
        -- But new bindings introduced by CoreSat may not have no
        -- arity info; it would do us no good anyway.  For example:
        --      let f = \ab -> e in f
        -- No point in having correct arity info for f!
        -- Hence the hasArity stuff below.
        -- NB: f_arity is only consulted for LetBound things
        f_arity   = stgArity f how_bound
        saturated = f_arity <= n_val_args

        fun_occ
         | not_letrec_bound         = noBinderInfo      -- Uninteresting variable
         | f_arity > 0 && saturated = stgSatOcc -- Saturated or over-saturated function call
         | otherwise                = stgUnsatOcc       -- Unsaturated function or thunk

        res_ty = exprType (mkApps (Var f) args)
        app = case idDetails f of
                DataConWorkId dc
                  | saturated    -> StgConApp dc args'
                                      (dropRuntimeRepArgs (fromMaybe [] (tyConAppArgs_maybe res_ty)))

                -- Some primitive operator that might be implemented as a library call.
                PrimOpId op      -> ASSERT( saturated )
                                    StgOpApp (StgPrimOp op) args' res_ty

                -- A call to some primitive Cmm function.
                FCallId (CCall (CCallSpec (StaticTarget _ lbl (Just pkgId) True)
                                          PrimCallConv _))
                                 -> ASSERT( saturated )
                                    StgOpApp (StgPrimCallOp (PrimCall lbl pkgId)) args' res_ty

                -- A regular foreign call.
                FCallId call     -> ASSERT( saturated )
                                    StgOpApp (StgFCallOp call (idUnique f)) args' res_ty

                TickBoxOpId {}   -> pprPanic "coreToStg TickBox" $ ppr (f,args')
                _other           -> StgApp f args'
        fvs = fun_fvs  `unionFVInfo` args_fvs

        tapp = foldr StgTick app (ticks ++ ticks')

    -- Forcing these fixes a leak in the code generator, noticed while
    -- profiling for trac #4367
    app `seq` fvs `seq` return (
        tapp,
        fvs
     )



-- ---------------------------------------------------------------------------
-- Argument lists
-- This is the guy that turns applications into A-normal form
-- ---------------------------------------------------------------------------

coreToStgArgs :: [CoreArg] -> CtsM ([StgArg], FreeVarsInfo, [Tickish Id])
coreToStgArgs []
  = return ([], emptyFVInfo, [])

coreToStgArgs (Type _ : args) = do     -- Type argument
    (args', fvs, ts) <- coreToStgArgs args
    return (args', fvs, ts)

coreToStgArgs (Coercion _ : args)  -- Coercion argument; replace with place holder
  = do { (args', fvs, ts) <- coreToStgArgs args
       ; return (StgVarArg coercionTokenId : args', fvs, ts) }

coreToStgArgs (Tick t e : args)
  = ASSERT( not (tickishIsCode t) )
    do { (args', fvs, ts) <- coreToStgArgs (e : args)
       ; return (args', fvs, t:ts) }

coreToStgArgs (arg : args) = do         -- Non-type argument
    (stg_args, args_fvs, ticks) <- coreToStgArgs args
    (arg', arg_fvs) <- coreToStgExpr arg
    let
        fvs = args_fvs `unionFVInfo` arg_fvs

        (aticks, arg'') = stripStgTicksTop tickishFloatable arg'
        stg_arg = case arg'' of
                       StgApp v []        -> StgVarArg v
                       StgConApp con [] _ -> StgVarArg (dataConWorkId con)
                       StgLit lit         -> StgLitArg lit
                       _                  -> pprPanic "coreToStgArgs" (ppr arg)

        -- WARNING: what if we have an argument like (v `cast` co)
        --          where 'co' changes the representation type?
        --          (This really only happens if co is unsafe.)
        -- Then all the getArgAmode stuff in CgBindery will set the
        -- cg_rep of the CgIdInfo based on the type of v, rather
        -- than the type of 'co'.
        -- This matters particularly when the function is a primop
        -- or foreign call.
        -- Wanted: a better solution than this hacky warning
    let
        arg_ty = exprType arg
        stg_arg_ty = stgArgType stg_arg
        bad_args = (isUnliftedType arg_ty && not (isUnliftedType stg_arg_ty))
                || (typePrimRep arg_ty /= typePrimRep stg_arg_ty)
        -- In GHCi we coerce an argument of type BCO# (unlifted) to HValue (lifted),
        -- and pass it to a function expecting an HValue (arg_ty).  This is ok because
        -- we can treat an unlifted value as lifted.  But the other way round
        -- we complain.
        -- We also want to check if a pointer is cast to a non-ptr etc

    WARN( bad_args, text "Dangerous-looking argument. Probable cause: bad unsafeCoerce#" $$ ppr arg )
     return (stg_arg : stg_args, fvs, ticks ++ aticks)


-- ---------------------------------------------------------------------------
-- The magic for lets:
-- ---------------------------------------------------------------------------

coreToStgLet
         :: CoreBind    -- bindings
         -> CoreExpr    -- body
         -> CtsM (StgExpr,      -- new let
                  FreeVarsInfo) -- variables free in the whole let

coreToStgLet bind body = do
    (bind2, bind_fvs,
     body2, body_fvs)
       <- mfix $ \ ~(_, _, _, rec_body_fvs) -> do

          ( bind2, bind_fvs, env_ext)
                <- vars_bind rec_body_fvs bind

          -- Do the body
          extendVarEnvCts env_ext $ do
             (body2, body_fvs) <- coreToStgExpr body

             return (bind2, bind_fvs,
                     body2, body_fvs)


        -- Compute the new let-expression
    let
        new_let | isJoinBind bind = StgLetNoEscape bind2 body2
                | otherwise       = StgLet bind2 body2

        free_in_whole_let
          = binders `minusFVBinders` (bind_fvs `unionFVInfo` body_fvs)

    return (
        new_let,
        free_in_whole_let
      )
  where
    binders        = bindersOf bind

    mk_binding binder rhs
        = (binder, LetBound NestedLet (manifestArity rhs))

    vars_bind :: FreeVarsInfo           -- Free var info for body of binding
              -> CoreBind
              -> CtsM (StgBinding,
                       FreeVarsInfo,
                       [(Id, HowBound)])  -- extension to environment


    vars_bind body_fvs (NonRec binder rhs) = do
        (rhs2, bind_fvs) <- coreToStgRhs body_fvs (binder,rhs)
        let
            env_ext_item = mk_binding binder rhs

        return (StgNonRec binder rhs2,
                bind_fvs, [env_ext_item])


    vars_bind body_fvs (Rec pairs)
      = mfix $ \ ~(_, rec_rhs_fvs, _) ->
           let
                rec_scope_fvs = unionFVInfo body_fvs rec_rhs_fvs
                binders = map fst pairs
                env_ext = [ mk_binding b rhs
                          | (b,rhs) <- pairs ]
           in
           extendVarEnvCts env_ext $ do
              (rhss2, fvss)
                     <- mapAndUnzipM (coreToStgRhs rec_scope_fvs) pairs
              let
                        bind_fvs = unionFVInfos fvss

              return (StgRec (binders `zip` rhss2),
                      bind_fvs, env_ext)

coreToStgRhs :: FreeVarsInfo      -- Free var info for the scope of the binding
             -> (Id,CoreExpr)
             -> CtsM (StgRhs, FreeVarsInfo)

coreToStgRhs scope_fv_info (bndr, rhs) = do
    (new_rhs, rhs_fvs) <- coreToStgExpr rhs
    return (mkStgRhs rhs_fvs bndr bndr_info new_rhs, rhs_fvs)
  where
    bndr_info = lookupFVInfo scope_fv_info bndr

mkStgRhs :: FreeVarsInfo -> Id -> StgBinderInfo -> StgExpr -> StgRhs
mkStgRhs = mkStgRhs' con_updateable
  where con_updateable _ _ = False

mkStgRhs' :: (DataCon -> [StgArg] -> Bool)
            -> FreeVarsInfo -> Id -> StgBinderInfo -> StgExpr -> StgRhs
mkStgRhs' con_updateable rhs_fvs bndr binder_info rhs
  | StgLam bndrs body <- rhs
  = StgRhsClosure noCCS binder_info
                   (getFVs rhs_fvs)
                   ReEntrant
                   bndrs body
  | isJoinId bndr -- must be nullary join point
  = ASSERT(idJoinArity bndr == 0)
    StgRhsClosure noCCS binder_info
                   (getFVs rhs_fvs)
                   ReEntrant -- ignored for LNE
                   [] rhs
  | StgConApp con args _ <- unticked_rhs
  , not (con_updateable con args)
  = -- CorePrep does this right, but just to make sure
    ASSERT2( not (isUnboxedTupleCon con || isUnboxedSumCon con)
           , ppr bndr $$ ppr con $$ ppr args)
    StgRhsCon noCCS con args
  | otherwise
  = StgRhsClosure noCCS binder_info
                   (getFVs rhs_fvs)
                   upd_flag [] rhs
 where

    (_, unticked_rhs) = stripStgTicksTop (not . tickishIsCode) rhs

    upd_flag | isUsedOnce (idDemandInfo bndr) = SingleEntry
             | otherwise                      = Updatable

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
    { unCtsM :: IdEnv HowBound
             -> a
    }

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

isLetBound :: HowBound -> Bool
isLetBound (LetBound _ _) = True
isLetBound _              = False

topLevelBound :: HowBound -> Bool
topLevelBound ImportBound         = True
topLevelBound (LetBound TopLet _) = True
topLevelBound _                   = False

-- For a let(rec)-bound variable, x, we record LiveInfo, the set of
-- variables that are live if x is live.  This LiveInfo comprises
--         (a) dynamic live variables (ones with a non-top-level binding)
--         (b) static live variabes (CAFs or things that refer to CAFs)
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

initCts :: IdEnv HowBound -> CtsM a -> a
initCts env m = unCtsM m env



{-# INLINE thenCts #-}
{-# INLINE returnCts #-}

returnCts :: a -> CtsM a
returnCts e = CtsM $ \_ -> e

thenCts :: CtsM a -> (a -> CtsM b) -> CtsM b
thenCts m k = CtsM $ \env
  -> unCtsM (k (unCtsM m env)) env

instance Functor CtsM where
    fmap = liftM

instance Applicative CtsM where
    pure = returnCts
    (<*>) = ap

instance Monad CtsM where
    (>>=)  = thenCts

instance MonadFix CtsM where
    mfix expr = CtsM $ \env ->
                       let result = unCtsM (expr result) env
                       in  result

-- Functions specific to this monad:

extendVarEnvCts :: [(Id, HowBound)] -> CtsM a -> CtsM a
extendVarEnvCts ids_w_howbound expr
   =    CtsM $   \env
   -> unCtsM expr (extendVarEnvList env ids_w_howbound)

lookupVarCts :: Id -> CtsM HowBound
lookupVarCts v = CtsM $ \env -> lookupBinding env v

lookupBinding :: IdEnv HowBound -> Id -> HowBound
lookupBinding env v = case lookupVarEnv env v of
                        Just xx -> xx
                        Nothing -> ASSERT2( isGlobalId v, ppr v ) ImportBound


-- ---------------------------------------------------------------------------
-- Free variable information
-- ---------------------------------------------------------------------------

type FreeVarsInfo = VarEnv (Var, HowBound, StgBinderInfo)
        -- The Var is so we can gather up the free variables
        -- as a set.
        --
        -- The HowBound info just saves repeated lookups;
        -- we look up just once when we encounter the occurrence.
        -- INVARIANT: Any ImportBound Ids are HaveCafRef Ids
        --            Imported Ids without CAF refs are simply
        --            not put in the FreeVarsInfo for an expression.
        --            See singletonFVInfo and freeVarsToLiveVars
        --
        -- StgBinderInfo records how it occurs; notably, we
        -- are interested in whether it only occurs in saturated
        -- applications, because then we don't need to build a
        -- curried version.
        -- If f is mapped to noBinderInfo, that means
        -- that f *is* mentioned (else it wouldn't be in the
        -- IdEnv at all), but perhaps in an unsaturated applications.
        --
        -- All case/lambda-bound things are also mapped to
        -- noBinderInfo, since we aren't interested in their
        -- occurrence info.
        --
        -- For ILX we track free var info for type variables too;
        -- hence VarEnv not IdEnv

emptyFVInfo :: FreeVarsInfo
emptyFVInfo = emptyVarEnv

singletonFVInfo :: Id -> HowBound -> StgBinderInfo -> FreeVarsInfo
-- Don't record non-CAF imports at all, to keep free-var sets small
singletonFVInfo id ImportBound info
   | mayHaveCafRefs (idCafInfo id) = unitVarEnv id (id, ImportBound, info)
   | otherwise                     = emptyVarEnv
singletonFVInfo id how_bound info  = unitVarEnv id (id, how_bound, info)

unionFVInfo :: FreeVarsInfo -> FreeVarsInfo -> FreeVarsInfo
unionFVInfo fv1 fv2 = plusVarEnv_C plusFVInfo fv1 fv2

unionFVInfos :: [FreeVarsInfo] -> FreeVarsInfo
unionFVInfos fvs = foldr unionFVInfo emptyFVInfo fvs

minusFVBinders :: [Id] -> FreeVarsInfo -> FreeVarsInfo
minusFVBinders vs fv = foldr minusFVBinder fv vs

minusFVBinder :: Id -> FreeVarsInfo -> FreeVarsInfo
minusFVBinder v fv = fv `delVarEnv` v
        -- When removing a binder, remember to add its type variables
        -- c.f. CoreFVs.delBinderFV

elementOfFVInfo :: Id -> FreeVarsInfo -> Bool
elementOfFVInfo id fvs = isJust (lookupVarEnv fvs id)

lookupFVInfo :: FreeVarsInfo -> Id -> StgBinderInfo
-- Find how the given Id is used.
-- Externally visible things may be used any old how
lookupFVInfo fvs id
  | isExternalName (idName id) = noBinderInfo
  | otherwise = case lookupVarEnv fvs id of
                        Nothing         -> noBinderInfo
                        Just (_,_,info) -> info

-- Non-top-level things only, both type variables and ids
getFVs :: FreeVarsInfo -> [Var]
getFVs fvs = [id | (id, how_bound, _) <- nonDetEltsUFM fvs,
  -- It's OK to use nonDetEltsUFM here because we're not aiming for
  -- bit-for-bit determinism.
  -- See Note [Unique Determinism and code generation]
                    not (topLevelBound how_bound) ]

plusFVInfo :: (Var, HowBound, StgBinderInfo)
           -> (Var, HowBound, StgBinderInfo)
           -> (Var, HowBound, StgBinderInfo)
plusFVInfo (id1,hb1,info1) (id2,hb2,info2)
  = ASSERT(id1 == id2 && hb1 == hb2)
    (id1, hb1, combineStgBinderInfo info1 info2)

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

myCollectArgs :: CoreExpr -> (Id, [CoreArg], [Tickish Id])
        -- We assume that we only have variables
        -- in the function position by now
myCollectArgs expr
  = go expr [] []
  where
    go (Var v)          as ts = (v, as, ts)
    go (App f a)        as ts = go f (a:as) ts
    go (Tick t e)       as ts = ASSERT( all isTypeArg as )
                                go e as (t:ts) -- ticks can appear in type apps
    go (Cast e _)       as ts = go e as ts
    go (Lam b e)        as ts
       | isTyVar b            = go e as ts -- Note [Collect args]
    go _                _  _  = pprPanic "CoreToStg.myCollectArgs" (ppr expr)

-- Note [Collect args]
-- ~~~~~~~~~~~~~~~~~~~
--
-- This big-lambda case occurred following a rather obscure eta expansion.
-- It all seems a bit yukky to me.

stgArity :: Id -> HowBound -> Arity
stgArity _ (LetBound _ arity) = arity
stgArity f ImportBound        = idArity f
stgArity _ LambdaBound        = 0
