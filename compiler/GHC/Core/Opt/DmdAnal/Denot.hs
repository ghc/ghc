{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Core.Opt.DmdAnal.Denot
   ( DmdAnalOpts(..)
   , denotProgram
   )
where

import GHC.Prelude

import GHC.Types.Demand
import GHC.Core.Opt.WorkWrap.Utils
import GHC.Core.Opt.DmdAnal.Framework
import GHC.Core
import GHC.Core.Multiplicity ( scaledThing )
import GHC.Utils.Outputable
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Core.DataCon
import GHC.Types.ForeignCall ( isSafeForeignCall )
import GHC.Types.Id
import GHC.Core.Utils
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.FVs      ( rulesRhsFreeIds, bndrRuleAndUnfoldingIdsList )
import GHC.Core.Coercion ( Coercion )
import GHC.Core.TyCo.FVs ( coVarsOfCos )
import GHC.Core.FamInstEnv
import GHC.Core.Opt.Arity ( typeArity )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Data.STuple
import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )

import Control.Monad
import Data.Foldable

import GHC.Utils.Trace
_ = pprTrace -- Tired of commenting out the import all the time

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

-- | Options for the demand analysis
data DmdAnalOpts = DmdAnalOpts
   { dmd_strict_dicts     :: !Bool     -- ^ Use strict dictionaries
   , dmd_unbox_width      :: !Int      -- ^ Product width until we will unbox function results eagerly
   , dmd_iter_check_prefs :: ![String] -- ^ Prefixes of binders we are want to know the iteration count of
   }

denotProgram :: DmdAnalOpts -> FamInstEnvs -> [CoreRule] -> CoreProgram -> Builder (() :~> DmdType)
denotProgram opts fam_envs rules binds
  = {-# SCC "denotProgram" #-} (\trans () -> trans topSubDmd) <$> go (emptyAnalEnv opts fam_envs) binds
  where
    -- See Note [Analysing top-level bindings]
    -- and Note [Why care for top-level demand annotations?]
    go :: AnalEnv -> [CoreBind] -> Builder (SubDemand :~> DmdType)
    go _   []     = return (\_ -> return nopDmdType)
    go env (b:bs) = denotBind TopLevel env b denot_body
      where
        denot_body :: AnalEnv -> Builder (SubDemand :~> DmdType)
        denot_body env' = do
          body_denot <- go env' bs
          -- See Note [Analysing top-level bindings]
          add_exported_uses env' (bindersOf b) body_denot

    -- | @add_exported_uses _ {e1,e2,..,ei,n1,n2,..,nj} dmd_ty@ adds to 'dmd_ty'
    -- the demand type expressed by using the exported 'Id's @{e1,..,ei}@ in
    -- unforeseen ('topDmd') ways. It's as if we returned a tuple @(e1,..,ei)@
    -- to the caller and we don't know how the caller uses that tuple.
    -- See Note [Analysing top-level bindings].
    add_exported_uses :: AnalEnv -> [Id] -> (SubDemand :~> DmdType) -> Builder (SubDemand :~> DmdType)
    add_exported_uses env ids body_denot = do
      !keep_alive <- keepAlive env (filter exported_or_rule_fv ids)
      return $ \eval_sd -> do
        keep_alive_dmds <- keep_alive ()
        dmd_ty <- body_denot eval_sd
        return $! dmd_ty `plusDmdType` keep_alive_dmds

    exported_or_rule_fv id = isExportedId id || id `elemVarSet` rule_fvs

    rule_fvs :: IdSet
    rule_fvs = rulesRhsFreeIds rules

keepAlive :: AnalEnv -> [Id] -> Builder (() :~> PlusDmdArg)
-- See Note [Absence analysis for stable unfoldings and RULES]
keepAlive _   []  = return $ const $ return $ mkPlusDmdArg emptyDmdEnv
keepAlive env ids = do
  -- pprTraceM "keepAlive" (ppr ids)
  trans_ids <- traverse (denotMultiExpr env . Var) ids
  return $ const $ foldr plusDmdArg (mkPlusDmdArg emptyDmdEnv) <$> traverse ($ topDmd) trans_ids

-- | We attach useful (e.g. not 'topDmd') 'idDemandInfo' to top-level bindings
-- that satisfy this function.
--
-- Basically, we want to know how top-level *functions* are *used*
-- (e.g. called). The information will always be lazy.
-- Any other top-level bindings are boring.
--
-- See also Note [Why care for top-level demand annotations?].
isInterestingTopLevelFn :: Id -> Bool
-- SG tried to set this to True and got a +2% ghc/alloc regression in T5642
-- (which is dominated by the Simplifier) at no gain in analysis precision.
-- If there was a gain, that regression might be acceptable.
-- Plus, we could use LetUp for thunks and share some code with local let
-- bindings.
isInterestingTopLevelFn id =
  typeArity (idType id) `lengthExceeds` 0

{- Note [Stamp out space leaks in demand analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand analysis pass outputs a new copy of the Core program in
which binders have been annotated with demand and strictness
information. It's tiresome to ensure that this information is fully
evaluated everywhere that we produce it, so we just run a single
seqBinds over the output before returning it, to ensure that there are
no references holding on to the input Core program.

This makes a ~30% reduction in peak memory usage when compiling
DynFlags (cf #9675 and #13426).

This is particularly important when we are doing late demand analysis,
since we don't do a seqBinds at any point thereafter. Hence code
generation would hold on to an extra copy of the Core program, via
unforced thunks in demand or strictness information; and it is the
most memory-intensive part of the compilation process, so this added
seqBinds makes a big difference in peak memory usage.

Note [Analysing top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a CoreProgram like
  e1 = ...
  n1 = ...
  e2 = \a b -> ... fst (n1 a b) ...
  n2 = \c d -> ... snd (e2 c d) ...
  ...
where e* are exported, but n* are not.
Intuitively, we can see that @n1@ is only ever called with two arguments
and in every call site, the first component of the result of the call
is evaluated. Thus, we'd like it to have idDemandInfo @LCL(CM(P(1L,A))@.
NB: We may *not* give e2 a similar annotation, because it is exported and
external callers might use it in arbitrary ways, expressed by 'topDmd'.
This can then be exploited by Nested CPR and eta-expansion,
see Note [Why care for top-level demand annotations?].

How do we get this result? Answer: By analysing the program as if it was a let
expression of this form:
  let e1 = ... in
  let n1 = ... in
  let e2 = ... in
  let n2 = ... in
  (e1,e2, ...)
E.g. putting all bindings in nested lets and returning all exported binders in a tuple.
Of course, we will not actually build that CoreExpr! Instead we faithfully
simulate analysis of said expression by adding the free variable 'DmdEnv'
of @e*@'s strictness signatures to the 'DmdType' we get from analysing the
nested bindings.

And even then the above form blows up analysis performance in T10370:
If @e1@ uses many free variables, we'll unnecessarily carry their demands around
with us from the moment we analyse the pair to the moment we bubble back up to
the binding for @e1@. So instead we analyse as if we had
  let e1 = ... in
  (e1, let n1 = ... in
  (    let e2 = ... in
  (e2, let n2 = ... in
  (    ...))))
That is, a series of right-nested pairs, where the @fst@ are the exported
binders of the last enclosing let binding and @snd@ continues the nested
lets.

Variables occurring free in RULE RHSs are to be handled the same as exported Ids.
See also Note [Absence analysis for stable unfoldings and RULES].

Note [Why care for top-level demand annotations?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Reading Note [Analysing top-level bindings], you might think that we go through
quite some trouble to get useful demands for top-level bindings. They can never
be strict, for example, so why bother?

First, we get to eta-expand top-level bindings that we weren't able to
eta-expand before without Call Arity. From T18894b:
  module T18894b (f) where
  eta :: Int -> Int -> Int
  eta x = if fst (expensive x) == 13 then \y -> ... else \y -> ...
  f m = ... eta m 2 ... eta 2 m ...
Since only @f@ is exported, we see all call sites of @eta@ and can eta-expand to
arity 2.

The call demands we get for some top-level bindings will also allow Nested CPR
to unbox deeper. From T18894:
  module T18894 (h) where
  g m n = (2 * m, 2 `div` n)
  {-# NOINLINE g #-}
  h :: Int -> Int
  h m = ... snd (g m 2) ... uncurry (+) (g 2 m) ...
Only @h@ is exported, hence we see that @g@ is always called in contexts were we
also force the division in the second component of the pair returned by @g@.
This allows Nested CPR to evaluate the division eagerly and return an I# in its
position.
-}

{-
************************************************************************
*                                                                      *
\subsection{The analyser itself}
*                                                                      *
************************************************************************
-}

-- | Analyse a binding group and its \"body\", e.g. where it is in scope.
--
-- It calls a function that knows how to analyse this \"body\" given
-- an 'AnalEnv' with updated demand signatures for the binding group
-- (reflecting their 'idDmdSigInfo') and expects to receive a
-- 'DmdType' in return, which it uses to annotate the binding group with their
-- 'idDemandInfo'.
denotBind
  :: TopLevelFlag
  -> AnalEnv
  -> CoreBind
  -> (AnalEnv -> Builder (SubDemand :~> DmdType)) -- NB: To build the body of the bind, we need the updated AnalEnv
  -> Builder (SubDemand :~> DmdType)
denotBind top_lvl env bind denot_body = case bind of
  NonRec id rhs
    | useLetUp top_lvl id
    -> denotBindLetUp           env id rhs denot_body
  _ -> denotBindLetDown top_lvl env bind   denot_body

-- | Annotates uninteresting top level functions ('isInterestingTopLevelFn')
-- with 'topDmd', the rest with the given demand.
writeBindIdDemand :: TopLevelFlag -> Id -> Demand -> EvalM ()
writeBindIdDemand top_lvl id = case top_lvl of
  TopLevel | not (isInterestingTopLevelFn id) -> \_   -> writeIdDemand id topDmd
  _                                           -> \dmd -> writeIdDemand id dmd

-- | Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- This function handles the up variant.
--
-- It is very simple. For  let x = rhs in body
--   * Demand-analyse 'body' in the current environment
--   * Find the demand, 'rhs_dmd' placed on 'x' by 'body'
--   * Demand-analyse 'rhs' in 'rhs_dmd'
--
-- This is used for a non-recursive local let without manifest lambdas (see
-- 'useLetUp').
--
-- This is the LetUp rule in the paper “Higher-Order Cardinality Analysis”.
denotBindLetUp :: AnalEnv
               -> Id
               -> CoreExpr
               -> (AnalEnv -> Builder (SubDemand :~> DmdType))
               -> Builder (SubDemand :~> DmdType)
denotBindLetUp env id rhs denot_body = do
  trans_body <- denot_body env -- NB: No sig for id
  trans_rhs  <- denotMultiExpr  env rhs
  -- TODO: We might want to `registerTransformer` here
  let !peel_id = findBndrDmd env NonRecursive id
  !keep_alive <- keepAlive env (bndrRuleAndUnfoldingIdsList id)
  let trans_bind eval_sd = do
        keep_alive_dmds <- keep_alive () -- so that we see the top usages as early as possible
        S2 body_ty id_dmd <- peel_id <$> trans_body eval_sd
        let !id_dmd' = finaliseBoxity (ae_fam_envs env) (ae_inl_fun env) (idType id) id_dmd
        writeIdDemand id id_dmd'
        rhs_ty <- trans_rhs id_dmd'
        return $! body_ty `plusDmdType` rhs_ty `plusDmdType` keep_alive_dmds
  return trans_bind

-- | Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- This function handles the down variant.
--
-- It computes a demand signature (by means of 'denotRhsSig') and uses
-- that at call sites in the body.
--
-- It is used for toplevel definitions, recursive definitions and local
-- non-recursive definitions that have manifest lambdas (cf. 'useLetUp').
-- Local non-recursive definitions without a lambda are handled with LetUp.
--
-- This is the LetDown rule in the paper “Higher-Order Cardinality Analysis”.
denotBindLetDown :: TopLevelFlag
                 -> AnalEnv
                 -> CoreBind
                 -> (AnalEnv -> Builder (SubDemand :~> DmdType))
                 -> Builder (SubDemand :~> DmdType)
denotBindLetDown top_lvl env bind denot_body = do
  let S2 rec_flag pairs = case bind of
           NonRec id rhs -> S2 NonRecursive [(id, rhs)]
           Rec ps        -> S2 Recursive    ps
  env' <- denotRhsSig top_lvl env rec_flag pairs
  trans_body <- denot_body env'
  traverse_ trackCall (bindersOf bind) -- because we read the lazy_fvs

  let !peel_ids = findBndrsDmds env' rec_flag (bindersOf bind)
  -- pprTraceM "bletdn" (ppr (map fst pairs) $$ ppr rec_flag)
  let trans_bind eval_sd = do
        body_ty <- trans_body eval_sd
        -- see Note [Lazy and unleashable free variables]
        S2 bind_ty id_dmds <- peel_ids . addLazyFVs body_ty <$> readLazyFVs (bindersOf bind)
        zipWithM_ (writeBindIdDemand top_lvl) (bindersOf bind) id_dmds
        return $! bind_ty
  return trans_bind
  -- If the actual demand is better than the vanilla call
  -- demand, you might think that we might do better to re-analyse
  -- the RHS with the stronger demand.
  -- But (a) That seldom happens, because it means that *every* path in
  --         the body of the let has to use that stronger demand
  -- (b) It often happens temporarily in when fixpointing, because
  --     the recursive function at first seems to place a massive demand.
  --     But we don't want to go to extra work when the function will
  --     probably iterate to something less demanding.
  -- In practice, all the times the actual demand on id2 is more than
  -- the vanilla call demand seem to be due to (b).  So we don't
  -- bother to re-analyse the RHS.


-- | Takes a Demand `n :* sd` and multiplies the result of `denotExpr` on `sd`
-- by `n`. The result is that we return the correct result for e.g. absent
-- demands. See ↦* relation in the Cardinality Analysis paper
denotMultiExpr :: AnalEnv
          -> CoreExpr
          -> Builder (Demand :~> PlusDmdArg)
-- The argument 'e' should satisfy the let/app invariant
denotMultiExpr env e = do
  massertPpr (not (isUnliftedType (exprType e)) || exprOkForSpeculation e) (ppr e)
  trans_e <- denotExpr env e
  -- If e is complicated enough to become a thunk, its contents will be
  -- evaluated at most once, so oneify its eval card.
  -- The bang is so that the arrow won't close over e
  let !oneify_thunk_card | exprIsTrivial e = id
                         | otherwise       = oneifyCard
  let trans_e_multi (n :* eval_sd) = do
        -- NB: (:*) expands AbsDmd and BotDmd as needed
        -- See Note [Analysing with absent demand] in GHC.Types.Demand
        dmd_ty <- trans_e eval_sd
        -- pprTraceM "star" (ppr n <+> ppr dmd_ty)
        return $! toPlusDmdArg $! multDmdType (oneify_thunk_card n) dmd_ty
  return trans_e_multi

nullaryCase :: DmdType -> Builder (SubDemand :~> DmdType)
nullaryCase !dmd_ty = return $ const (pure dmd_ty)

unaryCase :: AnalEnv -> CoreExpr -> (DmdType -> DmdType)
          -> Builder (SubDemand :~> DmdType)
unaryCase env e map_ty = do
  trans <- denotExpr env e
  return $! fmap map_ty . trans

-- Main Demand Analysis machinery
denotExpr
  :: AnalEnv
  -> CoreExpr
  -> Builder (SubDemand :~> DmdType)

-- denotExpr _ e | pprTrace "denotExpr" (ppr e) False = undefined

denotExpr env (Var var)   = denotVar env var -- important and complicated, so its own fun

denotExpr _ Lit{}         = nullaryCase nopDmdType
denotExpr _ Type{}        = nullaryCase nopDmdType -- Doesn't happen, in fact
denotExpr _ (Coercion co) = nullaryCase (unitDmdType (coercionDmdEnv co))

denotExpr env (Cast e co) = unaryCase env e (`plusDmdType` mkPlusDmdArg (coercionDmdEnv co))
denotExpr env (Tick _ e)  = unaryCase env e id

denotExpr env (App fun Type{}) = unaryCase env fun id
denotExpr env (App fun arg   ) = do
  -- This case handles value arguments (type args handled above)
  -- Crucially, coercions /are/ handled here, because they are
  -- value arguments (#10288)
  trans_fun <- denotExpr env fun
  trans_arg <- denotMultiExpr env arg
  return $ \eval_sd -> do
    let !call_sd = mkCalledOnceDmd eval_sd
    fun_ty <- trans_fun call_sd
    let !(arg_dmd, res_ty) = splitDmdTy fun_ty
    arg_ty <- trans_arg arg_dmd
    return $! res_ty `plusDmdType` arg_ty

denotExpr env (Lam var body) | isTyVar var = unaryCase env body id
denotExpr env (Lam id  body) | otherwise   = do
  trans_body <- denotExpr env body
  let !peel_id = findBndrDmd env NonRecursive id
  return $ \eval_sd -> do
    let !(n, body_sd) = peelCallDmd eval_sd
    S2 body_ty id_dmd <- peel_id <$> trans_body body_sd
    -- See Note [Finalising boxity for demand signature] in "GHC.Core.Opt.WorkWrap.Utils"
    -- and Note [Do not unbox class dictionaries]
    let !id_dmd' = finaliseBoxity (ae_fam_envs env) (ae_inl_fun env) (idType id) id_dmd
    writeIdDemand id id_dmd'
    let !lam_ty = addDemand id_dmd' body_ty
    return $! multDmdType n lam_ty

denotExpr env (Let bind body) = do
  let denot_body env' = denotExpr env' body
  denotBind NotTopLevel env bind denot_body

denotExpr env (Case scrut case_bndr _ty alts) = do
  trans_scrut <- denotExpr env scrut
  trans_alts  <- denotAlts env case_bndr alts
  let !is_single_data_alt
        | [Alt (DataAlt dc) _ _] <- alts = isJust $ tyConSingleAlgDataCon_maybe $ dataConTyCon dc
        | otherwise                      = False
  let !may_throw_precise = exprMayThrowPreciseException (ae_fam_envs env) scrut
  return $ \eval_sd -> do
    S3 scrut_sd case_bndr_dmd alt_ty <- trans_alts eval_sd
    writeIdDemand case_bndr case_bndr_dmd
    -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
    let !alt_ty' | may_throw_precise = deferAfterPreciseException alt_ty
                 | otherwise         = alt_ty
    let !scrut_sd'
          | is_single_data_alt = scrut_sd
          | Prod{} <- scrut_sd = topSubDmd -- Prod doesn't make sense for non-single data alt types
          | otherwise          = scrut_sd
    scrut_ty <- trans_scrut scrut_sd'
    return $! alt_ty' `plusDmdType` toPlusDmdArg scrut_ty

denotAlts :: AnalEnv -> Id -> [Alt Var] -> Builder (SubDemand :~> STriple SubDemand Demand DmdType)
denotAlts env case_bndr alts = do
  trans_altss <- traverse (denotAlt env case_bndr) alts
  -- NB: Base case is bot*, for empty case alternatives. This is a neutral
  -- element for lub*, and the right result when there really are no
  -- alternatives.
  return $ \eval_sd -> foldrM (do_one eval_sd) (S3 botSubDmd botDmd botDmdType) trans_altss
    where
      do_one eval_sd trans_alt (S3 scrut_sd2 case_bndr_dmd2 alt_ty2) = do
        S3 scrut_sd1 case_bndr_dmd1 alt_ty1 <- trans_alt eval_sd
        return $! S3 (scrut_sd1 `lubSubDmd` scrut_sd2)
                     (case_bndr_dmd1 `lubDmd` case_bndr_dmd2)
                     (alt_ty1 `lubDmdType` alt_ty2)

denotAlt :: AnalEnv -> Id -> Alt Var -> Builder (SubDemand :~> STriple SubDemand Demand DmdType)
denotAlt env case_bndr (Alt _ fld_bndrs rhs) = do
  trans_rhs <- denotExpr env rhs
  let !peel_bndrs     = findBndrsDmds env NonRecursive fld_bndrs
  let !peel_case_bndr = findBndrDmd env NonRecursive case_bndr
  return $ \eval_sd -> do
    rhs_ty <- trans_rhs eval_sd
    let !(S2 alt_ty  fld_dmds)      = peel_bndrs rhs_ty
    let !(S2 alt_ty' case_bndr_dmd) = peel_case_bndr alt_ty
    -- Evaluation cardinality on the case binder is irrelevant and a no-op.
    -- What matters is its nested sub-demand!
    -- NB: If case_bndr_dmd is absDmd, boxity will say Unboxed, which is
    -- what we want, because then `seq` will put a `seqDmd` on its scrut.
    let !(_ :* case_bndr_sd) = case_bndr_dmd
        -- See Note [Demand on the scrutinee of a product case]
        -- See Note [Demand on case-alternative binders]
        !(S2 scrut_sd fld_dmds') = addCaseBndrDmd case_bndr_sd fld_dmds
    writeBndrsDemands fld_bndrs fld_dmds'
    return $! S3 scrut_sd case_bndr_dmd alt_ty'

-- Precondition: If SubDemand is a Call, then there are no field demands
-- See Note [Demand on the scrutinee of a product case]
-- and Note [Demand on case-alternative binders]
addCaseBndrDmd :: SubDemand -- On the case binder
               -> [Demand]  -- On the fields of the constructor
               -> SPair SubDemand [Demand]
                            -- SubDemand on the case binder incl. field demands
                            -- and final demands for the components of the constructor
addCaseBndrDmd case_sd fld_dmds
  | Just (_, ds) <- viewProd (length fld_dmds) scrut_sd
  = S2 scrut_sd ds
  | Just _ <- viewCall case_sd
  = assertPpr (null fld_dmds) -- See the Precondition
              (text "Field demands on call sub-demand of case binder" $$ ppr case_sd $$ ppr fld_dmds)
              (S2 case_sd [])
  | otherwise
  = pprPanic "neither viewable as Call or as Prod" (ppr case_sd $$ ppr fld_dmds)
  where
    scrut_sd = case_sd `plusSubDmd` mkProd Unboxed fld_dmds

-- | A simple, syntactic analysis of whether an expression MAY throw a precise
-- exception when evaluated. It's always sound to return 'True'.
-- See Note [Which scrutinees may throw precise exceptions].
exprMayThrowPreciseException :: FamInstEnvs -> CoreExpr -> Bool
exprMayThrowPreciseException envs e
  | not (forcesRealWorld envs (exprType e))
  = False -- 1. in the Note
  | (Var f, _) <- collectArgs e
  , Just op    <- isPrimOpId_maybe f
  , op /= RaiseIOOp
  = False -- 2. in the Note
  | (Var f, _) <- collectArgs e
  , Just fcall <- isFCallId_maybe f
  , not (isSafeForeignCall fcall)
  = False -- 3. in the Note
  | otherwise
  = True  -- _. in the Note

-- | Recognises types that are
--    * @State# RealWorld@
--    * Unboxed tuples with a @State# RealWorld@ field
-- modulo coercions. This will detect 'IO' actions (even post Nested CPR! See
-- T13380e) and user-written variants thereof by their type.
forcesRealWorld :: FamInstEnvs -> Type -> Bool
forcesRealWorld fam_envs ty
  | ty `eqType` realWorldStatePrimTy
  = True
  | Just (tc, tc_args, _co)  <- normSplitTyConApp_maybe fam_envs ty
  , isUnboxedTupleTyCon tc
  , let field_tys = dataConInstArgTys (tyConSingleDataCon tc) tc_args
  = any (eqType realWorldStatePrimTy . scaledThing) field_tys
  | otherwise
  = False

{-
Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand A.  The "A" means
"absent", so this expression will never be needed. What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by multDmdType.

* Nevertheless, which sub-demand should we pick for analysis?
  Since the demand was absent, any would do. Worker/wrapper will replace
  absent bindings with an absent filler anyway, so annotations in the RHS
  of an absent binding don't matter much.
  Picking 'botSubDmd' would be the most useful, but would also look a bit
  misleading in the Core output of DmdAnal, because all nested annotations would
  be bottoming. Better pick 'seqSubDmd', so that we annotate many of those
  nested bindings with A themselves.

* In a previous incarnation of GHC we needed to be extra careful in the
  case of an *unlifted type*, because unlifted values are evaluated
  even if they are not used.  Example (see #9254):
     f :: (() -> (# Int#, () #)) -> ()
          -- Strictness signature is
          --    <CS(S(A,SU))>
          -- I.e. calls k, but discards first component of result
     f k = case k () of (# _, r #) -> r

     g :: Int -> ()
     g y = f (\n -> (# case y of I# y2 -> y2, n #))

  Here f's strictness signature says (correctly) that it calls its
  argument function and ignores the first component of its result.
  This is correct in the sense that it'd be fine to (say) modify the
  function so that always returned 0# in the first component.

  But in function g, we *will* evaluate the 'case y of ...', because
  it has type Int#.  So 'y' will be evaluated.  So we must record this
  usage of 'y', else 'g' will say 'y' is absent, and will w/w so that
  'y' is bound to an aBSENT_ERROR thunk.

  However, the argument of toSubDmd always satisfies the let/app
  invariant; so if it is unlifted it is also okForSpeculation, and so
  can be evaluated in a short finite time -- and that rules out nasty
  cases like the one above.  (I'm not quite sure why this was a
  problem in an earlier version of GHC, but it isn't now.)

Note [Always analyse in virgin pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tricky point: make sure that we analyse in the 'virgin' pass. Consider
   rec { f acc x True  = f (...rec { g y = ...g... }...)
         f acc x False = acc }
In the virgin pass for 'f' we'll give 'f' a very strict (bottom) type.
That might mean that we analyse the sub-expression containing the
E = "...rec g..." stuff in a bottom demand.  Suppose we *didn't analyse*
E, but just returned botType.

Then in the *next* (non-virgin) iteration for 'f', we might analyse E
in a weaker demand, and that will trigger doing a fixpoint iteration
for g.  But *because it's not the virgin pass* we won't start g's
iteration at bottom.  Disaster.  (This happened in $sfibToList' of
nofib/spectral/fibheaps.)

So in the virgin pass we make sure that we do analyse the expression
at least once, to initialise its signatures.

Note [Which scrutinees may throw precise exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is the specification of 'exprMayThrowPreciseExceptions',
which is important for Scenario 2 of
Note [Precise exceptions and strictness analysis] in GHC.Types.Demand.

For an expression @f a1 ... an :: ty@ we determine that
  1. False  If ty is *not* @State# RealWorld@ or an unboxed tuple thereof.
            This check is done by 'forcesRealWorld'.
            (Why not simply unboxed pairs as above? This is motivated by
            T13380{d,e}.)
  2. False  If f is a PrimOp, and it is *not* raiseIO#
  3. False  If f is an unsafe FFI call ('PlayRisky')
  _. True   Otherwise "give up".

It is sound to return False in those cases, because
  1. We don't give any guarantees for unsafePerformIO, so no precise exceptions
     from pure code.
  2. raiseIO# is the only primop that may throw a precise exception.
  3. Unsafe FFI calls may not interact with the RTS (to throw, for example).
     See haddock on GHC.Types.ForeignCall.PlayRisky.

We *need* to return False in those cases, because
  1. We would lose too much strictness in pure code, all over the place.
  2. We would lose strictness for primops like getMaskingState#, which
     introduces a substantial regression in
     GHC.IO.Handle.Internals.wantReadableHandle.
  3. We would lose strictness for code like GHC.Fingerprint.fingerprintData,
     where an intermittent FFI call to c_MD5Init would otherwise lose
     strictness on the arguments len and buf, leading to regressions in T9203
     (2%) and i386's haddock.base (5%). Tested by T13380f.

In !3014 we tried a more sophisticated analysis by introducing ConOrDiv (nic)
to the Divergence lattice, but in practice it turned out to be hard to untaint
from 'topDiv' to 'conDiv', leading to bugs, performance regressions and
complexity that didn't justify the single fixed testcase T13380c.

Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders] in GHC.Types.Demand.
This is crucial. Example:
   f x = case x of y { (a,b) -> k y a }
If we just take scrut_demand = 1P(L,A), then we won't pass x to the
worker, so the worker will rebuild
     x = (a, absent-error)
and that'll crash.

Note [Demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to #10148.

Example. Source code:
  f x@(p,_) = if p then foo x else True

  foo (p,True) = True
  foo (p,q)    = foo (q,p)

After strictness analysis, forgetting (b):
  f = \ (x_an1 [Dmd=1P(1L,ML)] :: (Bool, Bool)) ->
      case x_an1
      of wild_X7 [Dmd=MP(ML,ML)]
      { (p_an2 [Dmd=1L], ds_dnz [Dmd=A]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> foo wild_X7 }

Note that ds_dnz is syntactically dead, but the expression bound to it is
reachable through the case binder wild_X7. Now watch what happens if we inline
foo's wrapper:
  f = \ (x_an1 [Dmd=1P(1L,ML)] :: (Bool, Bool)) ->
      case x_an1
      of _ [Dmd=MP(ML,ML)]
      { (p_an2 [Dmd=1L], ds_dnz [Dmd=A]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> $wfoo_soq GHC.Types.True ds_dnz }

Look at that! ds_dnz has come back to life in the call to $wfoo_soq! A second
run of demand analysis would no longer infer ds_dnz to be absent.
But unlike occurrence analysis, which infers properties of the *syntactic*
shape of the program, the results of demand analysis dcDescribe expressions
*semantically* and are supposed to be mostly stable across Simplification.
That's why we should better account for (b).
In #10148, we ended up emitting a single-entry thunk instead of an updateable
thunk for a let binder that was an an absent case-alt binder during DmdAnal.

This is needed even for non-product types, in case the case-binder
is used but the components of the case alternative are not.

Note [Aggregated demand for cardinality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FIXME: This Note should be named [LetUp vs. LetDown] and probably predates
said separation. SG

We use different strategies for strictness and usage/cardinality to
"unleash" demands captured on free variables by bindings. Let us
consider the example:

f1 y = let {-# NOINLINE h #-}
           h = y
       in  (h, h)

We are interested in obtaining cardinality demand U1 on |y|, as it is
used only in a thunk, and, therefore, is not going to be updated any
more. Therefore, the demand on |y|, captured and unleashed by usage of
|h| is U1. However, if we unleash this demand every time |h| is used,
and then sum up the effects, the ultimate demand on |y| will be U1 +
U1 = U. In order to avoid it, we *first* collect the aggregate demand
on |h| in the body of let-expression, and only then apply the demand
transformer:

transf[x](U) = {y |-> U1}

so the resulting demand on |y| is U1.

The situation is, however, different for strictness, where this
aggregating approach exhibits worse results because of the nature of
|lub| operation for strictness. Consider the example:

f y c =
  let h x = y |seq| x
   in case of
        True  -> h True
        False -> y

It is clear that |f| is strict in |y|, however, the suggested analysis
will infer from the body of |let| that |h| is used lazily (as it is
used in one branch only), therefore lazy demand will be put on its
free variable |y|. Conversely, if the demand on |h| is unleashed right
on the spot, we will get the desired result, namely, that |f| is
strict in |y|.


************************************************************************
*                                                                      *
                    Demand transformer
*                                                                      *
************************************************************************
-}

liftPureTransformer :: (SubDemand -> DmdType) -> Builder (SubDemand :~> DmdType)
liftPureTransformer trans = return (\(!sd) -> pure (trans sd))

denotVar
  :: AnalEnv                 -- ^ The analysis environment
  -> Var                     -- ^ The variable
  -> Builder (SubDemand :~> DmdType)
                             -- ^ The evaluation context of the var mapped to
                             -- the demand type unleashed by the variable in this
                             -- context. The returned DmdEnv includes the demand on
                             -- this function plus demand on its free variables.
                             -- Wrapped inside a 'Builder' so that we see the
                             -- call dependencies on local functions

-- See Note [What are demand signatures?] in "GHC.Types.Demand"
denotVar env var
  -- Type variables are not tracked at all
  | isTyVar var
  = liftPureTransformer (const nopDmdType)
  -- Data constructors have bespoke transformers
  | isDataConWorkId var
  = liftPureTransformer (dmdTransformDataConSig (idArity var))
  -- Dictionary component selectors have bespoke transformers
  -- Used to be controlled by a flag.
  -- See #18429 for some perf measurements.
  | Just _ <- isClassOpId_maybe var
  = -- pprTrace "denotVar:DictSel" (ppr var $$ ppr (idDmdSig var) $$ ppr sd) $
    liftPureTransformer (dmdTransformDictSelSig (idDmdSig var))
  -- Imported functions have a strictness signature to turn into a transformer
  | isGlobalId var
  = liftPureTransformer (dmdTransformSig (idDmdSig var))
  -- Top-level or local let-bound thing for which we use LetDown ('useLetUp').
  -- In that case, we have a demand transformer to unleash in our AnalEnv.
  | Just !top_lvl <- lookupSigEnv env var = do
      -- pprTraceM "denotVar:LetDown" (vcat [ppr var, ppr (idUnique var), ppr top_lvl])
      trans_call <- denotCall env var
      return $ \eval_sd -> do
        dmd_ty <- trans_call eval_sd
        -- Now add the demand on `var` itself
        return $! case top_lvl of
          NotTopLevel -> plusVarDmd dmd_ty var (C_11 :* eval_sd)
          TopLevel
            | isInterestingTopLevelFn var
            -- Top-level things will be used multiple times or not at
            -- all anyway, hence the multDmd below: It means we don't
            -- have to track whether `var` is used strictly or at most
            -- once, because ultimately it never will.
            -> plusVarDmd dmd_ty var (C_0N `multDmd` (C_11 :* eval_sd))
            | otherwise
            -- don't bother tracking the demand on `var`; we annotate topDmd
            -- later in `writeBindIdDemand`
            -> dmd_ty
  -- Everything else:
  --   * Local let binders for which we use LetUp (cf. 'useLetUp')
  --   * Lambda binders
  --   * Case and constructor field binders
  | otherwise
  = -- pprTrace "denotVar:other" (vcat [ppr var, ppr (nonDetKeysUFM $ ae_sigs env)]) $
    liftPureTransformer (\sd -> unitDmdType $! unitVarEnv var $! (C_11 :* sd))

{- *********************************************************************
*                                                                      *
                      Binding right-hand sides
*                                                                      *
********************************************************************* -}

-- | @denotRhsSig@ analyses the given RHS to compute a demand signature
-- for the LetDown rule. It works as follows:
--
--  * assuming the weakest possible body sub-demand, L
--  * looking at the definition
--  * determining a strictness signature
--
-- Since it assumed a body sub-demand of L, the resulting signature is
-- applicable at any call site.
denotRhsSig
  :: TopLevelFlag
  -> AnalEnv
  -> RecFlag
  -> [(Id, CoreExpr)]
  -> Builder AnalEnv
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
-- See Note [NOINLINE and strictness]
denotRhsSig top_lvl env rec_flag pairs = fix_all env pairs
  where
    bind_ids = map fst pairs
    rec_ids
      | isRec rec_flag = mkUniqSet bind_ids
      | otherwise      = emptyUniqSet

    fix_all env []            = return env
    fix_all env ((id,rhs):bs) = registerTransformer id $ do
      !env_body <- fix_all (extendAnalEnv top_lvl env id) bs
      let !env_rhs | isRec rec_flag = env_body
                   | otherwise      = env   -- prevents shadowing errors

      let read_old_lazy_fvs | isRec rec_flag = readLazyFVs bind_ids
                            | otherwise      = pure emptyVarEnv

      -- adjustInlFun: See Note [Do not unbox class dictionaries]
      trans_rhs <- denotExpr (adjustInlFun id env_rhs) rhs
      !keep_alive <- keepAlive env_rhs (bndrRuleAndUnfoldingIdsList id)
      let !arity = idArity id

      -- Note that the transformer below makes no attempts to widen eval_sd
      -- whatsoever. That's done at call sites through 'denotCall'.
      let trans eval_sd = do
            -- See Note [Absence analysis for stable unfoldings and RULES]
            keep_alive_dmds <- keep_alive () -- so that we see the top usages as early as possible
            rhs_ty <- trans_rhs eval_sd
            let DmdType rhs_fvs rhs_args rhs_div = plusDmdType rhs_ty keep_alive_dmds
            let !rhs_fvs' = applyWhen (isTopLevel top_lvl) (multDmdEnv C_01)
                              -- discard confusing strictness info on top-level
                              -- vars, which must never end up strict anyway.
                          $ reuseBndrs rec_ids rhs_fvs
                              -- recursive binders will end up used many times
                              -- anyway and this speeds up fixed-point iteration
                              -- in many cases

            -- See Note [Lazy and unleashable free variables]
            -- and Note [Lazy free variables and monotonicity]
            lazy_fvs <- read_old_lazy_fvs
            let !lazy_fvs' = lazy_fvs `addWeakDmds` rhs_fvs'
                !sig_fvs   = rhs_fvs' `minusUFM` lazy_fvs'
            -- pprTraceM "lazy_fvs" (ppr id $$ ppr lazy_fvs $$ ppr rhs_fvs' $$ ppr lazy_fvs')
            writeLazyFVs bind_ids lazy_fvs'

            let !final_ty  = DmdType sig_fvs rhs_args rhs_div
            -- See Note [Demand signatures are computed for a threshold demand based on idArity]
            let !sig       = mkDmdSigForArity arity final_ty
            -- Write out the demand signature for this function
            -- NB: This assumes that we will persist the signature of the last
            -- analysis run over this function! Revisit if we ever maintain
            -- multiple approximations
            writeIdDmdSig id sig
            -- pprTraceM "sig" (ppr id <+> ppr sig <+> ppr lazy_fvs <+> ppr lazy_fvs')
            return final_ty

      return (S2 trans env_body)

denotCall :: AnalEnv -> Id -> Builder (SubDemand :~> DmdType)
denotCall env id = do
  let !arity = idArity id
  let !is_join_id = isJoinId id
  do_the_call <- trackCall id
  return $ \eval_sd -> do
    let S2 card body_sd = peelManyCalls arity eval_sd
    if isAbs card
      then return nopDmdType
      else do
        let !widened_body_sd
              | is_join_id
              -- See Note [Demand analysis for join points]
              -- See Note [Invariants on join points] invariant 2b, in GHC.Core
              --     rhs_arity matches the join arity of the join point
              = body_sd -- no widening at all
              | otherwise
              -- See Note [Unboxed demand on function bodies returning small products]
              = unboxedWhenSmall env id topSubDmd
        let !single_call_sd = mkCalledOnceDmds arity widened_body_sd
        dmd_ty <- do_the_call single_call_sd
        return $! multDmdType card dmd_ty

-- | @addWeakDmds lazy_fv rhs_fv@ lubs @lazy_fv@ with an entry @x->dmd@ from
-- @rhs_fv@ if either
--
--    * @x->dmd2@ is in @lazy_fv@. Then it lubs @dmd@ with @dmd2@ and puts that
--      in @lazy_fv@.
--    * @x@ is not in @lazy_fv@ and @dmd@ is a weak demand ('isWeakDmd').
--      Then it copies the entry to @lazy_fv@ (which amounts to pretending
--      there was an entry @x->A@ in @lazy_fv@).
--
-- See Note [Lazy and unleashable free variables]
-- and Note [Lazy free variables and monotonicity]
addWeakDmds :: DmdEnv -> DmdEnv -> DmdEnv
addWeakDmds = plusFilterUFM_C lubDmd isWeakDmd

unboxedWhenSmall :: AnalEnv -> Id -> SubDemand -> SubDemand
-- See Note [Unboxed demand on function bodies returning small products]
unboxedWhenSmall env id sd
  | Just n <- unboxableResultWidth env id
  , n <= dmd_unbox_width (ae_opts env)
  = unboxSubDemand sd
  | otherwise
  = sd

unboxableResultWidth :: AnalEnv -> Id -> Maybe Arity
unboxableResultWidth env id
  | (pis,ret_ty) <- splitPiTys (idType id)
  , count (not . isNamedBinder) pis == idArity id
  , Just (tc, _tc_args, _co) <- normSplitTyConApp_maybe (ae_fam_envs env) ret_ty
  , Just dc <- tyConSingleAlgDataCon_maybe tc
  , null (dataConExTyCoVars dc) -- Can't unbox results with existentials
  = Just (dataConRepArity dc)
  | otherwise
  = Nothing

-- | If given the (local, non-recursive) let-bound 'Id', 'useLetUp' determines
-- whether we should process the binding up (body before rhs) or down (rhs
-- before body).
--
-- We use LetDown if there is a chance to get a useful strictness signature to
-- unleash at call sites. LetDown is generally more precise than LetUp if we can
-- correctly guess how it will be used in the body, that is, for which incoming
-- demand the strictness signature should be computed, which allows us to
-- unleash higher-order demands on arguments at call sites. This is mostly the
-- case when
--
--   * The binding takes any arguments before performing meaningful work (cf.
--     'idArity'), in which case we are interested to see how it uses them.
--   * The binding is a join point, hence acting like a function, not a value.
--     As a big plus, we know *precisely* how it will be used in the body; since
--     it's always tail-called, we can directly unleash the incoming demand of
--     the let binding on its RHS when computing a strictness signature. See
--     [Demand analysis for join points].
--
-- Thus, if the binding is not a join point and its arity is 0, we have a thunk
-- and use LetUp, implying that we have no usable demand signature available
-- when we analyse the let body.
--
-- Since thunk evaluation is memoised, we want to unleash its 'DmdEnv' of free
-- vars at most once, regardless of how many times it was forced in the body.
-- This makes a real difference wrt. usage demands. The other reason is being
-- able to unleash a more precise product demand on its RHS once we know how the
-- thunk was used in the let body.
--
-- Characteristic examples, always assuming a single evaluation:
--
--   * @let x = 2*y in x + x@ => LetUp. Compared to LetDown, we find out that
--     the expression uses @y@ at most once.
--   * @let x = (a,b) in fst x@ => LetUp. Compared to LetDown, we find out that
--     @b@ is absent.
--   * @let f x = x*2 in f y@ => LetDown. Compared to LetUp, we find out that
--     the expression uses @y@ strictly, because we have @f@'s demand signature
--     available at the call site.
--   * @join exit = 2*y in if a then exit else if b then exit else 3*y@ =>
--     LetDown. Compared to LetUp, we find out that the expression uses @y@
--     strictly, because we can unleash @exit@'s signature at each call site.
--   * For a more convincing example with join points, see Note [Demand analysis
--     for join points].
--
useLetUp :: TopLevelFlag -> Var -> Bool
useLetUp top_lvl f = isNotTopLevel top_lvl && idArity f == 0 && not (isJoinId f)

{- Note [Demand analysis for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   g :: (Int,Int) -> Int
   g (p,q) = p+q

   f :: T -> Int -> Int
   f x p = g (join j y = (p,y)
              in case x of
                   A -> j 3
                   B -> j 4
                   C -> (p,7))

If j was a vanilla function definition, we'd analyse its body with
evalDmd, and think that it was lazy in p.  But for join points we can
do better!  We know that j's body will (if called at all) be evaluated
with the demand that consumes the entire join-binding, in this case
the argument demand from g.  Whizzo!  g evaluates both components of
its argument pair, so p will certainly be evaluated if j is called.

For f to be strict in p, we need /all/ paths to evaluate p; in this
case the C branch does so too, so we are fine.  So, as usual, we need
to transport demands on free variables to the call site(s).  Compare
Note [Lazy and unleashable free variables].

The implementation is easy.  When analysing a join point, we can
analyse its body with the demand from the entire join-binding (written
let_dmd here).

Another win for join points!  #13543.

However, note that the strictness signature for a join point can
look a little puzzling.  E.g.

    (join j x = \y. error "urk")
    (in case v of              )
    (     A -> j 3             )  x
    (     B -> j 4             )
    (     C -> \y. blah        ')

The entire thing is in a C1(L) context, so j's strictness signature
will be    [A]b
meaning one absent argument, returns bottom.  That seems odd because
there's a \y inside.  But it's right because when consumed in a C1(L)
context the RHS of the join point is indeed bottom.

Note [Demand signatures are computed for a threshold demand based on idArity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We compute demand signatures assuming idArity incoming arguments to approximate
behavior for when we have a call site with at least that many arguments. idArity
is /at least/ the number of manifest lambdas, but might be higher for PAPs and
trivial RHS (see Note [Demand analysis for trivial right-hand sides]).

Because idArity of a function varies independently of its cardinality
properties (cf. Note [idArity varies independently of dmdTypeDepth]), we
implicitly encode the arity for when a demand signature is sound to unleash
in its 'dmdTypeDepth' (cf. Note [Understanding DmdType and DmdSig] in
GHC.Types.Demand). It is unsound to unleash a demand signature when the
incoming number of arguments is less than that.
See Note [What are demand signatures?] in GHC.Types.Demand for more details
on soundness.

Why idArity arguments? Because that's a conservative estimate of how many
arguments we must feed a function before it does anything interesting with them.
Also it elegantly subsumes the trivial RHS and PAP case.

There might be functions for which we might want to analyse for more incoming
arguments than idArity. Example:

  f x =
    if expensive
      then \y -> ... y ...
      else \y -> ... y ...

We'd analyse `f` under a unary call demand C1(L), corresponding to idArity
being 1. That's enough to look under the manifest lambda and find out how a
unary call would use `x`, but not enough to look into the lambdas in the if
branches.

On the other hand, if we analysed for call demand C1(C1(L)), we'd get useful
strictness info for `y` (and more precise info on `x`) and possibly CPR
information, but

  * We would no longer be able to unleash the signature at unary call sites
  * Performing the worker/wrapper split based on this information would be
    implicitly eta-expanding `f`, playing fast and loose with divergence and
    even being unsound in the presence of newtypes, so we refrain from doing so.
    Also see Note [Don't eta expand in w/w] in GHC.Core.Opt.WorkWrap.

Since we only compute one signature, we do so for arity 1. Computing multiple
signatures for different arities (i.e., polyvariance) would be entirely
possible, if it weren't for the additional runtime and implementation
complexity.

Note [idArity varies independently of dmdTypeDepth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to check in GHC.Core.Lint that dmdTypeDepth <= idArity for a let-bound
identifier. But that means we would have to zap demand signatures every time we
reset or decrease arity. That's an unnecessary dependency, because

  * The demand signature captures a semantic property that is independent of
    what the binding's current arity is
  * idArity is analysis information itself, thus volatile
  * We already *have* dmdTypeDepth, wo why not just use it to encode the
    threshold for when to unleash the signature
    (cf. Note [Understanding DmdType and DmdSig] in GHC.Types.Demand)

Consider the following expression, for example:

    (let go x y = `x` seq ... in go) |> co

`go` might have a strictness signature of `<1L><L>`. The simplifier will identify
`go` as a nullary join point through `joinPointBinding_maybe` and float the
coercion into the binding, leading to an arity decrease:

    join go = (\x y -> `x` seq ...) |> co in go

With the CoreLint check, we would have to zap `go`'s perfectly viable strictness
signature.

Note [Demand analysis for trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T.

Fortunately, GHC.Core.Opt.Arity gives 'foo' arity 2, which is enough for LetDown to
forward plusInt's demand signature, and all is well (see Note [Newtype arity] in
GHC.Core.Opt.Arity)! A small example is the test case NewtypeArity.

Note [Absence analysis for stable unfoldings and RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #18638 shows that it's really important to do absence analysis
for stable unfoldings. Consider

   g = blah

   f = \x.  ...no use of g....
   {- f's stable unfolding is f = \x. ...g... -}

If f is ever inlined we use 'g'. But f's current RHS makes no use
of 'g', so if we don't look at the unfolding we'll mark g as Absent,
and transform to

   g = error "Entered absent value"
   f = \x. ...
   {- f's stable unfolding is f = \x. ...g... -}

Now if f is subsequently inlined, we'll use 'g' and ... disaster.

SOLUTION: if f has a stable unfolding, adjust its DmdEnv (the demands
on its free variables) so that no variable mentioned in its unfolding
is Absent.  This is done by the function Demand.keepAliveDmdEnv.

ALSO: do the same for Ids free in the RHS of any RULES for f.

PS: You may wonder how it can be that f's optimised RHS has somehow
discarded 'g', but when f is inlined we /don't/ discard g in the same
way. I think a simple example is
   g = (a,b)
   f = \x.  fst g
   {-# INLINE f #-}

Now f's optimised RHS will be \x.a, but if we change g to (error "..")
(since it is apparently Absent) and then inline (\x. fst g) we get
disaster.  But regardless, #18638 was a more complicated version of
this, that actually happened in practice.
-}

{- *********************************************************************
*                                                                      *
                      Fixpoints
*                                                                      *
********************************************************************* -}

{- Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

 * To get information on used free variables (both lazy and strict!)
   (see Note [Lazy and unleashable free variables])
 * To ensure that all expressions have been traversed at least once, and any left-over
   strictness annotations have been updated.

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")

Note [Trimming a demand to a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two reasons we sometimes trim a demand to match a type.
  1. GADTs
  2. Recursive products and widening

More on both below.  But the botttom line is: we really don't want to
have a binder whose demand is more deeply-nested than its type
"allows". So in findBndrDmd we call trimToType and findTypeShape to
trim the demand on the binder to a form that matches the type

Now to the reasons. For (1) consider
  f :: a -> Bool
  f x = case ... of
          A g1 -> case (x |> g1) of (p,q) -> ...
          B    -> error "urk"

where A,B are the constructors of a GADT.  We'll get a 1P(L,L) demand
on x from the A branch, but that's a stupid demand for x itself, which
has type 'a'. Indeed we get ASSERTs going off (notably in
splitUseProdDmd, #8569).

For (2) consider
  data T = MkT Int T    -- A recursive product
  f :: Int -> T -> Int
  f 0 _         = 0
  f _ (MkT n t) = f n t

Here f is lazy in T, but its *usage* is infinite: P(L,P(L,P(L, ...))).
Notice that this happens because T is a product type, and is recrusive.
If we are not careful, we'll fail to iterate to a fixpoint in dmdFix,
and bale out entirely, which is inefficient and over-conservative.

Worse, as we discovered in #18304, the size of the usages we compute
can grow /exponentially/, so even 10 iterations costs far too much.
Especially since we then discard the result.

To avoid this we use the same findTypeShape function as for (1), but
arrange that it trims the demand if it encounters the same type constructor
twice (or three times, etc).  We use our standard RecTcChecker mechanism
for this -- see GHC.Core.Opt.WorkWrap.Utils.findTypeShape.

This is usually call "widening".  We could do it just in dmdFix, but
since are doing this findTypeShape business /anyway/ because of (1),
and it has all the right information to hand, it's extremely
convenient to do it there.

-}

{- *********************************************************************
*                                                                      *
                 Strictness signatures and types
*                                                                      *
********************************************************************* -}

unitDmdType :: DmdEnv -> DmdType
unitDmdType dmd_env = DmdType dmd_env [] topDiv

coercionDmdEnv :: Coercion -> DmdEnv
coercionDmdEnv co = coercionsDmdEnv [co]

coercionsDmdEnv :: [Coercion] -> DmdEnv
coercionsDmdEnv cos = mapVarEnv (const topDmd) (getUniqSet $ coVarsOfCos cos)
                      -- The VarSet from coVarsOfCos is really a VarEnv Var

addLazyFVs :: DmdType -> DmdEnv -> DmdType
addLazyFVs dmd_ty lazy_fvs
  = dmd_ty `plusDmdType` (mkPlusDmdArg lazy_fvs)
        -- Using plusDmdType (rather than just plus'ing the envs)
        -- is vital.  Consider
        --      let f = \x -> (x,y)
        --      in  error (f 3)
        -- Here, y is treated as a lazy-fv of f, but we must `plusDmd` that L
        -- demand with the bottom coming up from 'error'
        --
        -- I got a loop in the fixpointer without this, due to an interaction
        -- with the lazy_fv filtering in denotRhsSig.  Roughly, it was
        --      letrec f n x
        --          = letrec g y = x `fatbar`
        --                         letrec h z = z + ...g...
        --                         in h (f (n-1) x)
        --      in ...
        -- In the initial iteration for f, f=Bot
        -- Suppose h is found to be strict in z, but the occurrence of g in its RHS
        -- is lazy.  Now consider the fixpoint iteration for g, esp the demands it
        -- places on its free variables.  Suppose it places none.  Then the
        --      x `fatbar` ...call to h...
        -- will give a x->V demand for x.  That turns into a L demand for x,
        -- which floats out of the defn for h.  Without the modifyEnv, that
        -- L demand doesn't get both'd with the Bot coming up from the inner
        -- call to f.  So we just get an L demand for x for g.

{-
Note [Do not strictify the argument dictionaries of a dfun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker can tie recursive knots involving dfuns, so we do the
conservative thing and refrain from strictifying a dfun's argument
dictionaries.
-}

{- Note [NOINLINE and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one point we disabled strictness for NOINLINE functions, on the
grounds that they should be entirely opaque.  But that lost lots of
useful semantic strictness information, so now we analyse them like
any other function, and pin strictness information on them.

That in turn forces us to worker/wrapper them; see
Note [Worker/wrapper for NOINLINE functions] in GHC.Core.Opt.WorkWrap.


Note [Lazy and unleashable free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LetDown generally unleashes demands on arguments and free variables at call
sites, both of which are part of the demand signature. That is especially
beneficial for strict and once-used FVs, as the following example inspired by
'roll' in imaginary/wheel-sieve2 shows:
```
  roll x = letrec
               go y = if ... then roll (x-1) else x+1
           in
           if ... then x+2 else go ms
```
We want to see that roll is strict in its arg x, which is because
go is called, which in turn is strict in its free var x.
We wouldn't see that go is strict if we used LetUp here.

An example where we get better once-used info on free variables is inspired by
simple's $wpolynomial (see #19001 for the original program):
```
  let x = expensive
      f n = f (n-1)
      f 0 = x
  in f 200
```
Here, x is only used once. If we used LetUp, analysis of the recursive
function f would have to conclude that it uses x multiple times, since
its RHS may be entered multiple times. Not so with LetDown: We infer
that x is used only once, because x occurs in a base case of f rather
than in an inductive case. That in turn may lead to eta-expansion of x
if it is has function type and is also called once.

Another example that is currently left on the table:
```
  f :: Int -> Int -> Int
  f x y = let t = x+1
      h z = if z==0 then t else
            if z==1 then x+1 else
            x + h (z-1)
  in h y
```
Calling h does indeed evaluate x, but we can only see that if we unleash
a demand on x at the call site for t. We use LetUp for thunks like t, so
we won't catch that at the moment.

        ON THE OTHER HAND

We don't want to put *all* the fv's from the RHS into the
DmdType. Because

 * it makes the demand signatures larger, and hence slows down fixpointing

and

 * it is useless information at the call site anyways:
   For lazy, used-many times fv's we will never get any better result than
   that, no matter how good the actual demand on the function at the call site
   is (unless it is always absent, but then the whole binder is useless).

Therefore we exclude lazy multiple-used fv's from the environment in the
demand signature's DmdType.

But now the signature lies! (Missing variables are assumed to be absent.) To
make up for this, the code that analyses the binding keeps the demand on those
variable separate (usually called "lazy_fv") and adds it to the demand of the
whole binding later, very much like the LetUp rule.

What if we decide _not_ to store a strictness signature for a binding at
all, as we do when aborting a fixed-point iteration? Then we risk losing
the information that the strict variables are being used. In that case, we
take all free variables mentioned in the (unsound) strictness signature,
conservatively approximate the demand put on them (topDmd), and add that
to the "lazy_fv" returned by "dmdFix".

Note [Lazy free variables and monotonicity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is another pitfall with Note [Lazy and unleashable free variables].
Consider the cartesian product of lists a and b:
```
cart a b =
  let go1 xs = case xs of {
        []    -> []
        x:xs' ->
           let z = go1 xs' in
           let go2 ys = case ys of {
             []    -> z
             y:ys' -> (x,y):go2 ys'
           in go2 b
      }
  in go1 a
```
Given the stable demand signature <1L>{z->ML} and lazy_fv {go2->L,x->L} for
go2, these are the demand signatures and lazy_fv of go1 after each iteration:
 1. <1L>{b->ML, go1->MCM(L)}, {}
 2. <1L>{}, {b->L, go1->L}
 3. <1L>{b->ML, go1->MCM(L)}, {}
 4. <1L>{}, {b->L, go1->L}
 5. ...
This stops after 10 iterations, but only because we abort fixed-point iteration
in that case; otherwise we'd loop endlessly.

First observation: go1 is part of the recursive group. It is extremely unlikely
that go1 will ever have be called-once after fixed-point iteration, so by
calling 'reuseBndrs' we immediately put go1->LCL(L) (which is just go1->L) in
the lazy_fv. This can safe us an additional iteration over the RHS in some
cases, for example when there is no used-once free variable like b that would
otherwise force an additional iteration.

Second observation: reusing the rec binders doesn't fix the problem we are
seeing. 'denotRhsSig' should be monotonic in the signature it takes and
produces, but the signature in the step from (1) to (2) clearly isn't.
By pretending that lazy_fv is merged back into the signature, the step (1) to
(2) becomes monotonic, but then (2) to (3) is non-monotonic. The reason is that
while the previous signature is an implicit input (via the SigEnv) to
'denotRhsSig', the previous lazy_fv are not!

So 'denotRhsSig' should simply *extend* the previous lazy_fv rather producing
its own. Then we have one lazy_fv per recursive group that we continually
extend. If a binder is in the domain of lazy_fv, it should not occur in the FVs
of the demand signature.

Note [Lazy free variables and Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Note [Initialising strictness], we effectively use the annotated AST as state
between fixed-point iteration to cache demand signatures.
But unlike the demand type of the strictness signature, the lazy FVs
from Note [Lazy free and unleashable variables] can't easily be persisted in the
AST. Result, as described in Note [Initialising strictness]: Exponential runtime
behavior.
Our solution is an additional field in DmdSig to cache the lazy FVs between
fixed-point iterations, so that it can take part in the mechanism described
in Note [Initialising strictness].

Note [Lazy free variables are stable after signature is stable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We know that we find the fixed-point when the signature didn't change between
fixed-point iterations. But why don't we need to test the lazy_fv?
Because the lazy_fv won't have changed if the signatures didn't change.
This is easy to see if you consider what would happen if we iterated an
additional time with identical signatures. In 'denotRhsSig', there isn't
even a data flow from lazy_fv into the function that analyses the RHS. Analysis
being a pure function, it observes the same old signatures through the AnalEnv
and will produce the exact same rhs_fv as in the previous iteration. Then the
computed lazy_fv' resulting from a call to addWeakDmds will be identical to
lazy_fv.

Bottom line: If the demand signatures are the same after iteration n and n+1,
then so will the lazy_fv of iteration n+1 and n+2. So we can just return the
lazy_fv after iteration n+1, together with the analysed RHSs after iteration
n+1.

************************************************************************
*                                                                      *
\subsection{Strictness signatures}
*                                                                      *
************************************************************************
-}


data AnalEnv = AE
   { ae_opts      :: !DmdAnalOpts -- ^ Analysis options
   , ae_sigs      :: !SigEnv
   , ae_fam_envs  :: !FamInstEnvs
   , ae_inl_fun   :: !InsideInlineableFun
                           -- ^ Whether we analyse the body of an inlineable fun.
                           -- See Note [Do not unbox class dictionaries].
   }

        -- We use the se_env to tell us whether to
        -- record info about a variable in the DmdEnv
        -- We do so if it's a LocalId, but not top-level
        --
        -- The DmdEnv gives the demand on the free vars of the function
        -- when it is given enough args to satisfy the strictness signature

type SigEnv = VarEnv TopLevelFlag

emptyAnalEnv :: DmdAnalOpts -> FamInstEnvs -> AnalEnv
emptyAnalEnv opts fam_envs
    = AE { ae_opts         = opts
         , ae_sigs         = emptySigEnv
         , ae_fam_envs     = fam_envs
         , ae_inl_fun      = NotInsideInlineableFun
         }

emptySigEnv :: SigEnv
emptySigEnv = emptyVarEnv

extendAnalEnv :: TopLevelFlag -> AnalEnv -> Id -> AnalEnv
extendAnalEnv top_lvl env var
  = env { ae_sigs = extendSigEnv top_lvl (ae_sigs env) var }

extendSigEnv :: TopLevelFlag -> SigEnv -> Id -> SigEnv
extendSigEnv top_lvl sigs var = extendVarEnv sigs var top_lvl

lookupSigEnv :: AnalEnv -> Id -> Maybe TopLevelFlag
lookupSigEnv env id = lookupVarEnv (ae_sigs env) id

-- | Sets 'ae_inl_fun' according to whether the given 'Id' has an inlineable
-- unfolding. See Note [Do not unbox class dictionaries].
adjustInlFun :: Id -> AnalEnv -> AnalEnv
adjustInlFun id env
  | isStableUnfolding (realIdUnfolding id) = env { ae_inl_fun = InsideInlineableFun }
  | otherwise                              = env { ae_inl_fun = NotInsideInlineableFun }


findBndrDmd :: AnalEnv -> RecFlag -> Id -> DmdType -> SPair DmdType Demand
-- See Note [Trimming a demand to a type]
findBndrDmd env rec_flag bndr = \(DmdType fvs args div) ->
  let (# !fvs', !dmd #) = worker div fvs
  in S2 (DmdType fvs' args div) dmd
  where
    !worker = find_bndr_dmd env rec_flag bndr
{-# INLINE findBndrDmd #-}

-- The manually worker/wrappered hot loop of 'findBndrDmd' and 'findBndrsDmds'.
-- Why wouldn't DmdAnal catch this? Because of the staging that is going on here.
-- We return a lambda of type `Divergence -> DmdEnv -> (# DmdEnv, Demand #)`.
-- We could never inline a wrapper of a lambda that we return because of
-- higher-ordnerness, so we have to optimise it ourselves.
find_bndr_dmd :: AnalEnv -> RecFlag -> Id -> Divergence -> DmdEnv -> (# DmdEnv, Demand #)
find_bndr_dmd env rec_flag bndr = \div fv_dmds ->
  -- Intentionally arity 3 so that we cache eval of all the where bindings below
  let !(!fv_dmds', !dmd) = peelFV bndr div fv_dmds
      !dmd' | strictify  = strictifyDictDmd id_ty $! trimToType ts dmd
            | otherwise  = trimToType ts dmd
  in (# fv_dmds', dmd' #)
  where
    !_ = idUnique bndr
    !id_ty = idType bndr
    !fam_envs = ae_fam_envs env
    !ts = findTypeShape fam_envs id_ty
    !strictify = dmd_strict_dicts (ae_opts env) && isNonRec rec_flag
           -- We never want to strictify a recursive let. At one point SG forgot
           -- this guard and got strange ghc/alloc regressions somewhere in
           -- Text.Read.Lex.lexStrItem in e.g. T11303b and a bunch of others
{-# INLINE find_bndr_dmd #-}

findBndrsDmds :: AnalEnv -> RecFlag -> [Var] -> DmdType -> SPair DmdType [Demand]
-- Return the demands on the Ids in the [Var]
findBndrsDmds env rec_flag bs = \(DmdType fvs args div) ->
  let (# !fvs', !dmds #) = worker div fvs
  in S2 (DmdType fvs' args div) dmds
  where
    !worker = find_bndrs_dmds env rec_flag bs
{-# INLINE findBndrsDmds #-}

find_bndrs_dmds :: AnalEnv -> RecFlag -> [Var] -> Divergence -> DmdEnv -> (# DmdEnv, [Demand] #)
find_bndrs_dmds env rec_flag bs = case bs of
  []               -> \_div fv_dmds -> (# fv_dmds, [] #)
  b:bs'
    | not (isId b) -> rest
    | otherwise    -> \div fv_dmds ->
        let !(# !fv_dmds',  !dmd  #) = cur div fv_dmds
            !(# !fv_dmds'', !dmds #) = rest div fv_dmds'
        in (# fv_dmds'', dmd:dmds #)
    where
      !rest = find_bndrs_dmds env rec_flag bs'
      !cur = find_bndr_dmd env rec_flag b

{- Note [Making dictionaries strict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Opt_DictsStrict flag makes GHC use call-by-value for dictionaries.  Why?

* Generally CBV is more efficient.

* Dictionaries are always non-bottom; and never take much work to
  compute.  E.g. a dfun from an instance decl always returns a dicionary
  record immediately.  See DFunUnfolding in CoreSyn.
  See also Note [Recursive superclasses] in TcInstDcls.

* The strictness analyser will then unbox dictionaries and pass the
  methods individually, rather than in a bundle.  If there are a lot of
  methods that might be bad; but worker/wrapper already does throttling.

* A newtype dictionary is *not* always non-bottom.  E.g.
      class C a where op :: a -> a
      instance C Int where op = error "urk"
  Now a value of type (C Int) is just a newtype wrapper (a cast) around
  the error thunk.  Don't strictify these!

See #17758 for more background and perf numbers.

The implementation is extremly simple: just make the strictness
analyser strictify the demand on a dictionary binder in
'findBndrDmd'.

However there is one case where this can make performance worse.
For the principle consider some function at the core level:
    myEq :: Eq a => a -> a -> Bool
    myEq eqDict x y = ((==) eqDict) x y
If we make the dictionary strict then WW can fire turning this into:
    $wmyEq :: (a -> a -> Bool) -> a -> a -> Bool
    $wmyEq eq x y = eq x y
Which *usually* performs better. However if the dictionary is known we
are far more likely to inline a function applied to the dictionary than
to inline one applied to a function. Sometimes this makes just enough
of a difference to stop a function from inlining. This is documented in
#18421.

It's somewhat similar to Note [Do not unbox class dictionaries] although
here our problem is with the inliner, not the specializer.

Note [Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See section 9.2 (Finding fixpoints) of the paper.

Our basic plan is to initialise the strictness of each Id in a
recursive group to "bottom", and find a fixpoint from there.  However,
this group B might be inside an *enclosing* recursive group A, in
which case we'll do the entire fixpoint shebang on for each iteration
of A. This can be illustrated by the following example:

Example:

  f [] = []
  f (x:xs) = let g []     = f xs
                 g (y:ys) = y+1 : g ys
              in g (h x)

At each iteration of the fixpoint for f, the analyser has to find a
fixpoint for the enclosed function g. In the meantime, the demand
values for g at each iteration for f are *greater* than those we
encountered in the previous iteration for f. Therefore, we can begin
the fixpoint for g not with the bottom value but rather with the
result of the previous analysis. I.e., when beginning the fixpoint
process for g, we can start from the demand signature computed for g
previously and attached to the binding occurrence of g.

To speed things up, we initialise each iteration of A (the enclosing
one) from the result of the last one, which is neatly recorded in each
binder.  That way we make use of earlier iterations of the fixpoint
algorithm. (Cunning plan.)

But on the *first* iteration we want to *ignore* the current strictness
of the Id, and start from "bottom".  Nowadays the Id can have a current
strictness, because interface files record strictness for nested bindings.
To know when we are in the first iteration, we look at the ae_virgin
field of the AnalEnv.


Note [Final Demand Analyser run]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the information that the demand analyser determines is not always
preserved by the simplifier.  For example, the simplifier will happily rewrite
  \y [Demand=MU] let x = y in x + x
to
  \y [Demand=MU] y + y
which is quite a lie: Now y occurs more than just once.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in
   GHC.Core.Opt.WorkWrap.
   If it's not reliable, it's better not to have it at all.

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

Note that, in contrast, the single-call information (CM(..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.
-}
