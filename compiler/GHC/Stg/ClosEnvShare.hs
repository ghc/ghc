{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Stg.ClosEnvShare ( stgClosEnvShare, CesLog ) where

import Control.Arrow hiding ((<+>))
import Data.Semigroup
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS

import GHC.Prelude

import GHC.Core.Multiplicity
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Supply
import GHC.Stg.FVs
import GHC.Stg.Syntax
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad

{-
*************************************************************************
*                                                                      *
Transformation
*                                                                      *
*************************************************************************
-}

-- | Top-level entry point for our transformation
stgClosEnvShare :: UniqSupply -> [StgTopBinding] -> ([StgTopBinding],CesLog)
stgClosEnvShare us binds =
  first unAnnTopBindingsFreeVars
        (stgClosEnvShare' us (annTopBindingsFreeVars binds))
-- We are sneaky (and inefficient) by annotating with free variables,
-- performing our transformation and then forgetting annotations here. This
-- means that we *must* completely traverse every expression to rebuild it
-- without the *Cg* part.

stgClosEnvShare' :: UniqSupply
                 -> [CgStgTopBinding]
                 -> ([CesStgTopBinding],CesLog)
stgClosEnvShare' us binds = runCesM us $ mapM stgCesTopBinding binds

stgCesTopBinding :: CgStgTopBinding -> CesM CesStgTopBinding
stgCesTopBinding (StgTopLifted bind) = StgTopLifted <$> stgTopCesBinding' bind
stgCesTopBinding (StgTopStringLit i bs) = return (StgTopStringLit i bs)

-- | For now, do not mess with top level binders
stgTopCesBinding' :: CgStgBinding -> CesM CesStgBinding
stgTopCesBinding' (StgNonRec x rhs) = StgNonRec x <$> stgCesRhs rhs
stgTopCesBinding' (StgRec bs)
  = StgRec <$> mapM (\(x,rhs) -> stgCesRhs rhs >>= \rhs' -> return (x,rhs')) bs

stgCesBinding :: CgStgBinding -> CesM CesStgBinding
stgCesBinding (StgNonRec x rhs) = StgNonRec x <$> stgCesRhs rhs
stgCesBinding (StgRec bs) =
  StgRec <$> mapM (\(x,rhs) -> (,) x <$> stgCesRhs rhs) bs

stgCesRhs :: CgStgRhs -> CesM CesStgRhs
stgCesRhs (StgRhsClosure fvs ccs u args body)
  = StgRhsClosure fvs ccs u args <$> stgCesExpr body
stgCesRhs (StgRhsCon ccs dc args) = return (StgRhsCon ccs dc args)
stgCesRhs (StgRhsEnv fvs) = return (StgRhsEnv fvs)

-- | In a bottom-up manner, we perform closure environment sharing analysis on
-- binders and subexpressions and then add the new bindings recursively back
-- down through the subexpressions. Naturally, all of the action is in the
-- let-expression case.
stgCesExpr :: CgStgExpr -> CesM CesStgExpr
stgCesExpr (StgCase e id alt_ty alts) =
  do e' <- stgCesExpr e
     alts' <- mapM stgCesAlt alts
     return (StgCase e' id alt_ty alts')
stgCesExpr (StgLet x b e) =
  do { b' <- stgCesBinding b
     ; e' <- stgCesExpr e
     ; seds <- closEnvShareAnalyze b' e'
     ; let b'' = unpackSharedEnvsBind seds b'
           e'' = unpackSharedEnvsExpr seds e'
     ; return (bindSharedEnvs seds (StgLet x b'' e''))
     }
-- Since, non-escaping bindings are not going to return closures, we do not do
-- any closure environment sharing here.
stgCesExpr (StgLetNoEscape x b e) =
  do b' <- stgCesBinding b
     e' <- stgCesExpr e
     return (StgLetNoEscape x b' e')
stgCesExpr (StgTick t e) = StgTick t <$> stgCesExpr e
stgCesExpr (StgCaseEnv i args e) = StgCaseEnv i args <$> stgCesExpr e
stgCesExpr (StgApp i args)          = return (StgApp i args)
stgCesExpr (StgLit l)               = return (StgLit l)
stgCesExpr (StgConApp dc args tys)  = return (StgConApp dc args tys)
stgCesExpr (StgOpApp op args ty)    = return (StgOpApp op args ty)

stgCesAlt :: CgStgAlt -> CesM CesStgAlt
stgCesAlt (acon,args,e) = stgCesExpr e >>= \e' -> return (acon,args,e')

{-
*************************************************************************
*                                                                      *
Analyses
*                                                                      *
*************************************************************************
-}

type SharingCandidate = (Id, DIdSet)

-- | This is the top level function which sharing analysis
closEnvShareAnalyze :: CesStgBinding -> CesStgExpr -> CesM [SharedEnvData]
closEnvShareAnalyze (StgNonRec id (StgRhsClosure fvs _ _ _ body)) expr
  = closEnvShareAnalyze' id fvs body expr
closEnvShareAnalyze (StgRec bs) expr
  = fmap concat $ forM bs $ \(id,rhs) ->
      case rhs of
        StgRhsClosure fvs _ _ _ body ->
          -- Delete the recursively bound identifiers *FOR ANALYSIS ONLY*. We
          -- don't want to add the recursive ids which will be declared *after*
          -- our shared environment.
          closEnvShareAnalyze' id (fvs `minusDVarSet` rec_ids) body expr
        _ -> return []
  where
    rec_ids = mkDVarSet (map fst bs)
closEnvShareAnalyze _ _
  = return []

-- | Since the shared environment will be inscope for both the body of the
-- closure and the body of the let-expression, we look in both when constructing
-- shared environments.
closEnvShareAnalyze'
  :: Id
  -> DIdSet
  -> CesStgExpr
  -> CesStgExpr
  -> CesM [SharedEnvData]
closEnvShareAnalyze' id fvs body expr
  | sizeDVarSet fvs > 2
  -- Because of the info pointer in the shared env and the pointer to the shared
  -- env in the new closure, we do not even try to share an environment unless
  -- it contains more than 2 free variables
  = shareEnvSupersets id fvs body expr >>= \msed ->
    case msed of
      Just sed -> return [sed]
      Nothing  -> return []
closEnvShareAnalyze' _ _ _ _ | otherwise = return []

-- Note [let block environment sharing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- $note
--
-- When there is a letrec block, ordinarily each will get a closure for all of
-- its own free variables. For example:
--
-- @
-- let f = {a,b,c,d,e} \x -> M
--     g = {a,b,c,d,q,z} \y -> N
--     h = {a,b,c,d,t,s} \z -> L in
--   ...
-- @
--
-- Every single closure has space allocated for @a@, @b@, @c@, and @d@. In this
-- case, we can bind a new environment to hold these variables:
--
-- @
-- let e = {a,b,c,d}
-- let f = {env,e} \x -> caseenv e of {a,b,c,d} -> M
--     g = {env,q,z} \y -> caseenv e of {a,b,c,d} -> N
--     h = {env,t,s} \z -> caseenv e of {a,b,c,d} -> L in
--   ...
-- @


-- | We look for closures in the expression that are supersets of this
-- identifier set because it will not create a space leak (subsets will) and we
-- can avoid allocating the elements in the closure twice.
shareEnvSupersets
  :: Id
  -> DIdSet
  -> CesStgExpr
  -> CesStgExpr
  -> CesM (Maybe SharedEnvData)
shareEnvSupersets id cl_env body expr =
  case supersetEnvIdsExpr cl_env body ++ supersetEnvIdsExpr cl_env expr of
    (x:xs) -> mkEnvId >>= \env_id ->
                return $ Just (SharedEnvData cl_env env_id (id:x:xs))
    []     -> return Nothing

supersetEnvIdsExpr :: DIdSet -> CesStgExpr -> [Id]
supersetEnvIdsExpr cl_env (StgLet _ bind expr) =
  supersetEnvIdsBind cl_env bind ++ supersetEnvIdsExpr cl_env expr
supersetEnvIdsExpr cl_env (StgLetNoEscape _ _ expr) =
  supersetEnvIdsExpr cl_env expr
supersetEnvIdsExpr cl_env (StgCase scrut id ty alts) =
  supersetEnvIdsExpr cl_env scrut ++ concatMap (supersetEnvIdsAlt cl_env) alts
supersetEnvIdsExpr cl_env (StgCaseEnv _ _ expr) =
  supersetEnvIdsExpr cl_env expr
supersetEnvIdsExpr cl_env (StgTick _ expr) =
  supersetEnvIdsExpr cl_env expr
supersetEnvIdsExpr _ _ = []

supersetEnvIdsBind :: DIdSet -> CesStgBinding -> [Id]
supersetEnvIdsBind cl_env (StgNonRec id rhs) =
  supersetEnvIdsRhs cl_env id rhs
supersetEnvIdsBind cl_env (StgRec bs) =
  concatMap (uncurry (supersetEnvIdsRhs cl_env)) bs

supersetEnvIdsRhs :: DIdSet -> Id -> CesStgRhs -> [Id]
supersetEnvIdsRhs cl_env id (StgRhsClosure l_env _ _ _ body) =
  (if cl_env `subDVarSet` l_env then [id] else []) ++
  supersetEnvIdsExpr cl_env body
supersetEnvIdsRhs _ _ _ = []

supersetEnvIdsAlt :: DIdSet -> CesStgAlt -> [Id]
supersetEnvIdsAlt cl_env (_,_,expr) = supersetEnvIdsExpr cl_env expr

-- | shareMinimalFVSetCover finds the minimal amount of shared environments that
-- cover all of the variables needed. Note that this produces a space
-- leak. Suppose we have the closures with the following free variable sets:
--
--   x1 = { a, b, c, d }
--   x2 = { a, b }
--   x3 = { d, e, f }
--   x4 = { z, a }
--
--  We will have the following closure structure generated:
--
--   e1 = { a, b, c, d }
--   e2 = { d, e, f }
--   e3 = { z, a }
--
--   x1 = { e1 }
--   x2 = { e1 }
--   x3 = { e2 }
--   x4 = { e3 }
--
-- Since x2 now depends on the variables c and d, there is a space
-- leak. Additionally, every envrionment is behind an extra indirection. This
-- latter problem can be solved by removing any shared environment which isn't
-- pointed to by more than one closure. Thus, the final output will be the
-- following (still containing the space leak):
--
--   e1 = { a, b, c, d }
--
--   x1 = { e1 }
--   x2 = { e1 }
--   x3 = { d, e, f }
--   x4 = { z, a }
shareMinimalFVSetCover :: [SharingCandidate] -> CesM [SharedEnvData]
shareMinimalFVSetCover cds
  | length cds < 2
  = log_env_too_small >> return []
shareMinimalFVSetCover cds
  | otherwise
  = concatMapM maybeMkSharedData covering
  where fvSets   = map snd cds
        totalFVs = unionDVarSets fvSets
        covering = greedyCover totalFVs []
        maybeMkSharedData fvSet =
          let sharers = filter (\cd -> subDVarSet (snd cd) fvSet) cds in
            if length sharers > 1
            then mkEnvId >>= \env_id ->
                   log_shared_env_created >>
                   return [SharedEnvData fvSet env_id (map fst sharers)]
            else return []

        greedyCover left_to_cover_set out_sets
          | isEmptyDVarSet left_to_cover_set
          = out_sets
        greedyCover left_to_cover_set out_sets
          | otherwise
          = let s = foldr
                      (\a b -> if ((sizeDVarSet (a `intersectDVarSet` left_to_cover_set))
                                  >
                                  (sizeDVarSet (b `intersectDVarSet` left_to_cover_set)))
                               then a
                               else b)
                      emptyDVarSet
                      fvSets
            in greedyCover (left_to_cover_set `minusDVarSet` s) (s:out_sets)

-- | This isn't really shared closure analysis; it always creates a first class
-- environment which is not shared with any other closure. It is here to test
-- the code generator or to be a baseline of how bad adding non-shared, yet
-- separate environment to a closure may be.
needlesslyShared :: [SharingCandidate] -> CesM [SharedEnvData]
needlesslyShared = mapM mkSed . filter (not . isEmptyDVarSet . snd)
  where mkSed (id,fvs) =
          mkEnvId >>= \env_id ->
            log_shared_env_created >>
            return (SharedEnvData fvs env_id [id])

{-
*************************************************************************
*                                                                      *
Data and Monad for transformation and analysis
*                                                                      *
*************************************************************************
-}

newtype CesM a
  = CesM { unwrapCesM :: WriterT CesLog UniqSM a }
  deriving (Functor, Applicative, Monad)

instance MonadUnique CesM where
  getUniqueSupplyM = CesM (lift getUniqueSupplyM)
  getUniqueM = CesM (lift getUniqueM)
  getUniquesM = CesM (lift getUniquesM)

runCesM :: UniqSupply -> CesM a -> (a,CesLog)
runCesM us = initUs_ us . runWriterT . unwrapCesM

data CesLog
  = CesLog
  { num_candidates :: Int
  , num_shared_env_created :: Int
  , num_env_too_small_to_share :: Int
  }

instance Outputable CesLog where
  ppr log = vcat
    [ text "Number of sharing candidates (i.e. rhs-closures):"
      <+> ppr (num_candidates log)
    , text "Number of shared environments created:"
      <+> ppr (num_shared_env_created log)
    , text "Number of environments too small to share:"
      <+> ppr (num_env_too_small_to_share log)
    ]

log_num_candidates :: Int -> CesM ()
log_num_candidates n = CesM . tell $ mempty { num_candidates = n }

log_shared_env_created :: CesM ()
log_shared_env_created = CesM . tell $ mempty { num_shared_env_created = 1 }

log_env_too_small :: CesM ()
log_env_too_small = CesM . tell $ mempty { num_env_too_small_to_share = 1 }

instance Semigroup CesLog where
  (CesLog x0 x1 x2) <> (CesLog y0 y1 y2) = CesLog (x0+y0) (x1+y1) (x2+y2)

instance Monoid CesLog where
  mempty = CesLog 0 0 0

-- | We lie here about the type of environments, since we don't really have a
-- type in GHC to express them. Note that STG is untyped so we do not really
-- *need* to have a type, what is important here is that environments have the
-- same representation as the type given (i.e. a function type) which states
-- that it is represented by a pointer to a heap object.
mkEnvId :: CesM Id
mkEnvId = mkSysLocalM (mkFastString "env")
                      Many
                      (mkVisFunTyMany (mkNumLitTy 0) (mkNumLitTy 0))

-- | Closure environment sharing analyses will generate these data types which
-- we can later decide to add to our program to share closures.
data SharedEnvData
  = SharedEnvData
  { sed_se  :: DIdSet
    -- ^ the variables which are shared
  , sed_binder :: Id
    -- ^ name of the shared environment in the heap
  , sed_rhs_ids :: [Id]
    -- ^ the list of identifiers is the list of bindings that must unpack the
    -- new environment
  } deriving Eq

instance Ord SharedEnvData where
  compare a b = compare (sharedVars a) (sharedVars b)
    where sharedVars x = sizeDVarSet (sed_se x) * length (sed_rhs_ids x)

-- | Take some shared environments and add bindings to them; these are escaping,
-- non-recursive let-bindings.
bindSharedEnvs :: [SharedEnvData] -> CesStgExpr -> CesStgExpr
bindSharedEnvs seds e = foldr bindSharedEnv e seds

bindSharedEnv :: SharedEnvData -> CesStgExpr -> CesStgExpr
bindSharedEnv sed e =
  StgLet noExtFieldSilent
         (StgNonRec (sed_binder sed) (StgRhsEnv (sed_se sed)))
         e

-- | Take a list of SharedEnvData and traverse a binding which will introduce
-- CaseEnv expressions wherever a closure's free variables have become part of a
-- shared environment.
unpackSharedEnvsBind :: [SharedEnvData] -> CesStgBinding -> CesStgBinding
unpackSharedEnvsBind seds (StgNonRec id rhs)
  = StgNonRec id (unpackSharedEnvsRhs seds id rhs)
unpackSharedEnvsBind seds (StgRec bs)
  = StgRec (map (\(id,rhs) -> (id,unpackSharedEnvsRhs seds id rhs)) bs)

-- Note that introducing a shared env will change the free variables. This is
-- important to change since the ClosEnvShare algorithm is bottom up thereby
-- making the new free variable here exposed to later analysis.
-- E.g.:
--    e = env {a,b,c}
--    {a,b,c,d} \n [x] -> M   ====> {e,d} \n [x] -> case-env e of {a,b,c} -> M
unpackSharedEnvsRhs :: [SharedEnvData] -> Id -> CesStgRhs -> CesStgRhs
unpackSharedEnvsRhs seds id (StgRhsClosure fvs ccs u args e) =
  let (fvs',e') = foldr go (fvs,e) seds in
    StgRhsClosure fvs' ccs u args e'
  where go sed (fvs,e) =
          if elem id (sed_rhs_ids sed)
          then ( (fvs `minusDVarSet` sed_se sed)
                 `extendDVarSet` (sed_binder sed)
               , StgCaseEnv (sed_binder sed) (sed_se sed) e)
          else (fvs,e)
unpackSharedEnvsRhs _ _ rhs = rhs

unpackSharedEnvsExpr :: [SharedEnvData] -> CesStgExpr -> CesStgExpr
unpackSharedEnvsExpr seds (StgLet x bind expr) =
  StgLet x (unpackSharedEnvsBind seds bind) (unpackSharedEnvsExpr seds expr)
unpackSharedEnvsExpr seds (StgLetNoEscape x bind expr) =
  StgLetNoEscape x bind (unpackSharedEnvsExpr seds expr)
unpackSharedEnvsExpr seds (StgCase scrut id ty alts) =
  StgCase (unpackSharedEnvsExpr seds scrut)
          id
          ty
          (map (unpackSharedEnvsAlt seds) alts)
unpackSharedEnvsExpr seds (StgCaseEnv id fvs expr) =
  StgCaseEnv id fvs (unpackSharedEnvsExpr seds expr)
unpackSharedEnvsExpr seds (StgTick t expr) =
  StgTick t (unpackSharedEnvsExpr seds expr)
unpackSharedEnvsExpr _ expr = expr

unpackSharedEnvsAlt :: [SharedEnvData] -> CesStgAlt -> CesStgAlt
unpackSharedEnvsAlt seds (con,args,expr) =
  (con,args,unpackSharedEnvsExpr seds expr)
