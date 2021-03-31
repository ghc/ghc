{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module GHC.Stg.ClosEnvShare ( stgClosEnvShare ) where

import Control.Monad

import GHC.Prelude
import GHC.Data.FastString
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Unique.Supply
import GHC.Stg.FVs
import GHC.Stg.Syntax
import GHC.Utils.Outputable
import GHC.Utils.Panic

{-
*************************************************************************
*                                                                      *
Transformation
*                                                                      *
*************************************************************************
-}

type CesAble pass = (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id)

-- | We are sneaky (and inefficient) by annotating with free variables,
-- performing our transformation and then forgetting annotations here. This
-- means that we *must* completely traverse every expression to rebuild it
-- without the *Cg* part.
stgClosEnvShare :: UniqSupply -> [StgTopBinding] -> [StgTopBinding]
stgClosEnvShare us binds = stgClosEnvShare' us (annTopBindingsFreeVars binds)

stgClosEnvShare' :: CesAble pass
                 => UniqSupply -> [GenStgTopBinding pass] -> [StgTopBinding]
stgClosEnvShare' us binds = initUs_ us $ mapM stgCesTopBinding binds

stgCesTopBinding :: CesAble pass
                 => GenStgTopBinding pass -> UniqSM StgTopBinding
stgCesTopBinding (StgTopLifted bind) = StgTopLifted <$> stgTopCesBinding' bind
stgCesTopBinding (StgTopStringLit i bs) = return (StgTopStringLit i bs)

-- | For now, do not mess with top level binders
stgTopCesBinding' :: CesAble pass => GenStgBinding pass -> UniqSM StgBinding
stgTopCesBinding' (StgNonRec x rhs) = StgNonRec x <$> stgCesRhs rhs
stgTopCesBinding' (StgRec bs)
  = StgRec <$> mapM (\(x,rhs) -> stgCesRhs rhs >>= \rhs' -> return (x,rhs')) bs

-- | Genereate shared environments at binding sites
stgCesBinding :: CesAble pass
              => GenStgBinding pass -> UniqSM (StgBinding, [SharedEnvData])
stgCesBinding (StgNonRec x rhs) =
  do rhs' <- stgCesRhs rhs
     return (StgNonRec x rhs', [])
stgCesBinding (StgRec bs) =
  let sharedEnvsData = findSharedEnvs bs in
    do bs' <- forM bs $ \(x,rhs) ->
                do rhs' <- stgCesRhs rhs
                   let rhs'' = unpackSharedEnvs x rhs' sharedEnvsData
                   return (x,rhs'')
       return (StgRec bs', sharedEnvsData)


stgCesRhs :: CesAble pass => GenStgRhs pass -> UniqSM StgRhs
stgCesRhs (StgRhsClosure _ ccs u args body)
  = StgRhsClosure noExtFieldSilent ccs u args <$> stgCesExpr body
stgCesRhs (StgRhsCon ccs dc args) = return (StgRhsCon ccs dc args)
stgCesRhs (StgRhsEnv fvs) = return (StgRhsEnv fvs)

stgCesExpr :: CesAble pass => GenStgExpr pass -> UniqSM StgExpr
stgCesExpr (StgApp i args)          = return (StgApp i args)
stgCesExpr (StgLit l)               = return (StgLit l)
stgCesExpr (StgConApp dc args tys)  = return (StgConApp dc args tys)
stgCesExpr (StgOpApp op args ty)    = return (StgOpApp op args ty)
stgCesExpr (StgCase e id alt_ty alts) =
  do e' <- stgCesExpr e
     alts' <- mapM stgCesAlt alts
     return (StgCase e' id alt_ty alts')
stgCesExpr (StgLet _ b e) =
  do (b',sharedEnvsData) <- stgCesBinding b
     e' <- stgCesExpr e
     bindSharedEnvs sharedEnvsData (StgLet noExtFieldSilent b' e')
stgCesExpr (StgLetNoEscape _ b e) =
  do (b',sharedEnvsData) <- stgCesBinding b
     e' <- stgCesExpr e
     bindSharedEnvs sharedEnvsData (StgLetNoEscape noExtFieldSilent b' e')
stgCesExpr (StgTick t e) = StgTick t <$> stgCesExpr e
stgCesExpr (StgCaseEnv x args e) = StgCaseEnv x args <$> stgCesExpr e

stgCesAlt :: CesAble pass => GenStgAlt pass -> UniqSM StgAlt
stgCesAlt (acon,args,e) = stgCesExpr e >>= \e' -> return (acon,args,e')

{-
*************************************************************************
*                                                                      *
Data for transformation and analysis
*                                                                      *
*************************************************************************
-}

data SharedEnvData
  = SharedEnvData
  { sed_se  :: DIdSet
    -- ^ the variables which are shared
  , sed_binder :: Id
    -- ^ name of the shared environment in the heap
  , sed_rhs_ids :: [Id]
    -- ^ the list of identifiers is the list of bindings must unpack the new
    -- environment
  }

bindSharedEnv :: SharedEnvData -> StgExpr -> UniqSM StgExpr
bindSharedEnv sed e =
  mkEnvId >>= \x ->
    return $ StgLet noExtFieldSilent (StgNonRec x (StgRhsEnv (sed_se sed))) e

bindSharedEnvs :: [SharedEnvData] -> StgExpr -> UniqSM StgExpr
bindSharedEnvs envs e = foldM (flip bindSharedEnv) e envs

mkEnvId :: UniqSM Id
mkEnvId = mkIdWithU <$> getUniqueM
  where mkIdWithU u =
          mkSysLocal (mkFastString "env")
                     u
                     (pprPanic "mkEnvId" $ text s)
                     (pprPanic "mkEnvId" $ text s)
        s = "First class environments are untyped; this information should not be needed"

unpackSharedEnv :: Id -> StgRhs -> SharedEnvData -> StgRhs
unpackSharedEnv id (StgRhsCon _ _ _) sed
  | elem id (sed_rhs_ids sed)
  = pprPanic "unpackSharedEnv"
  $ text "Created a shared environment for a constructor application"
unpackSharedEnv id (StgRhsEnv _) sed
  | elem id (sed_rhs_ids sed)
  = pprPanic "unpackSharedEnv"
  $ text "Created a shared environment for a shared environment"
unpackSharedEnv id (StgRhsClosure _ ccs u args e) sed
  | elem id (sed_rhs_ids sed)
  = StgRhsClosure noExtFieldSilent ccs u args (StgCaseEnv (sed_binder sed) (sed_se sed) e)
unpackSharedEnv _ rhs _ = rhs

unpackSharedEnvs :: Id -> StgRhs -> [SharedEnvData] -> StgRhs
unpackSharedEnvs id rhs seds = foldr (flip (unpackSharedEnv id)) rhs seds

{-
*************************************************************************
*                                                                      *
Analysis
*                                                                      *
*************************************************************************
-}

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

findSharedEnvs :: CesAble pass
               => [(BinderP pass,GenStgRhs pass)] -> [SharedEnvData]
findSharedEnvs _ = []
