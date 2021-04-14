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

-- | We are sneaky (and inefficient) by annotating with free variables,
-- performing our transformation and then forgetting annotations here. This
-- means that we *must* completely traverse every expression to rebuild it
-- without the *Cg* part.
stgClosEnvShare :: UniqSupply -> [StgTopBinding] -> ([StgTopBinding],CesLog)
stgClosEnvShare us binds = stgClosEnvShare' us (annTopBindingsFreeVars binds)

stgClosEnvShare' :: CesAble pass
                 => UniqSupply
                 -> [GenStgTopBinding pass]
                 -> ([StgTopBinding],CesLog)
stgClosEnvShare' us binds = runCesM us $ mapM stgCesTopBinding binds

stgCesTopBinding :: CesAble pass
                 => GenStgTopBinding pass -> CesM StgTopBinding
stgCesTopBinding (StgTopLifted bind) = StgTopLifted <$> stgTopCesBinding' bind
stgCesTopBinding (StgTopStringLit i bs) = return (StgTopStringLit i bs)

-- | For now, do not mess with top level binders
stgTopCesBinding' :: CesAble pass => GenStgBinding pass -> CesM StgBinding
stgTopCesBinding' (StgNonRec x rhs) = StgNonRec x <$> stgCesRhs rhs
stgTopCesBinding' (StgRec bs)
  = StgRec <$> mapM (\(x,rhs) -> stgCesRhs rhs >>= \rhs' -> return (x,rhs')) bs

stgCesBinding :: CesAble pass => GenStgBinding pass -> CesM StgBinding
stgCesBinding (StgNonRec x rhs) = StgNonRec x <$> stgCesRhs rhs
stgCesBinding (StgRec bs) =
  StgRec <$> mapM (\(x,rhs) -> (,) x <$> stgCesRhs rhs) bs
-- stgCesBinding (StgRec bs) =
--   do sharedEnvsData <- createSEDHorizontal bs
--      bs' <- forM bs $ \(x,rhs) ->
--               do rhs' <- stgCesRhs rhs
--                  let rhs'' = unpackSharedEnvs x rhs' sharedEnvsData
--                  return (x,rhs'')
--      return (StgRec bs', sharedEnvsData)

stgCesRhs :: CesAble pass => GenStgRhs pass -> CesM StgRhs
stgCesRhs (StgRhsClosure _ ccs u args body)
  = StgRhsClosure noExtFieldSilent ccs u args <$> stgCesExpr body
stgCesRhs (StgRhsCon ccs dc args) = return (StgRhsCon ccs dc args)
stgCesRhs (StgRhsEnv fvs) = return (StgRhsEnv fvs)

-- stgCesExpr :: CesAble pass => GenStgExpr pass -> CesM StgExpr
-- stgCesExpr expr =
--   case collectBinderChain expr of
--     [] -> stgCesExpr' expr
--     bs -> do sharedEnvsData <- createSEDShallow (collectBinderChain expr)
--              bindSharedEnvs sharedEnvsData <$> stgCesExpr' expr
--   where collectBinderChain e =
--           case e of
--             (StgLet _ (StgNonRec x rhs) e) -> (x,rhs):collectBinderChain e
--             (StgLet _ (StgRec bs) e)       -> bs ++ collectBinderChain e
--             _                              -> []

{-
TODO: I think that it is better that shared environments are introduced here at
the beginning of the expression; so an expression can be converted into one that
introduces a binding. This is different than the way I have it now wherein
shared environments are pulled out of let-expressions.
-}

stgCesExpr :: CesAble pass => GenStgExpr pass -> CesM StgExpr
stgCesExpr (StgApp i args)          = return (StgApp i args)
stgCesExpr (StgLit l)               = return (StgLit l)
stgCesExpr (StgConApp dc args tys)  = return (StgConApp dc args tys)
stgCesExpr (StgOpApp op args ty)    = return (StgOpApp op args ty)
stgCesExpr (StgCase e id alt_ty alts) =
  do e' <- stgCesExpr e
     alts' <- mapM stgCesAlt alts
     return (StgCase e' id alt_ty alts')
stgCesExpr (StgLet _ (StgNonRec x (StgRhsClosure fvs ccs u args body)) e) =
  do body' <- stgCesExpr body
     e' <- stgCesExpr e
     env_id <- mkEnvId
     let sed = SharedEnvData fvs env_id [x]
     let clos' = StgRhsClosure noExtFieldSilent ccs u args
                               (unpackSharedEnv sed body')
     return (bindSharedEnv sed (StgLet noExtFieldSilent (StgNonRec x clos') e'))
stgCesExpr (StgLet _ b e) =
  do b' <- stgCesBinding b
     e' <- stgCesExpr e
     return (StgLet noExtFieldSilent b' e')
stgCesExpr (StgLetNoEscape _ b e) =
  do b' <- stgCesBinding b
     e' <- stgCesExpr e
     return (StgLetNoEscape noExtFieldSilent b' e')
stgCesExpr (StgTick t e) = StgTick t <$> stgCesExpr e
stgCesExpr (StgCaseEnv x args e) = StgCaseEnv x args <$> stgCesExpr e


-- collectBinderChain :: CesAble pass
--                    => GenStgExpr pass -> [(BinderP pass, GenStgRhs pass)]
-- collectBinderChain e =
--   case e of
--     (StgLet _ (StgNonRec x rhs) e) -> (x,rhs):collectBinderChain e
--     (StgLet _ (StgRec bs) e)       -> bs ++ collectBinderChain e
--     _                              -> []

stgCesAlt :: CesAble pass => GenStgAlt pass -> CesM StgAlt
stgCesAlt (acon,args,e) = stgCesExpr e >>= \e' -> return (acon,args,e')

{-
*************************************************************************
*                                                                      *
Data and Monad for transformation and analysis
*                                                                      *
*************************************************************************
-}

type CesAble pass = (XRhsClosure pass ~ DIdSet, BinderP pass ~ Id)

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

newtype CesM a
  = CesM { unwrapCesM :: WriterT CesLog UniqSM a }
  deriving (Functor, Applicative, Monad)

instance MonadUnique CesM where
  getUniqueSupplyM = CesM (lift getUniqueSupplyM)
  getUniqueM = CesM (lift getUniqueM)
  getUniquesM = CesM (lift getUniquesM)

runCesM :: UniqSupply -> CesM a -> (a,CesLog)
runCesM us = initUs_ us . runWriterT . unwrapCesM

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

bindSharedEnv :: SharedEnvData -> StgExpr -> StgExpr
bindSharedEnv sed e =
  StgLet noExtFieldSilent (StgNonRec (sed_binder sed) (StgRhsEnv (sed_se sed))) e

bindSharedEnvs :: [SharedEnvData] -> StgExpr -> StgExpr
bindSharedEnvs envs e = foldr bindSharedEnv e envs

mkEnvId :: CesM Id
mkEnvId = mkSysLocalM (mkFastString "env") Many (mkVisFunTyMany (mkNumLitTy 0) (mkNumLitTy 0))
  -- where s = "First class environments are untyped; this information should not be needed"
  --       m = pprPanic "mkEnvId" $ text s

unpackSharedEnv :: SharedEnvData -> StgExpr -> StgExpr
unpackSharedEnv sed = StgCaseEnv (sed_binder sed) (sed_se sed)

{-
*************************************************************************
*                                                                      *
Analyses
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

type SharingCandidate = (Id, DIdSet)

-- | Shallow refers to creating shared environments of a block of bindings;
-- this is in contrast with a depth analysis which looks to shared environments
-- of closures to be bound within closures
createSEDShallow :: CesAble pass
                    => [(BinderP pass,GenStgRhs pass)] -> CesM [SharedEnvData]
createSEDShallow bs =
  log_num_candidates (length sharingCandidates) >>
     -- shareMinimalFVSetCover sharingCandidates
     needlesslyShared sharingCandidates
  where sharingCandidates = filter (isEmptyDVarSet . snd)
                            . map (second getRhsEnv)
                            $ bs

getRhsEnv :: CesAble pass => GenStgRhs pass -> DIdSet
getRhsEnv (StgRhsClosure fvs _ _ _ _) = fvs
getRhsEnv (StgRhsCon _ _ _) = emptyDVarSet
getRhsEnv (StgRhsEnv _) = emptyDVarSet

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
shareMinimalFVSetCover cds | length cds < 2 = log_env_too_small >> return []
shareMinimalFVSetCover cds | otherwise      = concatMapM maybeMkSharedData covering
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

-- | This isn't really shared closure analysis, rather it always creates a first
-- class environment. It is here to test the code generator
needlesslyShared :: [SharingCandidate] -> CesM [SharedEnvData]
needlesslyShared cds = mapM mkSed cds
  where mkSed (id,fvs) =
          mkEnvId >>= \env_id ->
            log_shared_env_created >>
            return (SharedEnvData fvs env_id [id])
