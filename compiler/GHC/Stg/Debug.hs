{-# LANGUAGE TupleSections #-}
-- This module contains functions which implement
-- the -finfo-table-map and -fdistinct-constructor-tables flags
module GHC.Stg.Debug(collectDebugInformation) where


import GHC.Prelude

import GHC.Core
import GHC.Stg.Syntax

import GHC.Types.Id
import GHC.Core.DataCon
import GHC.Types.IPE
import GHC.Unit.Module
import GHC.Types.Name   ( getName, getOccName, occNameString, nameSrcSpan)
import GHC.Data.FastString
import GHC.Driver.Session
import GHC.Driver.Ppr

import Control.Monad (when)
import Control.Monad.Trans.RWS
import GHC.Types.Unique.Map
import GHC.Types.SrcLoc
import Control.Applicative
import GHC.Utils.Outputable

data R = R { rDynFlags :: DynFlags, rModLocation :: ModLocation, rSpan :: Maybe (RealSrcSpan, String) }

type M a = RWS R () InfoTableProvMap a

withSpan :: (RealSrcSpan, String) -> M a -> M a
withSpan s act = local (\r -> r { rSpan = Just s }) act

collectDebugInformation :: DynFlags -> ModLocation -> [StgTopBinding] -> ([StgTopBinding], InfoTableProvMap)
collectDebugInformation dflags ml bs = case runRWS (mapM collectTop bs) (R dflags ml Nothing) emptyInfoTableProvMap of
                                            (bs', m, _) -> (bs', m)

collectTop :: StgTopBinding -> M StgTopBinding
collectTop (StgTopLifted t) = StgTopLifted <$> collectStgBind t
collectTop tb = return tb

collectStgBind :: StgBinding -> M StgBinding
collectStgBind  (StgNonRec bndr rhs) = do
    rhs' <- collectStgRhs bndr rhs
    return (StgNonRec bndr rhs')
collectStgBind (StgRec pairs) = do
    es <- mapM (\(b, e) -> (b,) <$> collectStgRhs b e) pairs
    return (StgRec es)

collectStgRhs :: Id -> StgRhs -> M StgRhs
collectStgRhs bndr (StgRhsClosure ext cc us bs e)= do
  e' <- collectExpr e
  recordInfo bndr e'
  return $ StgRhsClosure ext cc us bs e'
collectStgRhs _bndr (StgRhsCon cc dc _mn ticks args) = do
  n' <- incDc dc ticks
  return (StgRhsCon cc dc n' ticks args)


recordInfo :: Id -> StgExpr -> M ()
recordInfo bndr new_rhs = do
  modLoc <- asks rModLocation
  let
    thisFile = maybe nilFS mkFastString $ ml_hs_file modLoc
    best_span = quickSourcePos thisFile new_rhs
  recordStgIdPosition bndr best_span (((, occNameString (getOccName bndr))) <$> (srcSpanToRealSrcSpan (nameSrcSpan (getName bndr))))

collectExpr :: StgExpr -> M StgExpr
collectExpr = go
  where
    go (StgApp occ as) = return $ StgApp occ as
    go (StgLit lit) = return $ StgLit lit
    go (StgConApp dc _mn as tys) = do
      n' <- incDc dc []
      return (StgConApp dc n' as tys)
    go (StgOpApp op as ty) = return (StgOpApp op as ty)
    go (StgLam bs e) =  StgLam bs <$> collectExpr e
    go (StgCase scrut bndr ty alts) =
      StgCase <$> collectExpr scrut <*> pure bndr <*> pure ty <*> mapM collectAlts alts
    go (StgLet ext bind body) = do
        bind' <- collectStgBind bind
        body' <- go body
        return (StgLet ext bind' body')
    go (StgLetNoEscape ext bind body) = do
        bind' <- collectStgBind bind
        body' <- go body
        return (StgLetNoEscape ext bind' body')

    go (StgTick tick e) = do
       let k = case tick of
                SourceNote ss fp -> withSpan (ss, fp)
                _ -> id
       e' <- k (go e)
       return (StgTick tick e')

collectAlts :: StgAlt -> M StgAlt
collectAlts (ac, bs, e) = (ac, bs, ) <$> collectExpr e


--runM dflags ml act = runRWS act (dflags, ml)


quickSourcePos :: FastString -> StgExpr -> Maybe (RealSrcSpan, String)
quickSourcePos cur_mod (StgTick (SourceNote ss m) e)
  | srcSpanFile ss == cur_mod = Just (ss, m)
  | otherwise = quickSourcePos cur_mod e
quickSourcePos _ _ = Nothing

recordStgIdPosition :: Id -> Maybe (RealSrcSpan, String) -> Maybe (RealSrcSpan, String) -> M ()
recordStgIdPosition id best_span ss = do
  dflags <- asks rDynFlags
  when (gopt Opt_InfoTableMap dflags) $ do
    let tyString = showPpr dflags (idType id)
    cc <- asks rSpan
    pprTraceM "recordStgIdPosition" (ppr id $$ ppr cc $$ ppr best_span $$ ppr ss)
    case best_span <|> cc <|> ss of
      Nothing -> return ()
      Just (rss, d) -> modify (\env -> env { provClosure = addToUniqMap (provClosure env) (idName id) (tyString, rss, d)})

incDc :: DataCon -> [Tickish Id] -> M (Maybe Int)
incDc dc _ | isUnboxedTupleDataCon dc = return Nothing
incDc dc _ | isUnboxedSumDataCon dc = return Nothing
incDc dc ts = do
  dflags <- asks rDynFlags
  if not (gopt Opt_DistinctConstructorTables dflags) then return Nothing else do
          env <- get
          mcc <- asks rSpan
          let best_span = selectTick ts <|> mcc
          let dcMap' = alterUniqMap (maybe (Just [(0, best_span)]) (\xs@((k, _):_) -> Just ((k + 1, best_span) : xs))) (provDC env) dc
          put (env { provDC = dcMap' })
          let r = lookupUniqMap dcMap' dc
          return (fst . head <$> r)

selectTick :: [Tickish Id] -> Maybe (RealSrcSpan, String)
selectTick [] = Nothing
selectTick (SourceNote rss d : ts ) = selectTick ts <|> Just (rss, d)