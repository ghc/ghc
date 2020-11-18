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
    -- A span from the ticks surrounding the new_rhs
    best_span = quickSourcePos thisFile new_rhs
    -- A back-up span if the bndr had a source position, many do not (think internally generated ids)
    bndr_span = ((, occNameString (getOccName bndr))) <$> (srcSpanToRealSrcSpan (nameSrcSpan (getName bndr)))
  recordStgIdPosition bndr best_span bndr_span

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
      StgCase <$> collectExpr scrut <*> pure bndr <*> pure ty <*> mapM collectAlt alts
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

collectAlt :: StgAlt -> M StgAlt
collectAlt (ac, bs, e) = (ac, bs, ) <$> collectExpr e

-- | Try to find the best source position surrounding a 'StgExpr'. The
-- heuristic strips ticks from the current expression until it finds one which
-- is from the module currently being compiled. This is the same method that
-- the DWARF information uses to give locations to info tables.
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
    --Useful for debugging why a certain Id gets given a certain span
    --pprTraceM "recordStgIdPosition" (ppr id $$ ppr cc $$ ppr best_span $$ ppr ss)
    case best_span <|> cc <|> ss of
      Nothing -> return ()
      Just (rss, d) -> modify (\env -> env { provClosure = addToUniqMap (provClosure env) (idName id) (tyString, rss, d)})

incDc :: DataCon -> [Tickish Id] -> M (Maybe Int)
-- Unboxed tuples and sums do not allocate so they
-- have no info tables.
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
selectTick (_:ts) = selectTick ts

{-
Note [Mapping Info Tables to Source Positions]

This note describes what the `-finfo-table-map` flag achieves.

When debugging memory issues it is very useful to be able to map a specific closure
to a position in the source. The prime example is being able to map a THUNK to
a specific place in the source program, the mapping is usually quite precise because
a fresh info table is created for each distinct THUNK.

There are three parts to the implementation

1. In GHC.Stg.Debug, the SourceNote information is used in order to give a source location to
some specific closures.
2. In StgToCmm, the actually used info tables are recorded in an IORef, this
is important as it's hard to predict beforehand what code generation will do
and which ids will end up in the generated program.
3. During code generation, a mapping from the info table to the statically
determined location is emitted which can then be queried at runtime by
various tools.

-- Giving Source Locations to Closures

At the moment thunk and constructor closures are added to the map. This information
is collected in the `InfoTableProvMap` which provides a mapping from:

1. Data constructors to a list of where they are used.
2. `Name`s and where they originate from.

During the CoreToStg phase, this map is populated whenever something is turned into
a StgRhsClosure or an StgConApp. The current source position is recorded
depending on the location indicated by the surrounding SourceNote.

The functions which add information to the map are `recordStgIdPosition` and
`incDc`.

When the -fdistinct-constructor-tables` flag is turned on then every
usage of a data constructor gets its own distinct info table. This is orchestrated
in `collectExpr` where an incrementing number is used to distinguish each
occurrence of a data constructor.

-- StgToCmm

The info tables which are actually used in the generated program are recorded during the
conversion from STG to Cmm. The used info tables are recorded in the `emitProc` function.
All the used info tables are recorded in the `cgs_used_info` field. This step
is necessary because when the information about names is collected in the previous
phase it's unpredictable about which names will end up needing info tables. If
you don't record which ones are actually used then you end up generating code
which references info tables which don't exist.

-- Code Generation

The output of these two phases is combined together during code generation.
A C stub is generated which
creates the static map from info table pointer to the information about where that
info table was created from. This is created by `ipInitCode` in the same manner as a
C stub is generated for cost centres.

This information can be consumed in two ways.

1. The complete mapping is emitted into the eventlog so that external tools such
as eventlog2html can use the information with the heap profile by info table mode.
2. The `lookupIPE` function can be used via the `whereFrom#` primop to introspect
information about a closure in a running Haskell program.

Note [Distinct Info Tables for Constructors]

In the old times, each usage of a data constructor used the same info table.
This made it impossible to distinguish which actual usuage of a data constructor was
contributing primarily to the allocation in a program. Using the `-fdistinct-info-tables` flag you
can cause code generation to generate a distinct info table for each usage of
a constructor. Then, when inspecting the heap you can see precisely which usage of a constructor
was responsible for each allocation.

-}
