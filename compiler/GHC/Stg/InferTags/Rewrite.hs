--
-- Copyright (c) 2019 Andreas Klebinger
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module GHC.Stg.InferTags.Rewrite (rewriteTopBinds)
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Unique.Supply
import GHC.Types.Unique.FM
import GHC.Types.RepType
import GHC.Unit.Types (Module)

import GHC.Core.DataCon
import GHC.Core (AltCon(..) )
import GHC.Core.Type

import GHC.StgToCmm.Types

import GHC.Stg.Utils
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)

import GHC.Data.Maybe
import GHC.Utils.Panic

import GHC.Utils.Outputable
import GHC.Utils.Monad.State
import GHC.Utils.Misc

import GHC.Stg.InferTags.Types

import Control.Monad
import Data.Coerce


-- import GHC.Driver.Ppr

newtype RM a = RM { unRM :: (State (UniqFM Id TagSig, UniqSupply, Module) a) }
    deriving (Functor, Monad, Applicative)
------------------------------------------------------------
-- Add cases around strict fields where required.
------------------------------------------------------------

{-
The idea is simple:
* We traverse the STG AST looking for constructor allocation.
* For any allocation we check if there are strict fields.
* For any strict field we check if the argument is known to be tagged.
* If it's not, we wrap the whole thing in a case, which will force the
  argument before allocation.

This is described in detail in Note [Strict field invariant].

-}


--------------------------------
-- Utilities
--------------------------------

instance MonadUnique RM where
    getUniqueSupplyM = RM $ do
        (m, us, mod) <- get
        let (us1, us2) = splitUniqSupply us
        (put) (m,us2,mod)
        return us1

-- type TaggedSet = VarSet

getMap :: RM (UniqFM Id TagSig)
getMap = RM $ (fstOf3 <$> get)

setMap :: (UniqFM Id TagSig) -> RM ()
setMap m = RM $ do
    (_,us,mod) <- get
    put (m, us,mod)

getMod :: RM Module
getMod = RM $ (thdOf3 <$> get)

withBind :: GenStgBinding 'InferTaggedBinders -> RM a -> RM a
withBind (StgNonRec (id, tag) _) cont = coerce $ do
    oldMap <- getMap
    -- pprTraceM "AddBind" (ppr id)
    setMap $ addToUFM oldMap id tag
    a <- cont
    setMap oldMap
    return a
withBind (StgRec binds) cont = coerce $ do
    let (bnds,_rhss) = unzip binds
    !oldMap <- getMap
    -- pprTraceM "AddBinds" (ppr $ map fst bnds)
    setMap $! addListToUFM oldMap bnds
    a <- cont
    setMap oldMap
    return a



addBind :: GenStgBinding 'InferTaggedBinders -> RM ()
addBind (StgNonRec (id, tag) _) = coerce $ do
    s <- getMap
    -- pprTraceM "AddBind" (ppr id)
    setMap $ addToUFM s id tag
    return ()
addBind (StgRec binds) = coerce $ do
    let (bnds,_rhss) = unzip binds
    !s <- getMap
    -- pprTraceM "AddBinds" (ppr $ map fst bnds)
    setMap $! addListToUFM s bnds

withBinder :: (Id, TagSig) -> RM a -> RM a
withBinder (id,sig) cont = do
    oldMap <- getMap
    setMap $ addToUFM oldMap id sig
    a <- cont
    setMap oldMap
    return a

withBinders :: [(Id, TagSig)] -> RM a -> RM a
withBinders sigs cont = do
    oldMap <- getMap
    setMap $ addListToUFM oldMap sigs
    a <- cont
    setMap oldMap
    return a

isTagged :: Id -> RM Bool
isTagged v = do
    this_mod <- getMod
    case nameIsLocalOrFrom this_mod (idName v) of
        True
            | isUnliftedType (idType v)
            -> return True
            | otherwise -> do -- Local binding
                !s <- getMap
                let !sig = lookupWithDefaultUFM s (pprPanic "unknown Id:" (ppr v)) v
                return $ case sig of
                    TagSig _arity info ->
                        case info of
                            TagDunno -> False
                            TagProper -> True
                            TagTagged -> True
                            TagTuple _ -> True -- Consider unboxed tuples tagged
        False -- Imported
            | Just con <- (isDataConWorkId_maybe v)
            , isNullaryRepDataCon con
            -> return True
            | Just lf_info <- idLFInfo_maybe v
            -> return $ case lf_info of
                    -- Function, applied not entered.
                    LFReEntrant {}
                        -> True
                    -- Thunks need to be entered.
                    LFThunk {}
                        -> False
                    -- Constructors, already tagged.
                    LFCon {}
                        -- If we ever bind the fields we can infer the tags
                        -- based on the fields strictness. So a flat lattice
                        -- is fine.
                        -> True
                    LFUnknown {}
                        -> False
                    LFUnlifted {}
                        -> True
                    -- Shouldn't be possible. I don't think we can export letNoEscapes
                    LFLetNoEscape {}
                        -> True

            | otherwise
            -> return False


isArgTagged :: StgArg -> RM Bool
isArgTagged (StgLitArg _) = return True
isArgTagged (StgVarArg v) = isTagged v

mkLocalArgId :: Id -> RM Id
mkLocalArgId id = do
    !u <- getUniqueM
    return $! setIdUnique (localiseId id) u

---------------------------
-- Actual rewrite pass
---------------------------


rewriteTopBinds :: Module -> UniqSupply -> [GenStgTopBinding 'InferTaggedBinders] -> [TgStgTopBinding]
rewriteTopBinds mod us binds =
    let doBinds = mapM rewriteTop binds

    in evalState (unRM doBinds) (mempty, us, mod)

rewriteTop :: InferStgTopBinding -> RM TgStgTopBinding
rewriteTop (StgTopStringLit v s) = return $! (StgTopStringLit v s)
rewriteTop (StgTopLifted bind)   = do
    -- Top level bindings can, and must remain in scope
    addBind bind
    (StgTopLifted) <$!> (rewriteBinds bind)

-- For top level binds, the wrapper is guaranteed to be `id`
rewriteBinds :: InferStgBinding -> RM (TgStgBinding)
rewriteBinds (StgNonRec v rhs) = do
        (!rhs) <-  rewriteRhs v rhs
        return $! (StgNonRec (fst v) rhs)
rewriteBinds b@(StgRec binds) =
    -- Bring sigs of binds into scope for all rhss
    withBind b $ do
        (rhss) <- mapM (uncurry rewriteRhs) binds
        return $! (mkRec rhss)
        where
            mkRec :: [TgStgRhs] -> TgStgBinding
            mkRec rhss = StgRec (zip (map (fst . fst) binds) rhss)

-- Rewrite a RHS, the rewriteFlag tells us weither or not the RHS is in a context in which
-- we can avoid turning the RhsCon into a closure. (e.g. for top level bindings)
rewriteRhs :: (Id,TagSig) -> InferStgRhs
           -> RM (-- Bool, -- Should we turn it into an updateable closure
                TgStgRhs)
rewriteRhs (_id, tagSig) (StgRhsCon ccs con cn ticks args) = {-# SCC rewriteRhs_ #-} do
    -- pprTraceM "rewriteRhs" (ppr _id)

    -- Look up the nodes representing the constructor arguments.
    fieldInfos <- mapM isArgTagged args

    -- Filter out non-strict fields.
    let strictFields =
            getStrictConArgs con (zip args fieldInfos) :: [(StgArg,Bool)] -- (nth-argument, tagInfo)
    -- Filter out already tagged arguments.
    let needsEval = map fst . --get the actual argument
                        filter (not . snd) $ -- Keep untagged (False) elements.
                        strictFields :: [StgArg]
    let evalArgs = [v | StgVarArg v <- needsEval] :: [Id]

    if (null evalArgs)
        then return $! (StgRhsCon ccs con cn ticks args)
        else do
            --assert not (isTaggedSig tagSig)
            -- pprTraceM "CreatingSeqs for " $ ppr _id <+> ppr node_id

            -- At this point iff we have:
            --  * possibly untagged arguments to strict fields
            --  * then inference marked the binder as tag Dunno
            -- So we convert it into a RhsClosure.
            -- which will evaluate the arguments first when applied to an expression.
            -- Turn the rhs into a closure that evaluates the arguments to the strict fields
            conExpr <- mkSeqs evalArgs con cn args (panic "mkSeqs should not need to provide types")
            return $! (StgRhsClosure noExtFieldSilent ccs Updatable [] $! conExpr)
rewriteRhs _binding (StgRhsClosure ext ccs flag args body) = do
    -- mapM_ addBinder  args
    withBinders args $ do
        closure <- StgRhsClosure ext ccs flag (map fst args) <$> rewriteExpr False body
        return (closure)

type IsScrut = Bool

rewriteExpr :: IsScrut -> InferStgExpr -> RM TgStgExpr
rewriteExpr _ (e@StgCase {})          = rewriteCase e
rewriteExpr _ (e@StgLet {})           = rewriteLet e
rewriteExpr _ (e@StgLetNoEscape {})   = rewriteLetNoEscape e
rewriteExpr isScrut (StgTick t e)     = StgTick t <$!> rewriteExpr isScrut e
rewriteExpr _ e@(StgConApp {})        = rewriteConApp e

rewriteExpr isScrut e@(StgApp {})     = rewriteApp isScrut e
rewriteExpr _ (StgLit lit)           = return $! (StgLit lit)
rewriteExpr _ (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty)

rewriteCase :: InferStgExpr -> RM TgStgExpr
rewriteCase (StgCase scrut bndr alt_type alts) =
    withBinder bndr $ do
        pure StgCase <*>
            rewriteExpr True scrut <*>
            pure (fst bndr) <*>
            pure alt_type <*>
            mapM rewriteAlt alts

rewriteCase _ = panic "Impossible: nodeCase"

rewriteAlt :: InferStgAlt -> RM TgStgAlt
rewriteAlt (altCon, bndrs, rhs) = do
    withBinders bndrs $ do
        !rhs' <- rewriteExpr False rhs
        return $! (altCon, map fst bndrs, rhs')

rewriteLet :: InferStgExpr -> RM TgStgExpr
rewriteLet (StgLet xt bind expr) = do
    (!bind') <- rewriteBinds bind
    withBind bind $ do
        !expr' <- rewriteExpr False expr
        return $! (StgLet xt bind' expr')
rewriteLet _ = panic "Impossible"

rewriteLetNoEscape :: InferStgExpr -> RM TgStgExpr
rewriteLetNoEscape (StgLetNoEscape xt bind expr) = do
    (!bind') <- rewriteBinds bind
    withBind bind $ do
        !expr' <- rewriteExpr False expr
        return $! (StgLetNoEscape xt bind' expr')
rewriteLetNoEscape _ = panic "Impossible"

rewriteConApp :: InferStgExpr -> RM TgStgExpr
rewriteConApp (StgConApp con cn args tys) = do
    -- We check if the strict field arguments are already known to be tagged.
    -- If not we evaluate them first.
    fieldInfos <- mapM isArgTagged args
    let strictIndices = getStrictConArgs con (zip fieldInfos args) :: [(Bool, StgArg)]
    let needsEval = map snd . filter (not . fst) $ strictIndices :: [StgArg]
    let evalArgs = [v | StgVarArg v <- needsEval] :: [Id]
    if (not $ null evalArgs)
        then do
            -- pprTraceM "Creating conAppSeqs for " $ ppr nodeId <+> parens ( ppr evalArgs ) -- <+> parens ( ppr fieldInfos )
            mkSeqs evalArgs con cn args tys
        else return $! (StgConApp con cn args tys)

rewriteConApp _ = panic "Impossible"

rewriteApp :: IsScrut -> InferStgExpr -> RM TgStgExpr
rewriteApp True (StgApp _nodeId f args)
    | null args = do
        tagInfo <- isTagged f
        let !enter = (extInfo $ tagInfo)
        return $! StgApp enter f args
  where
    extInfo True        = StgSyn.NoEnter
    extInfo False       = StgSyn.MayEnter
    -- extInfo MaybeEnter        = StgSyn.MayEnter
    -- extInfo NoValue          = StgSyn.MayEnter
    -- extInfo UndetEnterInfo    = StgSyn.MayEnter

rewriteApp _ (StgApp _ f args) = return $ StgApp MayEnter f args
rewriteApp _ _ = panic "Impossible"


-- We would ideally replace ALL references to the evaluatee with the evaluted binding.
-- But for now we don't bother.
mkSeq :: Id -> Id -> TgStgExpr -> TgStgExpr
mkSeq id bndr !expr =
    -- pprTrace "mkSeq" (ppr (id,bndr)) $
    let altTy = mkStgAltTypeFromStgAlts bndr [(DEFAULT, [], panic "Not used")]
    in
    StgCase (StgApp MayEnter id []) bndr altTy [(DEFAULT, [], expr)]

-- Create a ConApp which is guaranteed to evaluate the given ids.
mkSeqs :: [Id] -> DataCon -> ConstructorNumber -> [StgArg] -> [Type] -> RM TgStgExpr
mkSeqs untaggedIds con cn args tys = do
    argMap <- mapM (\arg -> (arg,) <$> mkLocalArgId arg ) untaggedIds :: RM [(InId, OutId)]
    -- mapM_ (pprTraceM "Forcing strict args before allocation:" . ppr) argMap
    let taggedArgs :: [StgArg]
            = map   (\v -> case v of
                        StgVarArg v' -> StgVarArg $ fromMaybe v' $ lookup v' argMap
                        lit -> lit)
                    args

    let conBody = StgConApp con cn taggedArgs tys
    let body = foldr (\(v,bndr) expr -> mkSeq v bndr expr) conBody argMap
    return $! body

-- Out of all arguments passed at runtime only return these ending up in a
-- strict field
getStrictConArgs :: DataCon -> [a] -> [a]
getStrictConArgs con args
    -- These are always lazy in their arguments.
    | isUnboxedTupleDataCon con = []
    | isUnboxedSumDataCon con = []
    -- For proper data cons we have to check.
    | otherwise =
        [ arg | (arg,MarkedStrict)
                    <- zipEqual "getStrictConArgs"
                                args
                                (dataConRuntimeRepStrictness con)]
