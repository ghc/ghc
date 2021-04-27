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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg -ddump-cmm -ddump-asm -ddump-stg-final #-}
{-# OPTIONS_GHC -dsuppress-coercions -dno-suppress-type-signatures -dno-suppress-module-prefixes #-}
-- {-# OPTIONS_GHC -fprof-auto #-}
{-# OPTIONS_GHC -ticky -ticky-allocd -ticky-dyn-thunk #-}
{-# OPTIONS_GHC -dsuppress-ticks #-}

-- Node descriptions are not always present.
-- #if defined(DEBUG)
{-# OPTIONS_GHC -Wno-missing-fields #-}
-- #endif

{-# OPTIONS_GHC -Wno-unused-imports #-}
module GHC.Stg.InferTags.Rewrite (rewriteTopBinds)
where

#include "HsVersions.h"

#if defined(DEBUG)
#define WITH_NODE_DESC
#endif

import GHC.Prelude

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Unique.Supply

import GHC.Core.DataCon
import GHC.Core (AltCon(..), Alt(..), )
import GHC.Core.Type

import GHC.Stg.Utils
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)

import GHC.Data.Maybe
import GHC.Utils.Monad
import GHC.Utils.Panic

import Control.Monad

import GHC.Stg.InferTags -- .Types

import GHC.Driver.Ppr
import GHC.Utils.Outputable

import GHC.Utils.Monad.State
import GHC.Utils.Misc
import GHC.Types.Unique.FM
import GHC.Types.Name

import GHC.Stg.InferTags.Types
import GHC.Stg.DepAnal
import GHC.Unit.Types (Module)
import GHC.StgToCmm.Types
import GHC.Types.Var.Set

type RM = State (UniqFM Id TagSig, UniqSupply, Module)
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

-}


--------------------------------
-- Utilities
--------------------------------

instance MonadUnique RM where
    getUniqueSupplyM = do
        (m, us, mod) <- get
        let (us1, us2) = splitUniqSupply us
        put (m,us2,mod)
        return us1

type TaggedSet = VarSet

getMap :: RM (UniqFM Id TagSig)
getMap = fstOf3 <$> get

setMap :: (UniqFM Id TagSig) -> RM ()
setMap m = do
    (_,us,mod) <- get
    put (m, us,mod)

getMod :: RM Module
getMod = thdOf3 <$> get

addBind :: GenStgBinding 'TaggedSimon -> RM ()
addBind (StgNonRec (id, tag) _) = do
    s <- getMap
    -- pprTraceM "AddBind" (ppr id)
    setMap $ addToUFM s id tag
    return ()
addBind (StgRec binds) = do
    let (bnds,_rhss) = unzip binds
    !s <- getMap
    -- pprTraceM "AddBinds" (ppr $ map fst bnds)
    setMap $! addListToUFM s bnds

addBinder :: (Id,TagSig) -> RM ()
addBinder (id,sig) = do
    !s <- getMap
    setMap $ addToUFM s id sig
    return ()

addArg :: StgArg -> RM ()
addArg (StgLitArg _) = return ()
addArg (StgVarArg v) = do
    !s <- getMap
    setMap $! addToUFM s v (TagSig 0 TagDunno)
    return ()

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


rewriteTopBinds :: Module -> UniqSupply -> [GenStgTopBinding 'TaggedSimon] -> [TgStgTopBinding]
rewriteTopBinds mod us binds =
    evalState   (mapM (rewriteTop) $ binds)
                (mempty, us, mod)

rewriteTop :: InferStgTopBinding -> RM TgStgTopBinding
rewriteTop (StgTopStringLit v s) = return $! (StgTopStringLit v s)
rewriteTop      (StgTopLifted bind)  = do
    addBind bind
    (StgTopLifted . fst) <$!> (rewriteBinds bind)

-- For top level binds, the wrapper is guaranteed to be `id`
rewriteBinds :: InferStgBinding -> RM (TgStgBinding, TgStgExpr -> TgStgExpr)
rewriteBinds b@(StgNonRec v rhs) = do
        (!rhs, wrapper) <-  rewriteRhs v rhs
        return $! (StgNonRec (fst v) rhs, wrapper)
rewriteBinds b@(StgRec binds) = do
        addBind b
        (rhss, wrappers) <- unzip <$> mapM (uncurry rewriteRhs) binds
        let wrapper = foldl1 (.) wrappers
        return $! (mkRec rhss, wrapper)
  where
    mkRec :: [TgStgRhs] -> TgStgBinding
    mkRec rhss = StgRec (zip (map (fst . fst) binds) rhss)

-- Rewrite a RHS, the rewriteFlag tells us weither or not the RHS is in a context in which
-- we can avoid turning the RhsCon into a closure. (e.g. for top level bindings)
rewriteRhs :: (Id,TagSig) -> InferStgRhs -> RM (TgStgRhs, TgStgExpr -> TgStgExpr)
rewriteRhs (_id, tagSig) (StgRhsCon (node_id) ccs con cn ticks args) = {-# SCC rewriteRhs_ #-} do
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
        then return $! (StgRhsCon noExtFieldSilent ccs con cn ticks args, id)
        else do
            -- pprTraceM "CreatingSeqs for " $ ppr _id <+> ppr node_id

            evaldArgs <- mapM mkLocalArgId evalArgs -- Create case binders
            let varMap = zip evalArgs evaldArgs -- Match them up with original ids
            let updateArg (StgLitArg lit) = (StgLitArg lit)
                updateArg (StgVarArg v)
                    | Just v' <- lookup v varMap
                    = StgVarArg v'
                    | otherwise = StgVarArg v
            let evaldConArgs = map updateArg args
            -- At this point iff we have:
            --  * possibly untagged arguments to strict fields
            --  * and Dunno as tag signature
            -- Then we return a RhsClosure, otherwise we return a wrapper
            -- which will evaluate the arguments first when applied to an expression.
            if not (isTaggedSig tagSig) --rewriteFlag == MaybeClosure
                then do -- Turn the rhs into a closure that evaluates the arguments to the strict fields
                    conExpr <- mkSeqs evalArgs con cn args (panic "mkSeqs should not need to provide types")
                    return $! (StgRhsClosure noExtFieldSilent ccs ReEntrant [] $! conExpr, id)
                else do -- Return a case expression that will evaluate the arguments.
                    let evalExpr expr = foldr (\(v, vEvald) e -> mkSeq v vEvald e) expr varMap
                    return $! ((StgRhsCon noExtFieldSilent ccs con cn ticks evaldConArgs), evalExpr)
rewriteRhs _binding (StgRhsClosure ext ccs flag args body) = do
    mapM_ addBinder  args
    pure (,) <*>
        (StgRhsClosure ext ccs flag (map fst args) <$> rewriteExpr False body) <*>
        pure id

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
rewriteCase (StgCase scrut bndr alt_type alts) = do
    addBinder bndr
    pure StgCase <*>
        rewriteExpr True scrut <*>
        pure (fst bndr) <*>
        pure alt_type <*>
        mapM rewriteAlt alts

rewriteCase _ = panic "Impossible: nodeCase"

rewriteAlt :: InferStgAlt -> RM TgStgAlt
rewriteAlt (altCon, bndrs, rhs) = do
    mapM_ addBinder bndrs
    !rhs' <- rewriteExpr False rhs
    return $! (altCon, map fst bndrs, rhs')

rewriteLet :: InferStgExpr -> RM TgStgExpr
rewriteLet (StgLet xt bind expr) = do
    (!bind', !wrapper) <- rewriteBinds bind
    addBind bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLet xt bind' expr')
rewriteLet _ = panic "Impossible"

rewriteLetNoEscape :: InferStgExpr -> RM TgStgExpr
rewriteLetNoEscape (StgLetNoEscape xt bind expr) = do
    (!bind', wrapper) <- rewriteBinds bind
    addBind bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLetNoEscape xt bind' expr')
rewriteLetNoEscape _ = panic "Impossible"

rewriteConApp :: InferStgExpr -> RM TgStgExpr
rewriteConApp (StgConApp nodeId con cn args tys) = do
    -- node <- getNode nodeId
    -- We look at the INPUT because the output of this node will always have tagged
    -- strict fields in the end.
    fieldInfos <- mapM isArgTagged args
    let strictIndices = getStrictConArgs con (zip fieldInfos args) :: [(Bool, StgArg)]
    let needsEval = map snd . filter (not . fst) $ strictIndices :: [StgArg]
    let evalArgs = [v | StgVarArg v <- needsEval] :: [Id]
    if (not $ null evalArgs)
        then do
            -- pprTraceM "Creating conAppSeqs for " $ ppr nodeId <+> parens ( ppr evalArgs ) -- <+> parens ( ppr fieldInfos )
            mkSeqs evalArgs con cn args tys
        else return $! (StgConApp noExtFieldSilent con cn args tys)

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
    let taggedArgs
            = map   (\v -> case v of
                        StgVarArg v' -> StgVarArg $ fromMaybe v' $ lookup v' argMap
                        lit -> lit)
                    args

    let conBody = StgConApp noExtFieldSilent con cn taggedArgs tys
    let body = foldr (\(v,bndr) expr -> mkSeq v bndr expr) conBody argMap
    return $! body

-- mkLocalArgId :: Id -> RM Id
-- mkLocalArgId id = do
--     u <- getUniqueM
--     return $! setIdUnique (localiseId id) u

