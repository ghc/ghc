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

module GHC.Stg.InferTags.Rewrite
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
import GHC.Core (AltCon(..))
import GHC.Core.Type

import GHC.Stg.Utils
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)

import GHC.Data.Maybe
import GHC.Utils.Monad
import GHC.Utils.Panic

import Control.Monad

import GHC.Stg.InferTags.Types

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

rewriteTopBinds :: [InferStgTopBinding] -> AM [TgStgTopBinding]
rewriteTopBinds binds = mapM (rewriteTop) binds

rewriteTop :: InferStgTopBinding -> AM TgStgTopBinding
rewriteTop (StgTopStringLit v s) = return $! (StgTopStringLit v s)
rewriteTop      (StgTopLifted bind)  = do
    (StgTopLifted . fst) <$!> (rewriteBinds bind)

-- For bot level binds, the wrapper is guaranteed to be `id`
rewriteBinds :: InferStgBinding -> AM (TgStgBinding, TgStgExpr -> TgStgExpr)
rewriteBinds (StgNonRec v rhs) = do
        (!rhs, wrapper) <-  rewriteRhs v rhs
        return $! (StgNonRec v rhs, wrapper)
rewriteBinds (StgRec binds) =do
        (rhss, wrappers) <- unzip <$> mapM (uncurry rewriteRhs) binds
        let wrapper = foldl1 (.) wrappers
        return $! (mkRec rhss, wrapper)
  where
    mkRec :: [TgStgRhs] -> TgStgBinding
    mkRec rhss = StgRec (zip (map fst binds) rhss)

-- Rewrite a RHS, the rewriteFlag tells us weither or not the RHS is in a context in which
-- we can avoid turning the RhsCon into a closure. (e.g. for top level bindings)
rewriteRhs :: Id -> InferStgRhs -> AM (TgStgRhs, TgStgExpr -> TgStgExpr)
rewriteRhs _binding (StgRhsCon (node_id,rewriteFlag) ccs con args) = {-# SCC rewriteRhs_ #-} do
    node <- getNode node_id


    -- Look up the nodes representing the constructor arguments.
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    -- tagInfo <- lookupNodeResult node_id
    -- pprTraceM "rewriteRhsCon" $ ppr _binding <+> ppr tagInfo
    -- pprTraceM "rewriteConApp" $ ppr con <+> vcat [
    --     text "args" <+> ppr args,
    --     text "tagInfo" <+> ppr tagInfo,
    --     text "fieldInfos" <+> ppr fieldInfos
    --     -- text "strictIndices" <+> ppr strictIndices,
    --     -- text "needsEval" <+> ppr needsEval,
    --     -- text "evalArgs" <+> ppr evalArgs
    --     ]

    -- Filter out non-strict fields.
    let strictFields =
            getStrictConArgs con (zip args fieldInfos) :: [(StgArg,EnterLattice)] -- (nth-argument, tagInfo)
    -- Filter out already tagged arguments.
    let needsEval = map fst . --get the actual argument
                        filter (not . hasOuterTag . snd) $ -- arg known-tagged
                        strictFields :: [StgArg]
    let evalArgs = [v | StgVarArg v <- needsEval] :: [Id]

    if (null evalArgs)
        then return $! (StgRhsCon noExtFieldSilent ccs con args, id)
        else do
            -- pprTraceM "Creating seqs for " $ ppr _binding <+> ppr node_id

            evaldArgs <- mapM mkLocalArgId evalArgs -- Create case binders
            let varMap = zip evalArgs evaldArgs -- Match them up with original ids
            let updateArg (StgLitArg lit) = (StgLitArg lit)
                updateArg (StgVarArg v)
                    | Just v' <- lookup v varMap
                    = StgVarArg v'
                    | otherwise = StgVarArg v
            let evaldConArgs = map updateArg args
            -- At this point iff we have:
            -- * possibly untagged arguments to strict fields
            -- * maybeClosure as flag
            -- Then we return a RhsClosure, otherwise we return a wrapper
            -- which will evaluate the arguments first when applied to an expression.
            if rewriteFlag == MaybeClosure
                then do
                    conExpr <- mkSeqs evalArgs con args (panic "mkSeqs should not need to provide types")
                    return $! (StgRhsClosure noExtFieldSilent ccs ReEntrant [] $! conExpr, id)
                else do
                    let evalExpr expr = foldr (\(v, vEvald) e -> mkSeq v vEvald e) expr varMap
                    return $! ((StgRhsCon noExtFieldSilent ccs con evaldConArgs), evalExpr)
rewriteRhs _binding (StgRhsClosure ext ccs flag args body) = do
    pure (,) <*>
        (StgRhsClosure ext ccs flag args <$> rewriteExpr False body) <*>
        pure id

type IsScrut = Bool

rewriteExpr :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteExpr _ (e@StgCase {})          = rewriteCase e
rewriteExpr _ (e@StgLet {})           = rewriteLet e
rewriteExpr _ (e@StgLetNoEscape {})   = rewriteLetNoEscape e
rewriteExpr isScrut (StgTick t e)     = StgTick t <$!> rewriteExpr isScrut e
rewriteExpr _ e@(StgConApp {})        = rewriteConApp e

rewriteExpr isScrut e@(StgApp {})     = rewriteApp isScrut e
rewriteExpr _ (StgLit lit)           = return $! (StgLit lit)
rewriteExpr _ (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty)
rewriteExpr _ (StgLam {}) = error "Invariant violated: No lambdas in STG representation."

rewriteCase :: InferStgExpr -> AM TgStgExpr
rewriteCase (StgCase scrut bndr alt_type alts) =
    pure StgCase <*>
        rewriteExpr True scrut <*>
        pure bndr <*>
        pure alt_type <*>
        mapM rewriteAlt alts

rewriteCase _ = panic "Impossible: nodeCase"

rewriteAlt :: InferStgAlt -> AM TgStgAlt
rewriteAlt (altCon, bndrs, rhs) = do
    !rhs' <- rewriteExpr False rhs
    return $! (altCon, bndrs, rhs')

rewriteLet :: InferStgExpr -> AM TgStgExpr
rewriteLet (StgLet xt bind expr) = do
    (!bind', !wrapper) <- rewriteBinds bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLet xt bind' expr')
rewriteLet _ = panic "Impossible"

rewriteLetNoEscape :: InferStgExpr -> AM TgStgExpr
rewriteLetNoEscape (StgLetNoEscape xt bind expr) = do
    (!bind', wrapper) <- rewriteBinds bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLetNoEscape xt bind' expr')
rewriteLetNoEscape _ = panic "Impossible"

rewriteConApp :: InferStgExpr -> AM TgStgExpr
rewriteConApp (StgConApp nodeId con args tys) = do
    node <- getNode nodeId
    -- We look at the INPUT because the output of this node will always have tagged
    -- strict fields in the end.
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    let strictIndices = getStrictConArgs con (zip fieldInfos args) :: [(EnterLattice, StgArg)]
    let needsEval = map snd . filter (not . hasOuterTag . fst) $ strictIndices :: [StgArg]
    let evalArgs = [v | StgVarArg v <- needsEval] :: [Id]
    if (not $ null evalArgs)
        then do
            -- pprTraceM "Creating conAppSeqs for " $ ppr nodeId <+> parens ( ppr evalArgs ) -- <+> parens ( ppr fieldInfos )
            mkSeqs evalArgs con args tys
        else return $! (StgConApp noExtFieldSilent con args tys)

rewriteConApp _ = panic "Impossible"

rewriteApp :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteApp True (StgApp nodeId f args)
    | null args = do
    tagInfo <- lookupNodeResult nodeId
    let !enter = (extInfo $ enterInfo tagInfo)
    return $! StgApp enter f args
  where
    extInfo AlwaysEnter       = -- pprTrace "alwaysEnter" (ppr f)
                                --   StgSyn.AlwaysEnter
                                -- Reenters evaluated closures too often
                                  StgSyn.MayEnter
    extInfo NeverEnter        = StgSyn.NoEnter
    extInfo MaybeEnter        = StgSyn.MayEnter
    extInfo NoValue          = StgSyn.MayEnter
    extInfo UndetEnterInfo    = StgSyn.MayEnter

rewriteApp _ (StgApp _ f args) = return $ StgApp MayEnter f args -- TODO? Also apply here?
rewriteApp _ _ = panic "Impossible"

----------------------------------------------
-- Deal with exporting tagging information

_exportTaggedness :: [(Id,NodeId)] -> AM [(Id, EnterLattice)]
_exportTaggedness xs = mapMaybeM export xs
    where
        export (v,nid)
            | isInternalName (idName v)
            = return Nothing
            | isUnliftedType (idType v)
            = return Nothing
            | otherwise
            = do
                !res <- lookupNodeResult nid
                return $ Just (v,res)

-- We would ideally replace ALL references to the evaluatee with the evaluted binding.
-- But for now we don't.
mkSeq :: Id -> Id -> TgStgExpr -> TgStgExpr
mkSeq id bndr !expr =
    -- pprTrace "mkSeq" (ppr (id,bndr)) $
    let altTy = mkStgAltType bndr [(DEFAULT, [], panic "Not used")]
    in
    StgCase (StgApp MayEnter id []) bndr altTy [(DEFAULT, [], expr)]

-- Create a ConApp which is guaranteed to evaluate the given ids.
mkSeqs :: [Id] -> DataCon -> [StgArg] -> [Type] -> AM TgStgExpr
mkSeqs untaggedIds con args tys = do
    argMap <- mapM (\arg -> (arg,) <$> mkLocalArgId arg ) untaggedIds :: AM [(InId, OutId)]
    -- mapM_ (pprTraceM "Forcing strict args before allocation:" . ppr) argMap
    let taggedArgs
            = map   (\v -> case v of
                        StgVarArg v' -> StgVarArg $ fromMaybe v' $ lookup v' argMap
                        lit -> lit)
                    args

    let conBody = StgConApp noExtFieldSilent con taggedArgs tys
    let body = foldr (\(v,bndr) expr -> mkSeq v bndr expr) conBody argMap
    return $! body

mkLocalArgId :: Id -> AM Id
mkLocalArgId id = do
    u <- getUniqueM
    return $! setIdUnique (localiseId id) u

