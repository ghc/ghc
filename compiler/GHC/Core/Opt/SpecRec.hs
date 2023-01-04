{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Transfer specrec pragmas from functions having such a pragma
-- to functions calling such functions.
module GHC.Core.Opt.SpecRec
    ( transferSpecRecs
    ) where

import GHC.Prelude


import GHC.Types.Basic
import GHC.Driver.Session
import GHC.Types.Name hiding (varName)
import GHC.Types.Id
import GHC.Unit.Module.ModGuts
import GHC.Types.Var.Set
import GHC.Types.Name.Env
import GHC.Unit.Types
import GHC.Core
import GHC.Core.Rules
import GHC.Core.FVs
import GHC.Utils.Outputable

import Data.Graph
import GHC.Utils.Monad.State.Strict
import Control.Monad
import Data.Maybe
{-
-- We need to transfer the pragma in these cases:

{-# SPECREC foo #-}
foo = ...

We transfer the pragma if foo is mentioned in:
* The RHS of a function
* The unfolding. -- TODO: Not needed after desugar?
-- TODO: Rules

-}

transferSpecRecs :: ModGuts -> ModGuts
-- transferSpecRecs _dflags guts = guts
transferSpecRecs guts =
  let env :: Env
      env = Env
        { thisModule = mg_module guts
        , orphanRules = mkRuleBase (mg_rules guts)
        }
  in guts { mg_binds = doCoreProgram env (mg_binds guts)
          }

-- bind_fvs (NonRec _ rhs) =
doCoreProgram :: Env -> CoreProgram -> CoreProgram
doCoreProgram env binds = flip evalState (SS mempty mempty) $ do
    -- pprTraceM "binds_in" $ vcat $ map ppr binds
    let sorted_binds = depAnal (map getName . bindersOf) bind_deps binds
    -- pprTraceM "binds_sorted" $ vcat $ map (ppr . bindersOf) sorted_binds
    done_binds <- doSccs env sorted_binds
    -- pprTraceM "binds_out" $ vcat $ map (ppr . bindersOf) done_binds
    return done_binds
    where
        bind_deps bind =
            let bs = bindersOf bind
                rhss = rhssOfBind bind
                rhss_fvs = exprsSomeFreeVarsList (const True) rhss
                unfs = map realIdUnfolding bs
                unf_rhs = catMaybes . map maybeUnfoldingTemplate $ unfs
                unf_fvs = exprsSomeFreeVarsList (const True) unf_rhs
                id_rules = concatMap idCoreRules bs
                id_rules_fvs = rulesSomeFreeVarsList (const True) id_rules
            in map getName $ unf_fvs ++ id_rules_fvs ++ unf_fvs ++ rhss_fvs

mcons :: Monad m => a -> m [a] -> m [a]
mcons x xs = liftM (x:) xs

doSccs :: Env -> [SCC CoreBind] -> M [CoreBind]
doSccs env binds = do
    bindss <- mapM (doScc env) binds
    pure $ concat bindss

doScc :: Env -> SCC CoreBind -> M [CoreBind]
doScc env (AcyclicSCC bind) = do
    (b,is_spec) <- doBind env bind
    when is_spec $ addSpecBinders $ bindersOf b
    pure [b]
doScc env (CyclicSCC binds) = do
    -- A bunch of binders which might refer to each other in a cyclic fashion via
    -- something like rules. So we must put specrec on all of them.
    (bs,is_specs) <- unzip <$> mapM (doBind env) binds
    when (or is_specs) $ addSpecBinders $ bindersOfBinds bs
    pure $ map setSpec bs

addSpecBinders :: [Id] -> M ()
addSpecBinders ids = do
    mapM_ addSpec ids
    mapM_ addDone ids
    return ()

doBind :: Env -> CoreBind -> M (CoreBind, Bool)
doBind env bind = do
    to_spec <- spec_set <$> get
    let bs = bindersOf bind
        rhss = rhssOfBind bind
        spec_id = any idHasSpecRec bs
        spec_rhs = any (is_spec_expr to_spec) rhss
        spec_unf = any (is_spec_unf to_spec . realIdUnfolding) bs
        spec_rules = any (is_spec_rules to_spec . idCoreRules) bs

    if spec_id || spec_rhs || spec_unf || spec_rules
        -- If the rhs, unfolding or a rule rhs mentions a spec-rec function
        -- we must make the function itself spec-rec
        then pure (setSpec bind, True)
        -- Otherwise another binding might still become spec-rec in the future
        else pure (bind, False)

    where
        is_spec_rules spec_set rules =
            let fvs = rulesRhsSomeFVs (\v -> isId v && idHasSpecRec v || elemVarSet v spec_set) rules
            in not (isEmptyVarSet fvs)

        is_spec_expr :: VarSet -> CoreExpr -> Bool
        is_spec_expr spec_set expr =
            let fvs = exprSomeFreeVars (\v -> isId v && idHasSpecRec v || elemVarSet v spec_set) expr
            in not (isEmptyVarSet fvs)

        is_spec_unf :: VarSet -> Unfolding -> Bool
        is_spec_unf spec_set unf = do
            case maybeUnfoldingSource unf of
                -- We already look at the rhs and the unf is the same
                Just VanillaSrc -> False
                _ -> case maybeUnfoldingTemplate unf of
                    Just unf_tmpl -> do
                        is_spec_expr spec_set unf_tmpl
                    Nothing -> False

        -- TODO: Properly set activation

setSpec :: Bind Id -> Bind Id
setSpec (NonRec b rhs) = NonRec (setHasSpecRec b (Just AlwaysActive)) rhs
setSpec (Rec pairs) = Rec $ map (\(b,rhs) -> (setHasSpecRec b (Just AlwaysActive), rhs)) pairs


-- doBind :: Env -> CoreBind -> M CoreBind
-- doBind env (NonRec b rhs) = NonRec b <$> doExpr (addParent b env) rhs
-- doBind env (Rec bs) = Rec <$> mapM doPair bs
--   where
--     doPair (b,rhs) = (b,) <$> doExpr (addParent b env) rhs

-- doExpr :: Env -> CoreExpr -> M CoreExpr
-- doExpr env e@(Var v)
--   | needsCallSiteCostCentre env v = do
--     let nameDoc :: SDoc
--         nameDoc = withUserStyle alwaysQualify DefaultDepth $
--           hcat (punctuate dot (map ppr (parents env))) <> parens (text "calling:" <> ppr v)

--         ccName :: CcName
--         ccName = mkFastString $ renderWithContext defaultSDocContext nameDoc
--     ccIdx <- getCCIndex' ccName
--     let count = countEntries env
--         span = case revParents env of
--           top:_ -> nameSrcSpan $ varName top
--           _     -> noSrcSpan
--         cc = NormalCC (ExprCC ccIdx) ccName (thisModule env) span
--         tick :: CoreTickish
--         tick = ProfNote cc count True
--     pure $ Tick tick e
--   | otherwise = pure e
-- doExpr _env e@(Lit _)       = pure e
-- doExpr env (f `App` x)      = App <$> doExpr env f <*> doExpr env x
-- doExpr env (Lam b x)        = Lam b <$> doExpr env x
-- doExpr env (Let b rhs)      = Let <$> doBind env b <*> doExpr env rhs
-- doExpr env (Case scrut b ty alts) =
--     Case <$> doExpr env scrut <*> pure b <*> pure ty <*> mapM doAlt alts
--   where
--     doAlt (Alt con bs rhs)  = Alt con bs <$> doExpr env rhs
-- doExpr env (Cast expr co)   = Cast <$> doExpr env expr <*> pure co
-- doExpr env (Tick t e)       = Tick t <$> doExpr env e
-- doExpr _env e@(Type _)      = pure e
-- doExpr _env e@(Coercion _)  = pure e

data SpecState = SS
    { spec_set :: !VarSet
    , spec_done :: !VarSet
    }

type M = State SpecState

addSpec :: Var -> M ()
addSpec v = do
    s <- get
    put $! s { spec_set = extendVarSet (spec_set s) v }

addDone :: Var -> M ()
addDone v = do
    s <- get
    put $! s { spec_done = extendVarSet (spec_done s) v }



data Env = Env
  { thisModule  :: Module
  , orphanRules :: RuleBase
  }

