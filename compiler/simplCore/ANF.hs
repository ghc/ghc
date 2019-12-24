{-# LANGUAGE OverloadedStrings, TupleSections #-}
module ANF ( anfProgram, anfExpression ) where

import GhcPrelude

import CoreMonad
import CoreSyn
import CoreUtils
    ( exprType )
import DList
    ( DList )
import qualified DList as DL
import IdInfo
    ( IdDetails( VanillaId ), vanillaIdInfo )
import MkCore
    ( mkCoreLet )
import Name
    ( mkSystemVarName )
import Type
    ( Type )
import UniqSupply
    ( getUniqueM )
import Var
    ( mkLocalVar )

-- import Control.Monad.Trans.State.Strict

{-
    Transforms Core into a modified form of ANF (that also requires that 
    lambdas have names) before the simplifier runs, as a quick and dirty way to
    ensure the demand analysis will apply to every function argument, which is
    needed for RULEs to ensure that they are being used linearly. This is a
    crucial invariant that is generally assumed, but one which GHC is currently
    entirely unaware of.
-}

anfProgram :: CoreProgram -> CoreM CoreProgram
anfProgram = mapM anfBind

anfBind :: CoreBind -> CoreM CoreBind
anfBind (NonRec b ex) = NonRec b <$> anfExpression ex
anfBind (Rec bs) = Rec <$> mapM go_rec bs
    where
        go_rec (b, ex) = (b,) <$> anfExpression ex

anfAlt :: CoreAlt -> CoreM CoreAlt
anfAlt (con, pats, body) = (con, pats,) <$> anfExpression body

-- Unlike the Danvy style, we currently don't care about the final expression
-- in the body being an `App` or a `return var`.
anfExpression :: CoreExpr -> CoreM CoreExpr
anfExpression = flip anfExpressionK return

generateName :: Type -> CoreM CoreBndr
generateName ty = do
    uniq <- getUniqueM
    let name = mkSystemVarName uniq "v"
    return (mkLocalVar VanillaId name ty vanillaIdInfo)

{-
    letBind acc rhs
 => let v = rhs in acc v
-}
letBind :: CoreExpr -> (CoreBndr -> CoreM CoreExpr) -> CoreM CoreExpr
letBind rhs acc = do
    var <- generateName (exprType rhs)
    let bind = NonRec var rhs
    inner <- acc var
    return $ mkCoreLet bind inner

-- 'letBind' generates the actual let as in the paper
anfExpressionK :: CoreExpr -> (CoreExpr -> CoreM CoreExpr) -> CoreM CoreExpr
anfExpressionK (App lhs rhs) acc =
    anfExpressionK lhs $ \v0 ->
        anfExpressionK rhs $ \v1 ->
            -- (lhs rhs) => let v = (lhs rhs) in (acc v)
            letBind (App v0 v1) (\var -> acc $ Var var)
anfExpressionK (Lam bind bod) acc = do
    bod' <- anfExpression bod
    letBind (Lam bind bod') (\var -> acc $ Var var) 
anfExpressionK (Let (NonRec bind rhs) bod) acc = do 
{-
Which order we do these corresponds to LTR or RTL evaluation.

Right now, we just want to not reorder things syntactically.

Derivation:
     let bind = rhs in bod
 <=> (\bind -> bod) rhs 
  => let v = (\bind -> bod) rhs in acc v
  => let v = (let bind = rhs in bod) in acc v
  => acc (let bind = rhs in bod)
-}
    anfExpressionK rhs $ \anf_rhs ->
        anfExpressionK bod $ \anf_exp ->
            acc $ Let (NonRec bind anf_rhs) anf_exp
anfExpressionK (Let (Rec bs) bod) acc = -- TODO: SCC check for variables
    anfExpressionK bod $ \anf_bod -> do
        bs' <- DL.toList . DL.concat <$> mapM collectRecBinders bs
        acc $ Let (Rec bs') anf_bod
anfExpressionK (Case scrut var ty alts) acc = do
    anfAlts <- mapM anfAlt alts
    anfExpressionK scrut $ \anf_scrut ->
        acc $ Case anf_scrut var ty anfAlts
anfExpressionK (Cast ex co) acc = do
    anfEx <- anfExpression ex
    letBind (Cast anfEx co) (\var -> acc $ Var var)
anfExpressionK (Tick tick ex) acc = do
    anfEx <- anfExpression ex
    letBind (Tick tick anfEx) (\var -> acc $ Var var)
anfExpressionK ex acc = acc ex

type DLBndrs = DList (CoreBndr, CoreExpr)

returnDL :: a -> CoreM (DList a)
returnDL = return . DL.singleton

returnDLs :: [a] -> CoreM (DList a)
returnDLs = return . DL.fromList

collectFromRecBinders :: (CoreBndr, CoreExpr) -> CoreM DLBndrs
collectFromRecBinders (bind, rhs) = collectRecBindersK rhs $ \anf_rhs ->
    returnDL (bind, anf_rhs)

collectRecBinders :: (CoreBndr, CoreExpr) -> CoreM DLBndrs
collectRecBinders (b, App lhs rhs) =
    collectRecBindersK lhs $ \v0 ->
        collectRecBindersK rhs $ \v1 ->
            returnDL (b, App v0 v1) -- (lhs rhs) => let v = (lhs rhs) in (acc v)
collectRecBinders (b, Lam bind bod) = do
    bod' <- anfExpression bod
    returnDL (b, Lam bind bod')
collectRecBinders (b, Let (NonRec bind rhs) bod) = do -- this is the same as `(\bind -> bod) rhs`
{-
    {-
        This is the simple case of let-flattening, the substituion is required
        to preserve lexical scoping. The `Rec` case is the same, but we only
        have to be concerned with one variable, rather than a set of variables. 
    -}
    bindUniq <- getUniqueM
    let bind' = setVarUnique bind bindUniq
        bodSubst = extendIdSubst emptySubst bind (Var bind')
        bod' = substExpr "collectRecBinders:LetNonRec" bodSubst bod
-}
    collectRecBindersK rhs $ \anf_rhs ->
        collectRecBindersK bod $ \anf_bod ->
            returnDLs
                [ (bind, anf_rhs)
                , (b, anf_bod)
                ]
collectRecBinders (b, (Let (Rec bs) body)) = do
{- Nothing to freshen.
    {-
        This is the more complex case of let-flattening.
    -}
    let
        freshenBndr :: (CoreBndr, CoreExpr) -> StateT Subst CoreM (CoreBndr, CoreExpr)
        freshenBndr (bind, expr) = do
            bindUniq <- getUniqueM
            let bind' = setVarUnique bindUniq bind
            modify $ \subst -> extendIdSubst subst bind (Var bind')
            return (bind', expr)
    
        freshenExpr :: (CoreBndr, CoreExpr) -> (CoreBndr, CoreExpr)
        freshenExpr = fmap (substExpr "collectRecBinders:LetRec" subst)

    (freshBndrBs, subst) <- runStateT (traverse freshenBndr bs) emptySubst
    let fullyFreshBs = fmap freshenExpr freshBndrBs
        fullyFreshBody = freshenExpr body
-}            
    anf_binders <- mapM collectFromRecBinders bs
    anf_body_binders <- collectRecBindersK body $ \anf_body ->
        returnDL (b, anf_body)    
    return $ DL.concat (anf_body_binders:anf_binders)
collectRecBinders (b, Case scrut var ty alts) = do
    anf_alts <- mapM anfAlt alts
    collectRecBindersK scrut $ \anf_scrut ->
        returnDL $ (b, Case anf_scrut var ty anf_alts)
collectRecBinders (b, Cast ex co) =
    collectRecBindersK ex $ \anf_ex ->
        returnDL (b, Cast anf_ex co)
collectRecBinders (b, Tick tick ex) =
    collectRecBindersK ex $ \anf_ex ->
        returnDL (b, Tick tick anf_ex)
collectRecBinders (b, ex) =
    returnDL (b, ex)

collectRecBindersK :: CoreExpr -> (CoreExpr -> CoreM DLBndrs) -> CoreM DLBndrs
collectRecBindersK (App lhs rhs) acc = do
    lhsBndr <- generateName (exprType lhs)
    rhsBndr <- generateName (exprType rhs) 
    collectRecBindersK lhs $ \v0 ->
        collectRecBindersK rhs $ \v1 -> do
                let myNames = DL.fromList [ (lhsBndr, v0), (rhsBndr, v1) ]
                appNames <- acc (App (Var lhsBndr) (Var rhsBndr))
                return $ DL.append myNames appNames
collectRecBindersK (Let (NonRec bind rhs) body) acc =
    collectRecBindersK rhs $ \anf_rhs ->
        collectRecBindersK body $ \anf_body -> do
            bodyNames <- acc anf_body
            return (DL.cons (bind, anf_rhs) bodyNames)
collectRecBindersK (Let (Rec bs) body) acc =
    collectRecBindersK body $ \anf_body -> do
        bodyNames <- acc anf_body
        bsNames <- DL.concat <$> mapM collectFromRecBinders bs
        return (DL.append bodyNames bsNames)
collectRecBindersK (Case scrut var ty alts) acc =
    collectRecBindersK scrut $ \anf_scrut -> do
        anfAlts <- mapM anfAlt alts
        acc (Case anf_scrut var ty anfAlts)
collectRecBindersK (Cast ex co) acc = do
    collectRecBindersK ex $ \anf_ex ->
        acc (Cast anf_ex co)
-- Should acc be on the inside or the outside of the tick?
collectRecBindersK (Tick tick ex) acc = do
    collectRecBindersK ex $ \anf_ex ->
        acc (Tick tick anf_ex)
collectRecBindersK ex acc = acc =<< anfExpression ex
