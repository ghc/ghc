{-# LANGUAGE OverloadedStrings, TupleSections #-}
module ANF ( anfProgram, anfExpression ) where

import GhcPrelude

import CoreMonad
import CoreSyn
import CoreUtils  ( exprType )
import IdInfo     ( IdDetails( VanillaId ), vanillaIdInfo )
import MkCore     ( mkCoreLet )
import Name       ( mkSystemVarName )
import UniqSupply ( getUniqueM )
import Var        ( mkLocalVar )

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

anfExpression :: CoreExpr -> CoreM CoreExpr
anfExpression (App lhs rhs) = 
    anfExpressionApp lhs $ \v0 ->
        anfExpressionApp rhs $ \v1 ->
            return $ App v0 v1
anfExpression (Lam bind bod) = do
    bod' <- anfExpression bod
    return $ Lam bind bod'
anfExpression (Let bind bod) = do
    bind' <- anfBind bind
    bod'  <- anfExpression bod
    return $ Let bind' bod'
anfExpression (Case scrut var ty alts) = do
    scrut' <- anfExpression scrut
    alts' <- mapM anfAlt alts
    return $ Case scrut' var ty alts'
anfExpression (Cast ex co) = do
    ex' <- anfExpression ex
    return $ Cast ex' co
anfExpression (Tick tick ex) = do
    ex' <- anfExpression ex
    return $ Tick tick ex'
anfExpression ex = return ex

letBind :: (CoreExpr -> CoreM CoreExpr) -> CoreExpr -> CoreM CoreExpr
letBind acc rhs = do
    uniq <- getUniqueM
    let name = mkSystemVarName uniq "v"
        var = mkLocalVar VanillaId name (exprType rhs) vanillaIdInfo
        bind = NonRec var rhs
    
    inner <- acc (Var var)
    return $ mkCoreLet bind inner

anfExpressionApp :: CoreExpr -> (CoreExpr -> CoreM CoreExpr) -> CoreM CoreExpr
anfExpressionApp (App lhs rhs) acc =
    anfExpressionApp lhs $ \v0 ->
        anfExpressionApp rhs $ \v1 ->
            letBind acc $ App v0 v1
anfExpressionApp (Lam bind bod) acc = do
    bod' <- anfExpression bod
    letBind acc $ Lam bind bod'
anfExpressionApp (Let bind bod) acc = do
    bind' <- anfBind bind
    bod' <- anfExpression bod
    letBind acc $ Let bind' bod'
anfExpressionApp (Case scrut var ty alts) acc = do
    scrut' <- anfExpression scrut
    alts' <- mapM anfAlt alts
    letBind acc $ Case scrut' var ty alts'
anfExpressionApp (Cast ex co) acc = do
    ex' <- anfExpression ex
    letBind acc $ Cast ex' co
anfExpressionApp (Tick tick ex) acc = do
    ex' <- anfExpression ex
    letBind acc $ Tick tick ex'
anfExpressionApp ex acc = acc ex
