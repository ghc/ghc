%
% (c) Joachim Breitner 2013
%
\section{Common subexpression}

\begin{code}
module CommonContext (ccProgram) where

#include "HsVersions.h"

import Var
import VarSet
import Id
import CoreUtils
import Type
import CoreSyn
import Outputable
import FastString
import VarEnv
import CoreFVs
import MkCore
import TysWiredIn ( unitTy )
\end{code}


\begin{code}
ccProgram :: CoreProgram -> CoreProgram
ccProgram binds = map topLevelBind binds

topLevelBind :: CoreBind -> CoreBind
topLevelBind (NonRec b e) = NonRec b (findInterestingLet e)
topLevelBind (Rec pairs) = Rec (map (\(b,e) -> (b, findInterestingLet e)) pairs)

findInterestingLet :: CoreExpr -> CoreExpr
findInterestingLet (Type t) = Type t
findInterestingLet (Coercion c) = Coercion c
findInterestingLet (Lit lit) = Lit lit
findInterestingLet (Var v) = Var v
findInterestingLet (App f a) = App (findInterestingLet f) (findInterestingLet a)
findInterestingLet (Lam v e) = Lam v (findInterestingLet e)
findInterestingLet (Tick t e) = Tick t (findInterestingLet e)
findInterestingLet (Cast e co) = Cast (findInterestingLet e) co
findInterestingLet (Case scrut bndr ty alts) =
    Case (findInterestingLet scrut) bndr ty $
    map (\(c,v,e) -> (c,v, findInterestingLet e)) alts
findInterestingLet (Let (NonRec v e) body) =
    let (v',e',body') = process v e body
    in Let (NonRec v' (findInterestingLet e')) (findInterestingLet body')
findInterestingLet (Let (Rec pairs) body) =
    Let (Rec (map (\(b,e) -> (b, findInterestingLet e)) pairs)) (findInterestingLet body)

process :: Var -> CoreExpr -> CoreExpr -> (Var, CoreExpr, CoreExpr)
process v e body
  | idArity v <= 0 = (v, e, body)
  | otherwise
  = case contextOf v body of
        OneUse cts | not (null cts) ->
            let (bndrs, fun_body) = collectNBinders (idArity v) e
                fun_body' = addContex cts fun_body
                e' = mkLams bndrs fun_body'
                v' = setIdType v (exprType e')
                body' = replaceContext v v' cts body
            in -- pprTrace "findInterestingLet" (vcat [ppr v, ppr (idArity v), pprConts cts, ppr body])
               (v', mkLams bndrs fun_body', body')
        _ -> (v, e, body)

data Cont =
    Select Id Type [CoreAlt] | -- case [] of bndy { alts.. }
    AppTo CoreExpr |
    PassTo CoreExpr

data ContRes
    = NoUse
    | NeedsArgs Int
    | Building [Cont] -- We are still building the context
    | OneUse [Cont]
    | MultiUse

contextOf :: Id -> CoreExpr -> ContRes
contextOf _ (Type _) = NoUse
contextOf _ (Coercion _) = NoUse
contextOf _ (Lit _) = NoUse
contextOf v (Var v')
    | v == v'
    = NeedsArgs (idArity v)
    | otherwise
    = NoUse
--contextOf v (App f (Type _)) = finish $ contextOf v f
contextOf v (App f a) =
    case (contextOf v f, contextOf v a) of
        (NoUse, NoUse) -> NoUse
        (NoUse, NeedsArgs _) -> MultiUse -- Broken?
        (NoUse, Building cts) -> Building (PassTo f : cts)
        (NoUse, OneUse cts) -> OneUse cts
        (NoUse, MultiUse) -> MultiUse
        (NeedsArgs 1, NoUse) -> Building []
        (NeedsArgs i, NoUse) -> NeedsArgs (i-1)
        (NeedsArgs _, _) -> MultiUse
        (Building cts, NoUse) -> Building (AppTo a : cts)
        (Building _, _) -> MultiUse
        (OneUse cts, NoUse) -> OneUse cts
        (OneUse _, _) -> MultiUse
        (MultiUse, _) -> MultiUse
contextOf v (Case e wild ty alts) =
    case (contextOf v e, contextOfAlts v alts) of
        (NoUse, r) -> removeBoundVars [wild] $ finish r
        (NeedsArgs _, _) -> MultiUse -- Broken?
        (Building cts, NoUse) -> Building (Select wild ty alts : cts)
        (Building _, _) -> MultiUse
        (OneUse cts, NoUse) -> OneUse cts
        (OneUse _, _) -> MultiUse
        (MultiUse, _) -> MultiUse
contextOf v (Cast e co) = finish $ contextOf v e
contextOf v (Lam x e) = removeBoundVars [x] $ finish $ contextOf x e
contextOf v (Let binds body)
    | v `elemVarSet` bindFreeVars binds
    = MultiUse
    | otherwise
    = removeBoundVars (bindersOf binds) $ finish (contextOf v body)


contextOf v e = pprPanic "contextOf" (ppr v <+> ppr e)

contextOfAlts :: Id -> [CoreAlt] -> ContRes
contextOfAlts v = foldr combine NoUse . map (\(_,vs,e) -> removeBoundVars vs $ finish (contextOf v e))

finish :: ContRes -> ContRes
finish (NeedsArgs _) = MultiUse
finish (Building ctx) = OneUse ctx
finish r              = r

removeBoundVars :: [Var] -> ContRes -> ContRes
removeBoundVars _ MultiUse = MultiUse
removeBoundVars _ NoUse = NoUse
removeBoundVars vs (OneUse cts) = OneUse (reverse (takeWhile ok (reverse cts)))
  where ok c = disjointVarSet set (contFreeVars c)
        set = mkVarSet vs

combine :: ContRes -> ContRes -> ContRes
combine MultiUse _      = MultiUse
combine _ MultiUse      = MultiUse
combine NoUse r         = r
combine r NoUse         = r
combine (OneUse ctx) (OneUse ctx2) = OneUse (combineCtx ctx ctx2)
combine _ _             = pprPanic "combine" empty -- should not happen, due to finish

combineCtx :: [Cont] -> [Cont] -> [Cont]
combineCtx ctx1 ctx2 = reverse (go (reverse ctx1) (reverse ctx2))
  where
  go [] _ = []
  go _ [] = []
  go (c1 : cts1) (c2 : cts2)
    | c1 `eqCont` c2 = c1 : go cts1 cts2
    | otherwise      = []

-- The length of cts is sufficient, but we can do sanity checking with it
replaceContext :: Id -> Id -> [Cont] -> CoreExpr -> CoreExpr
replaceContext v v_new cts e = done (go e)
  where
  go (Type t) = Right (Type t, [])
  go (Coercion c) = Right (Coercion c, [])
  go (Lit lit) = Right (Lit lit, [])
  go (Var v') | v == v' = Left (Var v_new, idArity v)
              | otherwise = Right (Var v', [])

  go (App f e) = case go f of
    Left (f', 1) -> Right (App f' e, reverse cts)
    Left (f', n) -> Left (App f' e, n-1)
    Right (f', []) -> case go e of
        Right (e', []) -> Right (App f' e', [])
        Right (e', c : cts)
            | c `eqCont` PassTo f
            -> Right (e', cts)
    Right (f', c : cts)
        | c `eqCont` AppTo e
        -> Right (f', cts)

  go (Case e wild ty alts) = case go e of
    Right (e', c : cts)
        | c `eqCont` Select wild ty alts
        -> Right (e', cts)
    Right (e', []) -> Right (Case e' wild ty (map (\(c,vs,e) -> (c, vs, done (go e))) alts), [])
  go (Let binds body) = case go body of
    Right (body', []) -> Right (Let binds body', [])

  go (Cast e co) = case go e of
    Right (e', []) -> Right (Cast e' co, [])

  go (Lam x e) = case go e of
    Right (e', []) -> Right (Lam x e', [])

  go (Tick t e) = case go e of
    Right (e', []) -> Right (Tick t e', [])

  done (Right (e, [])) = e


addContex :: [Cont] -> CoreExpr -> CoreExpr
addContex [] e = e
addContex (c : cts) e = addCont c (addContex cts e)

addCont :: Cont -> Expr Id -> Expr Id
addCont (Select wild ty alts) e = Case e wild ty alts
addCont (PassTo f) e = App f e
addCont (AppTo e) f = App f e

eqCont :: Cont -> Cont -> Bool
c1 `eqCont` c2 = addCont c1 (Var hole) `eqExprSimple` addCont c2 (Var hole)
 where hole = mkWildValBinder unitTy -- Type is wrong. shrug.

contFreeVars :: Cont -> VarSet
contFreeVars c = exprFreeVars (addCont c (Var hole)) `delVarSet` hole
 where hole = mkWildValBinder unitTy -- Type is wrong. shrug.


collectNBinders :: OutputableBndr b => Int -> Expr b -> ([b], Expr b)
collectNBinders n e = go n [] e
  where
    go 0 bs e = (reverse bs, e)
    go n bs (Lam b e) = go (n-1) (b:bs) e
    go n bs e = pprPanic "collectNBinders" (ppr n <+> ppr e)

instance Outputable ContRes where
    ppr NoUse         = ptext (sLit "NoUse")
    ppr (NeedsArgs n) = ptext (sLit "NeedsArgs") <+> int n
    ppr (Building cts)  = ptext (sLit "Building") <+> pprConts cts
    ppr (OneUse cts)  = ptext (sLit "OneUse") <+> pprConts cts
    ppr MultiUse      = ptext (sLit "MultiUse")

pprConts :: [Cont] -> SDoc
pprConts cts = ppr (addContex cts (Var (mkWildValBinder unitTy)))

eqExprSimple :: CoreExpr -> CoreExpr -> Bool
eqExprSimple e1 e2 = eqExpr in_scope  e1 e2
   where in_scope = mkInScopeSet (exprsFreeVars [e1,e2])
\end{code}
