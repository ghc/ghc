
-- ==========================================================--
-- === Turn type expressions into domain expressions.     ===--
-- ===                                     TExpr2DExpr.hs ===--
-- ==========================================================--

module TExpr2DExpr where
import BaseDefs
import Utils
import MyUtils
import DomainExpr
import MakeDomains
import TypeCheck5

import Data.List(nub) -- 1.3

-- ==========================================================--
-- This may need fixing up if we start instantiating domain
-- variables to expressions which contain other domain
-- variables within them.
-- 4 Feb: solved the above problem by replacing the offending
--        domain variables with 2.
-- 5 Feb: fixed to curry domains properly, if necessary.
--
txGetInstantiations :: DExpr ->
                       DExpr ->
                       AList Naam Domain

txGetInstantiations simplest usage
   = consistent [] (gi simplest usage)
     where
        gi (DXVar v)      dexpr           = [(v, dxApplyDSubst_2 dexpr)]
        gi DXTwo          DXTwo           = []
        gi (DXLift1 dxs1) (DXLift1 dxs2)  = concat (myZipWith2 gi dxs1 dxs2)
        gi (DXLift2 dxs1) (DXLift2 dxs2)  = concat (myZipWith2 gi dxs1 dxs2)
        gi (DXFunc dxss1 dxt1) (DXFunc dxss2 dxt2)
          = let basis_arity = length dxss1
                usage_arity = length dxss2
                (new_dxss2, new_dxt2) =
                   if usage_arity > basis_arity
                   then (take basis_arity dxss2, 
                         DXFunc (drop basis_arity dxss2) dxt2)
                   else (dxss2, dxt2)
            in  gi dxt1 new_dxt2 ++ concat (myZipWith2 gi dxss1 new_dxss2)

        consistent acc [] = acc
        consistent acc ((v,dx):rest)
           = case utLookup acc v of
                Nothing -> consistent ((v,dx):acc) rest
                Just dy -> if dx == dy 
                           then consistent acc rest
                           else panic "txGetInstantiations"


-- ==========================================================--
--
tx2dxAnnTree :: TypeDependancy ->
                AnnExpr Naam TExpr ->
                AnnExpr Naam DExpr

tx2dxAnnTree td tree = tcMapAnnExpr (tx2dx td) tree


-- ==========================================================--
--
tx2dx :: TypeDependancy -> TExpr -> DExpr

tx2dx td texpr 
   = let typeVars = sort (nub (tcTvars_in texpr))
         dVarEnv = zip typeVars [[x] | x <- "abcdefghijklmnopqrstuvwxyz"]
     in  if length typeVars > 26 
         then panic "tx2dx" 
         else dxNormaliseDExpr (tx2dx_aux td dVarEnv texpr)

tx2dx_aux td env (TVar v) 
   = DXVar (utSureLookup env "tx2dx_aux(1)" v)
tx2dx_aux td env (TCons "int" []) 
   = DXTwo
tx2dx_aux td env (TCons "char" []) 
   = DXTwo
tx2dx_aux td env (TArr t1 t2) 
   = DXFunc [tx2dx_aux td env t1] (tx2dx_aux td env t2)
tx2dx_aux td env (TCons tname targs) 
   = if mdIsRecursiveType td tname 
     then DXLift2 (map (tx2dx_aux td env) targs)
     else DXLift1 (map (tx2dx_aux td env) targs)

-- ==========================================================--
-- === end                                 TExpr2DExpr.hs ===--
-- ==========================================================--
