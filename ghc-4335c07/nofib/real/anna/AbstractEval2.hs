 
-- ==========================================================--
-- === Reduction of abstract expressions                  ===--
-- ===                                   AbstractEval2.hs ===--
-- ==========================================================--

module AbstractEval2 where
import BaseDefs
import Utils
import MyUtils
import AbstractVals2
import Apply

-- ==========================================================--
--
aeEval :: HExpr Naam -> HExpr Naam

aeEval   (HVar _)   = panic "aeEval(1)"
aeEval   (HLam _ _) = panic "aeEval(2)"
aeEval   (HTable _) = panic "aeEval(3)"

aeEval h@(HPoint _) = h

aeEval   (HMeet es) = HPoint (foldr1 (\/) (map aeEvalConst es))

aeEval   (HApp (HTable t) e2)
   = aeEval (utSureLookup t "aeEval(5)" (aeEvalConst e2))

aeEval   (HVAp (HPoint f) es)
   = HPoint (apApply f (map aeEvalConst es))

aeEval   (HApp f@(HApp _ _) someArg)
   = aeEval (HApp (aeEval f) someArg)

aeEval   (HApp f@(HPoint _) e)
   = aeEval (HVAp f [e])

aeEval x = panic "aeEval(4)"


-- ==========================================================--
--
aeEvalConst :: HExpr Naam -> Route

aeEvalConst e 
   = case aeEval e of {HPoint p -> p; _ -> panic "aeEvalConst"}


-- ==========================================================--
--
aeEvalExact :: HExpr Naam -> [HExpr Naam] -> Route

aeEvalExact (HLam vs e) args
   = case aeEval (aeSubst (myZip2 vs args) e) of
       {HPoint p -> p; _ -> panic "aeEvalExact"}


-- ==========================================================--
--
aeSubst :: AList Naam (HExpr Naam) -> HExpr Naam -> HExpr Naam

aeSubst rho (HVar v)      = utSureLookup rho "aeSubst" v
aeSubst rho h@(HPoint p)  = h
aeSubst rho (HLam _ _)    = panic "aeSubst(1)"
aeSubst rho (HMeet es)    = HMeet (map (aeSubst rho) es)
aeSubst rho (HTable t)    = HTable (map2nd (aeSubst rho) t)
aeSubst rho (HApp e1 e2)  = HApp (aeSubst rho e1) (aeSubst rho e2)
aeSubst rho (HVAp f es)   = HVAp (aeSubst rho f) (map (aeSubst rho) es)


-- ==========================================================--
--
aeMkMeet :: HExpr Naam -> [HExpr Naam] -> HExpr Naam

aeMkMeet bottom []    = bottom
aeMkMeet bottom [x]   = x
aeMkMeet bottom xs    = HMeet xs


-- ==========================================================--
-- === end                               AbstractEval2.hs ===--
-- ==========================================================--
