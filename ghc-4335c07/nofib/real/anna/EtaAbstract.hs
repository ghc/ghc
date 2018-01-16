
-- ==========================================================--
-- === Add parameters to supercombinators which           ===--
-- === otherwise return functions          EtaAbstract.hs ===--
-- ==========================================================--

module EtaAbstract where
import BaseDefs
import Utils
import MyUtils


-- ==========================================================--
-- Doesn't assume that the tree has been lambda-lifted.
-- It does however assume that all lambda-terms are 
-- directly attached to a let-binding.
--
eaEtaAbstract :: AnnExpr Naam TExpr ->
                 AnnExpr Naam TExpr

eaEtaAbstract ae@(tau, AVar v)     = ae
eaEtaAbstract ae@(tau, ANum n)     = ae
eaEtaAbstract ae@(tau, AConstr c)  = ae
eaEtaAbstract ae@(tau, AAp e1 e2) 
   = (tau, AAp (eaEtaAbstract e1) (eaEtaAbstract e2))
eaEtaAbstract ae@(tau, ACase sw alts)
   = (tau, ACase (eaEtaAbstract sw) 
                 [(n, (ps, eaEtaAbstract rhs)) | (n, (ps, rhs)) <- alts])
eaEtaAbstract ae@(tau, ALam vs e)
   = (tau, ALam vs (eaEtaAbstract e))

eaEtaAbstract ae@(tau, ALet rf defs body)
   = let typeInfo = [eaUncurry ty | (n, (ty, rhs)) <- defs]
         mergedDefs = map2nd mergeLams defs
         fixedDefs = myZipWith2 fixOne mergedDefs typeInfo
         fixOne sc@(n, (tau, ALam vs e)) (argTs, resT)
            | length vs == length argTs  = sc
            | length vs >  length argTs  = panic "eaEtaAbstract"
            | length vs <  length argTs  = eaMain sc argTs resT
         fixOne sc@(n, (tau, non_lam_b)) (argTs, resT)
            | null argTs  = sc
            | otherwise   = eaMain (n, (tau, ALam [] (tau, non_lam_b))) argTs resT
         mergeLams ae@(tau, ALam vs (tau2, ALam vs2 e))
            = mergeLams (tau, ALam (vs++vs2) e)
         mergeLams anyThingElse = anyThingElse
     in (tau, ALet rf fixedDefs (eaEtaAbstract body))
        

-- ==========================================================--
--
eaMain :: (Naam, AnnExpr Naam TExpr) ->
          [TExpr] ->
          TExpr ->
          (Naam, AnnExpr Naam TExpr)

eaMain (scname, (tau, ALam vs (tau2, rhs))) argTs resT
   = let actualArity  = length vs
         reqdArity    = length argTs
         newArgsReqd  = reqdArity - actualArity
         newArgs      = eaMakeNewArgs newArgsReqd vs
         newArgsTypes = myZip2 newArgs (drop actualArity argTs)
         appArgTLists = map ((flip drop) argTs) 
                            (actualArity `myIntsFromTo` (reqdArity-1))
         appTypes     = map (eaCurry resT) appArgTLists
         newBody      = eaMakeApChain (myZip2 newArgsTypes appTypes) (tau2, rhs)
     in (scname, (tau, ALam (vs++newArgs) newBody))


-- ==========================================================--
--
eaMakeApChain :: [((Naam, TExpr), TExpr)] ->
                 AnnExpr Naam TExpr ->
                 AnnExpr Naam TExpr

eaMakeApChain [] app = app
eaMakeApChain (((v, vtype), vaptype):rest) app
   = eaMakeApChain rest (vaptype, AAp app (vtype, AVar v))


-- ==========================================================--
--
eaMakeNewArgs :: Int -> [Naam] -> [Naam]

eaMakeNewArgs n vs
   = let leadingvs = filter (not.null) (map (takeWhile (== 'v')) vs)
         root = last (sort ("":leadingvs)) ++ "v"
         newNames = map f (1 `myIntsFromTo` n)
         f n = root ++ show (n :: Int)
     in newNames


-- ==========================================================--
--
eaCurry :: TExpr -> [TExpr] -> TExpr

eaCurry resT []           = resT
eaCurry resT (argT:argTs) = TArr argT (eaCurry resT argTs)


-- ==========================================================--
--
eaUncurry :: TExpr -> ([TExpr], TExpr)

eaUncurry (TVar tv) = ([], TVar tv)

eaUncurry (TArr t1 t2)
   = let (rest, final) = eaUncurry t2
     in (t1:rest, final)

eaUncurry (TCons tcon targs) 
   = ([], TCons tcon targs)


-- ==========================================================--
-- === end                                 EtaAbstract.hs ===--
-- ==========================================================--


