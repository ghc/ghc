module Language.Core.CoreUtils where

import Language.Core.Core
import Language.Core.Utils
import Language.Core.Printer()

--import Debug.Trace

import Data.Generics
import Data.List
import Data.Maybe

splitDataConApp_maybe :: Exp -> Maybe (Qual Dcon, [Ty], [Exp])
splitDataConApp_maybe (Dcon d) = Just (d, [], [])
splitDataConApp_maybe (Appt rator t) = 
   case splitDataConApp_maybe rator of
     Just (r, ts, rs) -> Just (r, ts ++ [t], rs)
     Nothing          -> Nothing
splitDataConApp_maybe (App rator rand) =
  case splitDataConApp_maybe rator of
    Just (r, ts, rs) -> Just (r, ts, rs++[rand])
    Nothing -> Nothing
splitDataConApp_maybe _ = Nothing

splitApp :: Exp -> (Exp, [Exp])
splitApp (Appt rator _) = splitApp rator
splitApp (App rator rand) =
  case splitApp rator of
    (r, rs) -> (r, rs++[rand])
splitApp e = (e, [])

splitAppIgnoreCasts :: Exp -> (Exp, [Exp])
splitAppIgnoreCasts (Appt rator _) = splitApp rator
splitAppIgnoreCasts (App (Cast rator _) rand) = splitApp (App rator rand)
splitAppIgnoreCasts (App rator rand) =
  case splitApp rator of
    (r, rs) -> (r, rs++[rand])
splitAppIgnoreCasts e = (e, [])

splitFunTy_maybe :: Ty -> Maybe ([Ty], Ty)
splitFunTy_maybe (Tforall _ t) = splitFunTy_maybe t
splitFunTy_maybe t = 
  case splitFunTy2_maybe t of
    Just (rator, rand) -> case splitFunTy_maybe rand of
                            Just (r,s) -> Just (rator:r, s)
                            Nothing -> Just ([rator], rand)
    Nothing -> Nothing

splitFunTy2_maybe :: Ty -> Maybe (Ty,Ty)
splitFunTy2_maybe (Tapp (Tapp (Tcon c) t) u) | c == tcArrow = Just (t, u)
splitFunTy2_maybe _ = Nothing

vdefNamesQ :: [Vdef] -> [Qual Var]
vdefNamesQ = map (\ (Vdef (v,_,_)) -> v)

vdefNames :: [Vdef] -> [Var]
vdefNames = snd . unzip . vdefNamesQ

vdefTys :: [Vdef] -> [Ty]
vdefTys = map (\ (Vdef (_,t,_)) -> t)

vdefgNames :: Vdefg -> [Var]
vdefgNames = snd . unzip . vdefgNamesQ

vdefgNamesQ :: Vdefg -> [Qual Var]
vdefgNamesQ (Rec vds) = map (\ (Vdef (v,_,_)) -> v) vds
vdefgNamesQ (Nonrec (Vdef (v,_,_))) = [v]

vdefgTys :: Vdefg -> [Ty]
vdefgTys (Rec vds) = map (\ (Vdef (_,t,_)) -> t) vds
vdefgTys (Nonrec (Vdef (_,t,_))) = [t]
vdefgBodies :: Vdefg -> [Exp]
vdefgBodies (Rec vds) = map (\ (Vdef (_,_,e)) -> e) vds
vdefgBodies (Nonrec (Vdef (_,_,e))) = [e]

vbNames :: [Vbind] -> [Var]
vbNames = fst . unzip

-- assumes v is not bound in e
substIn :: Data a => Var -> Var -> a -> a
substIn v newV = everywhereExcept (mkT frob)
  where frob (Var (Nothing,v1)) | v == v1   = Var (Nothing,newV)
        frob e                              = e

substVars :: Data a => [Var] -> [Var] -> a -> a
substVars oldVars newVars e = foldl' (\ e1 (old,new) -> substIn old new e1) 
  e (zip oldVars newVars)


tdefNames :: [Tdef] -> [Qual Var]
tdefNames = concatMap doOne
  where doOne (Data qtc _ cds) = qtc:(concatMap doCdef cds)
        doOne (Newtype qtc qtc1 _ _) = [qtc, qtc1]
        doCdef (Constr qdc _ _) = [qdc]

tdefDcons :: [Tdef] -> [Qual Var]
tdefDcons = concatMap doOne
  where doOne (Data _ _ cds) = concatMap doCdef cds
        doOne _ = []
        doCdef (Constr qdc _ _) = [qdc]

tdefTcons :: [Tdef] -> [Qual Var]
tdefTcons = concatMap doOne
  where doOne (Data qtc _ _) = [qtc]
        doOne (Newtype qtc qtc1 _ _) = [qtc, qtc1]

filterVdefgs :: (Vdef -> Bool) -> [Vdefg] -> [Vdefg]
filterVdefgs ok = catMaybes . (map dropNames)
  where dropNames (Nonrec v) | not (ok v) = Nothing
        dropNames v@(Nonrec _) = Just v
        dropNames (Rec bs) = case filter ok bs of
           [] -> Nothing
           newBs -> Just (Rec newBs)

applyNewtype :: CoercionKind -> [Ty] -> (Ty,Ty)
applyNewtype _d@(DefinedCoercion tbs (from,to)) tys = 
  let (tvs,_) = unzip tbs in
    let res = (substl tvs tys from,substl tvs tys to) in
      -- trace ("co = " ++ show d ++ " args  = " ++ show tys ++ " res = " ++ show res) $
        res

{- Simultaneous substitution on types for type variables,
   renaming as neceessary to avoid capture.
   No checks for correct kindedness. -}
substl :: [Tvar] -> [Ty] -> Ty -> Ty
substl tvs ts t = f (zip tvs ts) t
  where 
    f env t0 =
     case t0 of
       Tcon _ -> t0
       Tvar v -> case lookup v env of
                   Just t1 -> t1
                   Nothing -> t0
       Tapp t1 t2 -> Tapp (f env t1) (f env t2)
       Tforall (tv,k) t1 -> 
         if tv `elem` free then
           Tforall (t',k) (f ((tv,Tvar t'):env) t1)
         else 
	   Tforall (tv,k) (f (filter ((/=tv).fst) env) t1)
       TransCoercion t1 t2 -> TransCoercion (f env t1) (f env t2)
       SymCoercion t1 -> SymCoercion (f env t1)
       UnsafeCoercion t1 t2 -> UnsafeCoercion (f env t1) (f env t2)
       LeftCoercion t1 -> LeftCoercion (f env t1)
       RightCoercion t1 -> RightCoercion (f env t1)
       InstCoercion t1 t2 -> InstCoercion (f env t1) (f env t2)
     where free = foldr union [] (map (freeTvars.snd) env)
           t' = freshTvar free 

   
{- Return free tvars in a type -}
freeTvars :: Ty -> [Tvar]
freeTvars (Tcon _) = []
freeTvars (Tvar v) = [v]
freeTvars (Tapp t1 t2) = freeTvars t1 `union` freeTvars t2
freeTvars (Tforall (t,_) t1) = delete t (freeTvars t1) 
freeTvars (TransCoercion t1 t2) = freeTvars t1 `union` freeTvars t2
freeTvars (SymCoercion t) = freeTvars t
freeTvars (UnsafeCoercion t1 t2) = freeTvars t1 `union` freeTvars t2
freeTvars (LeftCoercion t) = freeTvars t
freeTvars (RightCoercion t) = freeTvars t
freeTvars (InstCoercion t1 t2) = freeTvars t1 `union` freeTvars t2

{- Return any tvar *not* in the argument list. -}
freshTvar :: [Tvar] -> Tvar
freshTvar tvs = maximum ("":tvs) ++ "x" -- one simple way!

splitLambda :: Exp -> ([Bind],Exp)
splitLambda (Lam vb e) = case splitLambda e of
  (vbs,rhs) -> (vb:vbs,rhs)
splitLambda (Note _ e) = splitLambda e
splitLambda e          = ([],e)

vbinds :: [Bind] -> [(Var,Ty)]
vbinds = foldl' stuff []
  where stuff :: [(Var,Ty)] -> Bind -> [(Var,Ty)]
        stuff rest (Tb _) = rest
        stuff rest (Vb p) = p:rest

splitBinds :: [Bind] -> ([(Tvar,Kind)],[(Var,Ty)])
splitBinds = foldr stuff ([],[])
  where stuff (Tb t) (tbs,vbs) = (t:tbs,vbs)
        stuff (Vb v) (tbs,vbs) = (tbs,v:vbs)

freeVars :: Exp -> [Qual Var]
freeVars (Var v)                    = [v]
freeVars (Dcon _)                   = []
freeVars (Lit _)                    = []
freeVars (App f g)                  = freeVars f `union` freeVars g
freeVars (Appt e _)                 = freeVars e
freeVars (Lam (Tb _) e)             = freeVars e
freeVars (Lam (Vb (v,_)) e)         = delete (unqual v) (freeVars e)
freeVars (Let (Nonrec (Vdef (v,_,rhs))) e) = freeVars rhs `union` (delete v (freeVars e))
freeVars (Let r@(Rec _) e)         = (freeVars e \\ boundVars) `union` (freeVarss rhss \\ boundVars)
  where boundVars = map unqual $ vdefgNames r
        rhss      = vdefgBodies r
freeVars (Case e (v,_) _ alts)      = freeVars e `union` (delete v1 (boundVarsAlts alts))
  where v1 = unqual v
        boundVarsAlts as = freeVarss rhss \\ (v1:caseVars)
          where rhss = map (\ a -> case a of
                             Acon _ _ _ r -> r
                             Alit _ r     -> r
                             Adefault r   -> r) as
                caseVars = foldl' union [] (map (\ a -> case a of
                                               Acon _ _ vbs _ ->
                                                 (map unqual (fst (unzip vbs)))
                                               _              -> []) as)
freeVars (Cast e _)                 = freeVars e
freeVars (Note _ e)                 = freeVars e
freeVars (External {})              = []

freeVarss :: [Exp] -> [Qual Var]
freeVarss = foldl' union [] . map freeVars