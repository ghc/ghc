{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{- 
Preprocess a module to normalize it in the following ways:
	(1) Saturate all constructor and primop applications. 
	(2) Arrange that any non-trivial expression of unlifted kind ('#')
             is turned into the scrutinee of a Case.
After these preprocessing steps, Core can be interpreted (or given an operational semantics)
      ignoring type information almost completely.
-}


module Language.Core.Prep where

import Data.Either
import Data.List

import Language.Core.Prims
import Language.Core.Core
import Language.Core.Env
import Language.Core.Check
import Language.Core.Environments
import Language.Core.Encoding

prepModule :: Menv -> Module -> Module
prepModule globalEnv (Module mn tdefs vdefgs) = 
    Module mn tdefs vdefgs' 
  where

    (tcenv, cenv) = mkTypeEnvsNoChecking tdefs
    (_,vdefgs') = foldl' prepTopVdefg (eempty,[]) vdefgs

    prepTopVdefg (venv,vdefgs) vdefg = (venv',vdefgs ++ [vdefg'])
       where (venv',vdefg') = prepVdefg (venv,eempty) vdefg
 
    prepVdefg (env@(venv,_)) (Nonrec(Vdef((Nothing,x),t,e))) = 
	(eextend venv (x,t), Nonrec(Vdef((Nothing,x),t,prepExp env e)))
    prepVdefg (env@(venv,_))  (Nonrec(Vdef(qx,t,e))) =
     	(venv, Nonrec(Vdef(qx,t,prepExp env e)))
    prepVdefg (venv,tvenv) (Rec vdefs) = 
	(venv',Rec [ Vdef(qx,t,prepExp (venv',tvenv) e) | Vdef(qx,t,e) <- vdefs])
	where venv' = foldl' eextend venv [(x,t) | Vdef((Nothing,x),t,_) <- vdefs]

    prepExp _ (Var qv) = Var qv
    prepExp _ (Dcon qdc) = Dcon qdc
    prepExp _ (Lit l) = Lit l
    prepExp env e@(App _ _) = unwindApp env e []
    prepExp env e@(Appt _ _) = unwindApp env e []
    prepExp (venv,tvenv) (Lam (Vb vb) e) = Lam (Vb vb) (prepExp (eextend venv vb,tvenv) e)
    prepExp (venv,tvenv) (Lam (Tb tb) e) = Lam (Tb tb) (prepExp (venv,eextend tvenv tb) e)
    prepExp env@(venv,tvenv) (Let (Nonrec(Vdef((Nothing,x),t,b))) e) 
        | (kindOfTy tvenv t `eqKind` Kunlifted && suspends b) = 
            -- There are two places where we call the typechecker, one of them
            -- here.
            -- We need to know the type of the let body in order to construct
            -- a case expression. 
                                -- need to extend the env with the let-bound var too!
            let eTy = typeOfExp (eextend venv (x, t)) tvenv e in
               Case (prepExp env b) (x,t) 
                  eTy
                  [Adefault (prepExp (eextend venv (x,t),tvenv) e)] 
    prepExp (venv,tvenv) (Let vdefg e) =  Let vdefg' (prepExp (venv',tvenv) e)
		where (venv',vdefg') = prepVdefg (venv,tvenv) vdefg
    prepExp env@(venv,tvenv) (Case e vb t alts) = Case (prepExp env e) vb t (map (prepAlt (eextend venv vb,tvenv)) alts)
    prepExp env (Cast e t) = Cast (prepExp env e) t
    prepExp env (Note s e) = Note s (prepExp env e)
    prepExp _ (External s t) = External s t

    prepAlt (venv,tvenv) (Acon qdc tbs vbs e) = Acon qdc tbs vbs (prepExp (foldl' eextend venv vbs,foldl' eextend tvenv tbs) e)
    prepAlt env (Alit l e) = Alit l (prepExp env e)
    prepAlt env (Adefault e) = Adefault (prepExp env e)


    unwindApp env (App e1 e2) as = unwindApp env e1 (Left e2:as)
    unwindApp env (Appt e t) as  = unwindApp env e (Right t:as)
    unwindApp env (op@(Dcon qdc)) as = 
        -- possibly dubious to assume no type args
        etaExpand [] (drop n atys) (rewindApp env op as)
        where (tbs,atys0,_) = splitTy (qlookup cenv_ eempty qdc)
	      atys = map (substl (map fst tbs) ts) atys0
	      ts = [t | Right t <- as]
              n = length [e | Left e <- as]
    unwindApp env (op@(Var(qv@(_,p)))) as | isPrimVar qv =
	etaExpand (snd (unzip extraTbs)) (drop n atys) (rewindApp env op as)
        where -- TODO: avoid copying code. these two cases are the same

              -- etaExpand needs to add the type arguments too! Bah!
              (tbs, atys0, _) = (maybe (error "unwindApp") splitTy (elookup (venv_ primEnv) p))
              n_args = length ts
              (appliedTbs, extraTbs) = (take n_args tbs, drop n_args tbs)
              atys = map (substl (map fst appliedTbs) ts) atys0
              ts = [t | Right t <- as]
              n = length [e | Left e <- as]
    unwindApp env op as = rewindApp env op as


    etaExpand :: [Kind] -> [Ty] -> Exp -> Exp
    etaExpand ks ts e = 
         -- what a pain
         let tyArgs = [(zEncodeString $ "$t_"++(show i),k) | (i, k) <- zip [(1::Integer)..] ks]   
             termArgs = [ (zEncodeString $ '$':(show i),t) | (i,t) <- zip [(1::Integer)..] ts] in
          foldr (\ (t1,k1) e -> Lam (Tb (t1,k1)) e)
	   (foldr (\ (v,t) e -> Lam (Vb (v,t)) e)
              (foldl' (\ e (v,_) -> App e (Var (unqual v)))
                 (foldl' (\ e (tv,_) -> Appt e (Tvar tv))
                   e tyArgs)
              termArgs) termArgs)
           tyArgs

    rewindApp _ e [] = e
    rewindApp env@(venv,tvenv) e1 (Left e2:as) | kindOfTy tvenv t `eqKind` Kunlifted && suspends e2 =
       -- This is the other place where we call the typechecker.
        Case newScrut (v,t) (typeOfExp venv' tvenv rhs) [Adefault rhs]
        where newScrut = prepExp env e2
              rhs = (rewindApp (venv', tvenv) (App e1 (Var (unqual v))) as)
                 -- note:
                 -- e1 gets moved inside rhs. so if we pick a case
                 -- var name (outside e1) equal to a name bound *inside*
                 -- e1, the binding *inside* e1 will shadow "v"
                 -- Which would be name capture!
                 -- So, we pass the bound vars of e1 to freshVar along with
                 -- the domain of the current env.
              v = freshVar (edomain venv `union` (boundVars e1))
              t = typeOfExp venv tvenv e2
              venv' = eextend venv (v,t)
    rewindApp env e1 (Left e2:as) = rewindApp env (App e1 (prepExp env e2)) as
    rewindApp env e (Right t:as) = rewindApp env (Appt e t) as

    freshVar vs = maximum ("":vs) ++ "x" -- one simple way!
    
    typeOfExp :: Venv -> Tvenv -> Exp -> Ty
    typeOfExp = checkExpr mn globalEnv tcenv cenv

    kindOfTy :: Tvenv -> Ty -> Kind
    kindOfTy tvenv = checkType mn globalEnv tcenv tvenv

    {- Return false for those expressions for which Interp.suspendExp builds a thunk. -}
    suspends (Var _) = False
    suspends (Lit _) = False
    suspends (Lam (Vb _) _) = False
    suspends (Lam _ e) = suspends e
    suspends (Appt e _) = suspends e
    suspends (Cast e _) = suspends e
    suspends (Note _ e) = suspends e
    suspends (External _ _) = False
    suspends _ = True

    mlookup :: (Envs -> Env a b) -> Env a b -> Mname -> Env a b
    mlookup _ local_env Nothing = local_env
    mlookup selector _  (Just m) =   
      case elookup globalEnv m of
        Just env -> selector env
        Nothing -> error ("Prep: undefined module name: " ++ show m)

    qlookup ::  (Ord a, Show a) => (Envs -> Env a b) -> Env a b -> (Mname,a) -> b
    qlookup selector local_env (m,k) =   
      case elookup (mlookup selector local_env m) k of
        Just v -> v
        Nothing -> error ("undefined identifier: " ++ show k)

boundVars :: Exp -> [Id]
boundVars (Lam (Vb (v,_)) e) = [v] `union` boundVars e
boundVars (Lam _ e) = boundVars e
boundVars (Let vds e) = (boundVarsVdefs vds) `union` boundVars e
boundVars (Case scrut (v,_) _ alts) = 
   [v] `union` (boundVars scrut) `union` boundVarsAlts alts
boundVars (Cast e _) = boundVars e
boundVars (Note _ e) = boundVars e
boundVars (App e1 e2) = boundVars e1 `union` boundVars e2
boundVars (Appt e _) = boundVars e
boundVars _ = []

boundVarsVdefs :: Vdefg -> [Id]
boundVarsVdefs (Rec vds) = nub (concatMap boundVarsVdef vds)
boundVarsVdefs (Nonrec vd) = boundVarsVdef vd

boundVarsVdef :: Vdef -> [Id]
boundVarsVdef (Vdef ((_,v),_,e)) = [v] `union` boundVars e

boundVarsAlts :: [Alt] -> [Var]
boundVarsAlts as = nub (concatMap boundVarsAlt as)

boundVarsAlt :: Alt -> [Var]
boundVarsAlt (Acon _ _ vbs e) = (map fst vbs) `union` (boundVars e)
boundVarsAlt (Alit _ e) = boundVars e
boundVarsAlt (Adefault e) = boundVars e