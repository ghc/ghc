{- 
Preprocess a module to normalize it in the following ways:
	(1) Saturate all constructor and primop applications. 
	(2) Arrange that any non-trivial expression of unlifted kind ('#')
             is turned into the scrutinee of a Case.
After these preprocessing steps, Core can be interpreted (or given an operational semantics)
      ignoring type information almost completely.
-}


module Prep where

import Prims
import Core
import Printer
import Env
import Check

primArgTys :: Env Var [Ty]
primArgTys = efromlist (map f Prims.primVals)
  where f (v,t) = (v,atys)
             where (_,atys,_) = splitTy t

prepModule :: Menv -> Module -> Module
prepModule globalEnv (Module mn tdefs vdefgs) = 
    Module mn tdefs vdefgs' 
  where
    (_,vdefgs') = foldl prepTopVdefg (eempty,[]) vdefgs

    prepTopVdefg (venv,vdefgs) vdefg = (venv',vdefgs ++ [vdefg'])
       where (venv',vdefg') = prepVdefg (venv,eempty) vdefg
 
    prepVdefg (env@(venv,_)) (Nonrec(Vdef(("",x),t,e))) = 
	(eextend venv (x,t), Nonrec(Vdef(("",x),t,prepExp env e)))
    prepVdefg (env@(venv,_))  (Nonrec(Vdef(qx,t,e))) = 
	(venv, Nonrec(Vdef(qx,t,prepExp env e)))
    prepVdefg (venv,tvenv) (Rec vdefs) = 
	(venv',Rec [Vdef(qx,t,prepExp (venv',tvenv) e) | Vdef(qx,t,e) <- vdefs])
	where venv' = foldl eextend venv [(x,t) | Vdef(("",x),t,_) <- vdefs]

    prepExp env (Var qv) = Var qv
    prepExp env (Dcon qdc) = Dcon qdc
    prepExp env (Lit l) = Lit l
    prepExp env e@(App _ _) = unwindApp env e []
    prepExp env e@(Appt _ _) = unwindApp env e []
    prepExp (venv,tvenv) (Lam (Vb vb) e) = Lam (Vb vb) (prepExp (eextend venv vb,tvenv) e)
    prepExp (venv,tvenv) (Lam (Tb tb) e) = Lam (Tb tb) (prepExp (venv,eextend tvenv tb) e)
    prepExp env@(venv,tvenv) (Let (Nonrec(Vdef(("",x),t,b))) e) | kindof tvenv t == Kunlifted && suspends b =
		Case (prepExp env b) (x,t) [Adefault (prepExp (eextend venv (x,t),tvenv) e)]
    prepExp (venv,tvenv) (Let vdefg e) =  Let vdefg' (prepExp (venv',tvenv) e)
		where (venv',vdefg') = prepVdefg (venv,tvenv) vdefg
    prepExp env@(venv,tvenv) (Case e vb alts) = Case (prepExp env e) vb (map (prepAlt (eextend venv vb,tvenv)) alts)
    prepExp env (Coerce t e) = Coerce t (prepExp env e)
    prepExp env (Note s e) = Note s (prepExp env e)
    prepExp env (External s t) = External s t

    prepAlt (venv,tvenv) (Acon qdc tbs vbs e) = Acon qdc tbs vbs (prepExp (foldl eextend venv vbs,foldl eextend tvenv tbs) e)
    prepAlt env (Alit l e) = Alit l (prepExp env e)
    prepAlt env (Adefault e) = Adefault (prepExp env e)


    unwindApp env (App e1 e2) as = unwindApp env e1 (Left e2:as)
    unwindApp env (Appt e t) as  = unwindApp env e (Right t:as)
    unwindApp env (op@(Dcon qdc)) as =
        etaExpand (drop n atys) (rewindApp env op as)
        where (tbs,atys0,_) = splitTy (qlookup cenv_ eempty qdc)
	      atys = map (substl (map fst tbs) ts) atys0
	      ts = [t | Right t <- as]
              n = length [e | Left e <- as]
    unwindApp env (op@(Var(m,p))) as | m == primMname =
	etaExpand (drop n atys) (rewindApp env op as)
        where Just atys = elookup primArgTys p
              n = length [e | Left e <- as]
    unwindApp env op as = rewindApp env op as


    etaExpand ts e = foldl g e [('$':(show i),t) | (i,t) <- zip [1..] ts]
	  where g e (v,t) = Lam (Vb(v,t)) (App e (Var ("",v)))

    rewindApp env e [] = e
    rewindApp env@(venv,tvenv) e1 (Left e2:as) | kindof tvenv t == Kunlifted && suspends e2 =
	Case (prepExp env' e2) (v,t)
		[Adefault (rewindApp env' (App e1 (Var ("",v))) as)]
        where v = freshVar venv
              t = typeofExp env e2
              env' = (eextend venv (v,t),tvenv)
    rewindApp env e1 (Left e2:as) = rewindApp env (App e1 (prepExp env e2)) as
    rewindApp env e (Right t:as) = rewindApp env (Appt e t) as

    freshVar venv = maximum ("":edomain venv) ++ "x" -- one simple way!

    typeofExp :: (Venv,Tvenv) -> Exp -> Ty
    typeofExp (venv,_) (Var qv) = qlookup venv_ venv qv
    typeofExp env (Dcon qdc) = qlookup cenv_ eempty qdc
    typeofExp env (Lit l) = typeofLit l
	where typeofLit (Lint _ t) = t
	      typeofLit (Lrational _ t) = t
	      typeofLit (Lchar _ t) = t
              typeofLit (Lstring _ t) = t
    typeofExp env (App e1 e2) = t
          where (Tapp(Tapp _ t0) t) = typeofExp env e1
    typeofExp env (Appt e t) = substl [tv] [t] t'
	  where (Tforall (tv,_) t') = typeofExp env e
    typeofExp (venv,tvenv) (Lam (Vb(v,t)) e) = tArrow t (typeofExp (eextend venv (v,t),tvenv) e)
    typeofExp (venv,tvenv) (Lam (Tb tb) e) = Tforall tb (typeofExp (venv,eextend tvenv tb) e)
    typeofExp (venv,tvenv) (Let vdefg e) = typeofExp (venv',tvenv) e
	  where venv' = case vdefg of
			  Nonrec (Vdef((_,x),t,_)) -> eextend venv (x,t)
                          Rec vdefs -> foldl eextend venv [(x,t) | Vdef((_,x),t,_) <- vdefs]
    typeofExp (venv,tvenv) (Case _ vb (alt:_)) = typeofAlt (eextend venv vb,tvenv) alt
	where typeofAlt (venv,tvenv) (Acon _ tbs vbs e) = typeofExp (foldl eextend venv vbs,foldl eextend tvenv tbs) e
	      typeofAlt env (Alit _ e) = typeofExp env e
	      typeofAlt env (Adefault e) = typeofExp env e
    typeofExp env (Coerce t _) = t
    typeofExp env (Note _ e) = typeofExp env e
    typeofExp env (External _ t) = t

    {- Return false for those expressions for which Interp.suspendExp buidds a thunk. -}
    suspends (Var _) = False
    suspends (Lit _) = False
    suspends (Lam (Vb _) _) = False
    suspends (Lam _ e) = suspends e
    suspends (Appt e _) = suspends e
    suspends (Coerce _ e) = suspends e
    suspends (Note _ e) = suspends e
    suspends (External _ _) = False
    suspends _ = True

    kindof :: Tvenv -> Ty -> Kind
    kindof tvenv (Tvar tv) = 
      case elookup tvenv tv of
	Just k -> k
        Nothing -> error ("impossible Tyvar " ++ show tv)
    kindof tvenv (Tcon qtc) = qlookup tcenv_ eempty qtc
    kindof tvenv (Tapp t1 t2) = k2
	where Karrow _ k2 = kindof tvenv t1
    kindof tvenv (Tforall _ t) = kindof tvenv t

    mlookup :: (Envs -> Env a b) -> Env a b -> Mname -> Env a b
    mlookup _ local_env "" = local_env
    mlookup selector _  m =   
      case elookup globalEnv m of
        Just env -> selector env
        Nothing -> error ("undefined module name: " ++ m)

    qlookup ::  (Ord a, Show a) => (Envs -> Env a b) -> Env a b -> (Mname,a) -> b
    qlookup selector local_env (m,k) =   
      case elookup (mlookup selector local_env m) k of
        Just v -> v
        Nothing -> error ("undefined identifier: " ++ show k)

