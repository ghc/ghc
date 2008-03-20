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
 
    prepVdefg (env@(venv,_)) (Nonrec(Vdef((Nothing,x),t,e))) = 
	(eextend venv (x,t), Nonrec(Vdef((Nothing,x),t,prepExp env e)))
    prepVdefg (env@(venv,_))  (Nonrec(Vdef(qx,t,e))) = 
	(venv, Nonrec(Vdef(qx,t,prepExp env e)))
    prepVdefg (venv,tvenv) (Rec vdefs) = 
	(venv',Rec [Vdef(qx,t,prepExp (venv',tvenv) e) | Vdef(qx,t,e) <- vdefs])
	where venv' = foldl eextend venv [(x,t) | Vdef((Nothing,x),t,_) <- vdefs]

    prepExp env (Var qv) = Var qv
    prepExp env (Dcon qdc) = Dcon qdc
    prepExp env (Lit l) = Lit l
    prepExp env e@(App _ _) = unwindApp env e []
    prepExp env e@(Appt _ _) = unwindApp env e []
    prepExp (venv,tvenv) (Lam (Vb vb) e) = Lam (Vb vb) (prepExp (eextend venv vb,tvenv) e)
    prepExp (venv,tvenv) (Lam (Tb tb) e) = Lam (Tb tb) (prepExp (venv,eextend tvenv tb) e)
    prepExp env@(venv,tvenv) (Let (Nonrec(Vdef((Nothing,x),t,b))) e) 
        | kindof tvenv t == Kunlifted && suspends b =
            -- There are two places where we call the typechecker, one of them
            -- here.
            -- We need to know the type of the let body in order to construct
            -- a case expression. 
            let eTy = typeOfExp env e in
		Case (prepExp env b) (x,t) 
                  eTy
                  [Adefault (prepExp (eextend venv (x,t),tvenv) e)]
    prepExp (venv,tvenv) (Let vdefg e) =  Let vdefg' (prepExp (venv',tvenv) e)
		where (venv',vdefg') = prepVdefg (venv,tvenv) vdefg
    prepExp env@(venv,tvenv) (Case e vb t alts) = Case (prepExp env e) vb t (map (prepAlt (eextend venv vb,tvenv)) alts)
    prepExp env (Cast e t) = Cast (prepExp env e) t
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
    unwindApp env (op@(Var(qv@(_,p)))) as | isPrimVar qv =
	etaExpand (drop n atys) (rewindApp env op as)
        where Just atys = elookup primArgTys p
              n = length [e | Left e <- as]
    unwindApp env op as = rewindApp env op as


    etaExpand ts e = foldl g e [('$':(show i),t) | (i,t) <- zip [1..] ts]
	  where g e (v,t) = Lam (Vb(v,t)) (App e (Var (unqual v)))

    rewindApp env e [] = e
    rewindApp env@(venv,tvenv) e1 (Left e2:as) | kindof tvenv t == Kunlifted && suspends e2 =
       -- This is the other place where we call the typechecker.
	Case (prepExp env' e2) (v,t) (typeOfExp env rhs) [Adefault rhs]
        where rhs = (rewindApp env' (App e1 (Var (unqual v))) as)
              v = freshVar venv
              t = typeOfExp env e2
              env' = (eextend venv (v,t),tvenv)
    rewindApp env e1 (Left e2:as) = rewindApp env (App e1 (prepExp env e2)) as
    rewindApp env e (Right t:as) = rewindApp env (Appt e t) as

    freshVar venv = maximum ("":edomain venv) ++ "x" -- one simple way!

    typeOfExp :: (Venv, Tvenv) -> Exp -> Ty
    typeOfExp = uncurry (checkExpr mn globalEnv tdefs)

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

